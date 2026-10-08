-- | Bazel's persistent-worker protocol on stdin and stdout, the protocol
-- BuildBuddy speaks to a worker it keeps in a recycled runner
-- (enterprise/server/remote_execution/persistentworker at v2.310.0).
--
-- BuildBuddy starts the worker once per runner, appending
-- @--persistent_worker@ to the action's arguments without its flag files, and
-- then writes one length-delimited @WorkRequest@ per action and reads one
-- @WorkResponse@ back. Requests come one at a time, so this is a singleplex
-- worker: the handler runs on this thread and the next request waits for it.
-- A request carries arguments (its @argfile@ already expanded) and no
-- environment, so the environment the process started with stands for every
-- request, and the working directory is the runner's exec root, the same path
-- for every action the runner serves.
--
-- The worker never exits on its own account. The executor writes the next
-- request without checking, so a worker that retired would fail that request;
-- retirement is the runner pool's (@executor.runner_pool.max_runner_memory_usage_bytes@).
--
-- The messages are decoded by hand, which keeps the worker free of a second
-- generated protocol: WorkRequest fields 1 (arguments, repeated string) and 3
-- (request_id); WorkResponse fields 1 (exit_code), 2 (output) and 3 (request_id),
-- per src/main/protobuf/worker_protocol.proto in Bazel.
module GhcWorker.BazelWorker (
  isBazelWorkerInvocation,
  runBazelWorker,
  WorkRequest (..),
  decodeWorkRequest,
  encodeWorkResponse,
) where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent (newMVar)
import Control.Exception (SomeException, displayException, try)
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word64, Word8)
import GhcWorker.GhcHandler (ghcHandler)
import GhcWorker.Instrumentation (WorkerStatus (..), toGrpcHandler)
import GhcWorker.RequestCwd (newProcessCwdLock)
import Internal.State (newState)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.IO (Handle, BufferMode (..), hFlush, hSetBinaryMode, hSetBuffering, stderr, stdin, stdout, hPutStrLn)
import System.Posix.IO (dup, dupTo, fdToHandle, stdError, stdOutput)
import Types.FeatureFlags (FeatureFlags)
import Types.Grpc (CommandEnv (..), RequestArgs (..))

-- | Whether this process was started as a Bazel-protocol worker.
isBazelWorkerInvocation :: [String] -> Bool
isBazelWorkerInvocation = elem "--persistent_worker"

data WorkRequest = WorkRequest {
  arguments :: [String],
  requestId :: Int32
} deriving stock (Eq, Show)

-- | Serve requests from stdin until it closes.
--
-- A request prelude, when given, runs before each request with the request's
-- arguments, and a request whose prelude fails is answered with the prelude's
-- exit code and output instead of being compiled. It is how a request gets what
-- a one-shot command gets from the wrapper it runs through: lab's toolchain
-- passes its nix substitute wrapper around @true@, which realises the store
-- paths the request's arguments and argsfiles name through the executor's nix
-- daemon. A worker started once has no other point at which that can happen.
runBazelWorker :: Maybe FilePath -> FeatureFlags -> IO ()
runBazelWorker prelude features = do
  responses <- claimStdout
  state <- newState
  status <- newMVar WorkerStatus {active = 0}
  cwdLock <- newProcessCwdLock
  env <- CommandEnv . Map.fromList <$> getEnvironment
  let handler = toGrpcHandler (ghcHandler state features Nothing Nothing cwdLock) status state Nothing
  hSetBinaryMode stdin True
  let loop = readDelimited stdin >>= \case
        Nothing -> hPutStrLn stderr "ghc-worker: stdin closed, exiting"
        Just bytes -> do
          (output, code, rid) <- case decodeWorkRequest bytes of
            Left err -> pure (["ghc-worker: undecodable WorkRequest: " ++ err], 1, 0)
            Right req -> runPrelude req.arguments >>= \case
              Just (out, code) -> pure (out, code, req.requestId)
              Nothing -> do
                result <- try (handler.run env (RequestArgs req.arguments))
                pure case result of
                  Right (out, code) -> (out, code, req.requestId)
                  Left (e :: SomeException) -> (["Uncaught exception: " ++ displayException e], 1, req.requestId)
          BS.hPut responses (encodeWorkResponse code (unlines output) rid)
          hFlush responses
          loop
  loop
  where
    runPrelude args = case prelude of
      Nothing -> pure Nothing
      Just cmd -> do
        (code, out, err) <- readProcessWithExitCode cmd args ""
        pure case code of
          ExitSuccess -> Nothing
          ExitFailure n -> Just (["ghc-worker: request prelude " ++ cmd ++ " exited " ++ show n, out, err], fromIntegral n)

-- | The handle responses go out on, with fd 1 pointed at stderr: a stray
-- write to stdout from GHC or a library would otherwise land in the middle
-- of the response stream and desynchronise it.
claimStdout :: IO Handle
claimStdout = do
  hFlush stdout
  fd <- dup stdOutput
  _ <- dupTo stdError stdOutput
  h <- fdToHandle fd
  hSetBinaryMode h True
  hSetBuffering h (BlockBuffering Nothing)
  pure h

readDelimited :: Handle -> IO (Maybe BS.ByteString)
readDelimited h = readVarint h >>= \case
  Nothing -> pure Nothing
  Just n -> Just <$> BS.hGet h (fromIntegral n)

readVarint :: Handle -> IO (Maybe Word64)
readVarint h = go 0 0
  where
    go shift acc = do
      b <- BS.hGet h 1
      if BS.null b
        then pure Nothing
        else do
          let w = BS.head b
              acc' = acc .|. (fromIntegral (w .&. 0x7f) `shiftL` shift)
          if testBit w 7 then go (shift + 7) acc' else pure (Just acc')

decodeVarint :: BS.ByteString -> Either String (Word64, BS.ByteString)
decodeVarint = go 0 0
  where
    go :: Int -> Word64 -> BS.ByteString -> Either String (Word64, BS.ByteString)
    go shift acc bs = case BS.uncons bs of
      Nothing -> Left "truncated varint"
      Just (w, rest) ->
        let acc' = acc .|. (fromIntegral (w .&. 0x7f) `shiftL` shift)
        in if testBit w 7 then go (shift + 7) acc' rest else Right (acc', rest)

-- | Decode the fields this worker reads and skip the rest (inputs, verbosity,
-- sandbox_dir, cancel), which a singleplex worker without a sandbox has no use for.
decodeWorkRequest :: BS.ByteString -> Either String WorkRequest
decodeWorkRequest = go [] 0
  where
    go :: [String] -> Int32 -> BS.ByteString -> Either String WorkRequest
    go args rid bs
      | BS.null bs = Right WorkRequest {arguments = reverse args, requestId = rid}
      | otherwise = do
          (key, rest) <- decodeVarint bs
          let field = key `shiftR` 3
          case key .&. 7 of
            0 -> do
              (v, rest') <- decodeVarint rest
              go args (if field == 3 then fromIntegral v else rid) rest'
            2 -> do
              (len, rest') <- decodeVarint rest
              let (payload, rest'') = BS.splitAt (fromIntegral len) rest'
              if BS.length payload /= fromIntegral len
                then Left "truncated field"
                else go (if field == 1 then Text.unpack (Text.decodeUtf8Lenient payload) : args else args) rid rest''
            1 -> go args rid (BS.drop 8 rest)
            5 -> go args rid (BS.drop 4 rest)
            w -> Left ("unsupported wire type " ++ show w)

-- | A length-delimited WorkResponse.
encodeWorkResponse :: Int32 -> String -> Int32 -> BS.ByteString
encodeWorkResponse code output rid =
  LBS.toStrict (Builder.toLazyByteString (varint (fromIntegral (BS.length body)) <> Builder.byteString body))
  where
    out = Text.encodeUtf8 (Text.pack output)
    body = LBS.toStrict $ Builder.toLazyByteString $
      Builder.word8 0x08 <> varint (fromIntegral (fromIntegral code :: Int64))
        <> Builder.word8 0x12 <> varint (fromIntegral (BS.length out)) <> Builder.byteString out
        <> (if rid == 0 then mempty else Builder.word8 0x18 <> varint (fromIntegral (fromIntegral rid :: Int64)))

varint :: Word64 -> Builder.Builder
varint n
  | n < 0x80 = Builder.word8 (fromIntegral n :: Word8)
  | otherwise = Builder.word8 (fromIntegral (n .&. 0x7f) .|. 0x80) <> varint (n `shiftR` 7)
