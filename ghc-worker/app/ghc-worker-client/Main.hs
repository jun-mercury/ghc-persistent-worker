-- | A client for a running @ghc-worker@ whose command line is GHC's.
--
-- Where buck2 cannot run the worker protocol itself, an action's command is
-- this program: it sends its own arguments and environment to the server
-- listening on the unix socket @$GHC_PERSISTENT_WORKER_SOCKET@ as one
-- @ExecuteCommand@, prints the response's stderr, and exits with the
-- response's exit code. The environment gains @GHC_WORKER_CWD@, the client's
-- working directory, so the server resolves the request's relative paths
-- against the execution root this client runs in (see
-- "GhcWorker.RequestCwd", which reads the same literal).
--
-- Only the proto package and grapesy are linked, so the binary does not carry
-- the @ghc@ library the server does.
module Main where

import Control.Exception (SomeException, displayException, try)
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Network.GRPC.Client (Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (&), (.~))
import BuckWorkerProto (ExecuteCommand, ExecuteCommand'EnvironmentEntry, ExecuteResponse, Worker)
import Proto.Worker_Fields qualified as Fields
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

socketVar :: String
socketVar = "GHC_PERSISTENT_WORKER_SOCKET"

requestCwdVar :: String
requestCwdVar = "GHC_WORKER_CWD"

entry :: (String, String) -> Proto ExecuteCommand'EnvironmentEntry
entry (key, value) =
  defMessage
    & Fields.key .~ BS.pack key
    & Fields.value .~ BS.pack value

request :: [String] -> [(String, String)] -> Proto ExecuteCommand
request argv env =
  defMessage
    & Fields.argv .~ (BS.pack <$> argv)
    & Fields.env .~ (entry <$> env)

execute :: FilePath -> Proto ExecuteCommand -> IO (Proto ExecuteResponse)
execute socket req =
  withConnection def (ServerUnix socket) \ connection ->
    withRPC connection def (Proxy @(Protobuf Worker "execute")) \ call -> do
      sendFinalInput call req
      recvNextOutput call

main :: IO ()
main = do
  argv <- getArgs
  socket <- lookupEnv socketVar >>= \case
    Just s | not (null s) -> pure s
    _ -> fail' (socketVar ++ " is not set; it names the unix socket of the ghc-worker to send this command to")
  cwd <- getCurrentDirectory
  env <- filter ((/= requestCwdVar) . fst) <$> getEnvironment
  result <- try (execute socket (request argv ((requestCwdVar, cwd) : env)))
  case result of
    Left (e :: SomeException) ->
      fail' ("no response from the ghc-worker at " ++ socket ++ ": " ++ displayException e)
    Right response -> do
      let output = response.stderr
      if Text.null output then pure () else Text.hPutStr stderr output
      exitWith case response.exitCode of
        0 -> ExitSuccess
        code -> ExitFailure (fromIntegral code)
  where
    fail' msg = do
      hPutStrLn stderr ("ghc-worker-client: " ++ msg)
      exitWith (ExitFailure 1)
