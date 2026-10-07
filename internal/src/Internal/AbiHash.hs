{-# LANGUAGE CPP #-}

module Internal.AbiHash where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isSpace)
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session (targetProfile)
import GHC.Iface.Binary (CheckHiWay (IgnoreHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Unit.Module.ModIface (ModIface, mi_mod_hash, mi_src_hash)
import GHC.Utils.Logger (LogFlags (..), log_default_dump_context)
import GHC.Utils.Outputable (ppr, renderWithContext)
import System.FilePath (dropExtension)

#if !MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Unit.Module.ModIface (mi_final_exts)

#endif

data AbiHash =
  AbiHash {
    path :: String,
    hash :: String,
    srcHash :: String
  }
  deriving stock (Eq, Show)

-- | The @.hash@ sidecar's contents: the ABI hash on the first line, as before, and the interface's source hash on the
-- second. Both are needed to decide whether an in-memory 'ModIface' still stands in for the interface on disk, and
-- reading them from here rather than from the interface keeps that check out of the binary format, which moves between
-- compiler versions.
-- | The sidecar for an interface the compile has just written, so that the two hashes are read out of the 'ModIface'
-- in one place rather than at each call site.
abiHashFor :: HscEnv -> ModIface -> String -> AbiHash
abiHashFor hsc_env iface path =
  AbiHash {path, hash = showAbiHash hsc_env iface, srcHash = show (mi_src_hash iface)}

renderSidecar :: AbiHash -> String
renderSidecar AbiHash {hash, srcHash} = unlines [strip hash, strip srcHash]
  where
    -- 'showAbiHash' renders through 'ppr', which may break its output across lines, and a line-delimited file cannot
    -- carry a field that contains a newline. The reader compares with whitespace removed, so removing it here loses
    -- nothing and keeps one field to a line.
    strip = filter (not . isSpace)

-- | The two hashes of a sidecar written by 'renderSidecar', or 'Nothing' when it has fewer than two lines, which is
-- what a sidecar written before this format looks like.
parseSidecar :: String -> Maybe (String, String)
parseSidecar text = case lines text of
  abi : src : _ -> Just (abi, src)
  _ -> Nothing

showAbiHash :: HscEnv -> ModIface -> String
showAbiHash HscEnv {hsc_dflags} iface =
  dump hsc_dflags hash
  where
    dump dflags = renderWithContext (log_default_dump_context (initLogFlags dflags)) . ppr

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
    hash = mi_mod_hash iface
#else
    hash = mi_mod_hash (mi_final_exts iface)
#endif

readAbiHash ::
  MonadIO m =>
  HscEnv ->
  Maybe String ->
  m (Maybe (ModIface, AbiHash))
readAbiHash hsc_env@HscEnv {hsc_dflags, hsc_NC} (Just path) = do
  let hi_file = dropExtension path
  iface <- liftIO $ readBinIface (targetProfile hsc_dflags) hsc_NC IgnoreHiWay QuietBinIFace hi_file
  pure (Just (iface, abiHashFor hsc_env iface path))

readAbiHash _ _ = pure Nothing
