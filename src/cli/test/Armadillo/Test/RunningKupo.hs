{-# LANGUAGE DeriveAnyClass #-}
module Armadillo.Test.RunningKupo(
  RunningKupo(..),
  KupoLog(..),
  withKupo
) where

import           Armadillo.Kupo            (KupoConfig (..))
import           Control.Concurrent.Async  (race)
import           Control.Tracer            (Tracer, traceWith)
import           Convex.Devnet.CardanoNode (RunningNode (..))
import           Convex.Devnet.Utils       (checkProcessHasNotDied, withLogFile)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Text                 as Text
import           Data.Void                 (absurd)
import           GHC.Generics              (Generic)
import           GHC.IO.Handle.Types       (Handle)
import           System.FilePath           ((</>))
import           System.IO                 (BufferMode (NoBuffering),
                                            hSetBuffering)
import           System.Process            (CreateProcess (..), ProcessHandle,
                                            StdStream (UseHandle), proc,
                                            withCreateProcess)

data KupoLog =
  StartingKupo KupoConfig
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RunningKupo =
  RunningKupo
    { rkHandle :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    , rkConfig :: KupoConfig
    }

withKupo :: Tracer IO KupoLog -> FilePath -> RunningNode -> KupoConfig -> (RunningKupo -> IO a) -> IO a
withKupo tracer stateDirectory node cfg action = do
  let logFilePath = stateDirectory </> "kupo.log"
      process = kupoProcess (Just stateDirectory) node cfg
  withLogFile logFilePath $ \out -> do
    hSetBuffering out NoBuffering
    traceWith tracer (StartingKupo cfg)
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \stdin stdout stderr processHandle -> do
        let rae = RunningKupo{rkHandle = (stdin, stdout, stderr, processHandle), rkConfig = cfg}
        fmap (either absurd id) $ race
            (checkProcessHasNotDied (Text.pack kupoExecutable) processHandle)
            (action rae)

kupoExecutable :: String
kupoExecutable = "kupo"

kupoProcess :: Maybe FilePath -> RunningNode -> KupoConfig -> CreateProcess
kupoProcess cwd RunningNode{rnNodeSocket, rnNodeConfigFile} KupoConfig{kcHost, kcPort} = (proc kupoExecutable strAgrs){cwd} where
  strAgrs =
    [ "--node-socket", rnNodeSocket
    , "--node-config", rnNodeConfigFile
    , "--host", kcHost
    , "--port", show kcPort
    , "--since", "origin"
    , "--in-memory"
    , "--match", "*/*"
    ]
