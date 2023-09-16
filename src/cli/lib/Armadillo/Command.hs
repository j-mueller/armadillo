{-| Running blockchain actions
-}
module Armadillo.Command(
  TxCommandError(..),
  runBlockchainAction,

  -- * Individual commands
  deployRefScripts
) where

import           Armadillo.BuildTx                  (ReferenceScripts)
import qualified Armadillo.BuildTx                  as BuildTx
import           Cardano.Api                        (CardanoMode, Env,
                                                     LocalNodeConnectInfo, TxIn)
import qualified Cardano.Api                        as C
import           Control.Monad.Except               (ExceptT, MonadError,
                                                     runExceptT)
import           Convex.BuildTx                     (runBuildTxT)
import           Convex.Class                       (MonadBlockchain,
                                                     MonadBlockchainCardanoNodeT,
                                                     MonadBlockchainError (..),
                                                     runMonadBlockchainCardanoNodeT)
import           Convex.Lenses                      (emptyTx)
import           Convex.NodeClient.WaitForTxnClient (MonadBlockchainWaitingT,
                                                     runMonadBlockchainWaitingT)
import           Convex.Query                       (BalanceAndSubmitError,
                                                     MonadUtxoQuery,
                                                     WalletAPIQueryT,
                                                     balanceAndSubmitOperator,
                                                     runWalletAPIQueryT)
import           Convex.Utils                       (mapError)
import           Convex.Wallet.Operator             (Operator, Signing)
import           ErgoDex.CardanoApi                 (CardanoApiScriptError)
import           Servant.Client                     (ClientEnv)


runBlockchainAction :: Functor m => LocalNodeConnectInfo CardanoMode -> Env -> ClientEnv -> MonadBlockchainWaitingT (MonadBlockchainCardanoNodeT TxCommandError (WalletAPIQueryT (ExceptT TxCommandError m))) a -> m (Either (MonadBlockchainError TxCommandError) a)
runBlockchainAction connectInfo nodeEnv env action =
  let mapErr (Left err)         = Left (MonadBlockchainError err) -- FIXME: This needs to be fixed in MonadBlockchainCardanoNodeT!
      mapErr (Right (Left err)) = Left err
      mapErr (Right (Right x))  = Right x
  in fmap mapErr
      $ runExceptT
      $ runWalletAPIQueryT env
        $ runMonadBlockchainCardanoNodeT connectInfo
          $ runMonadBlockchainWaitingT connectInfo nodeEnv action

data TxCommandError =
  ScriptErr CardanoApiScriptError
  | BalanceSubmitFailed BalanceAndSubmitError
  deriving (Show)

deployRefScripts ::
  ( MonadBlockchain m
  , MonadError TxCommandError m
  , MonadUtxoQuery m
  ) =>
  Operator Signing ->
  m (ReferenceScripts TxIn)
deployRefScripts operator = do
  (cs, btx) <- mapError ScriptErr $ runBuildTxT $ do
    BuildTx.deployReferenceScripts operator
  finalTx <- mapError BalanceSubmitFailed $ balanceAndSubmitOperator operator Nothing (btx emptyTx)
  let txId = C.getTxId $ C.getTxBody finalTx
      mkIdx i = C.TxIn txId (C.TxIx $ fromIntegral i)
  pure (mkIdx <$> cs)
