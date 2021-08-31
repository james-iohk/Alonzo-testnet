{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusExample.DeadlineRedeemer
  ( deadlineScript
  , deadlineScriptShortBs
  ) where

import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts

import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import           PlutusTx.Prelude as P hiding (Semigroup (..), unless)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{-# INLINABLE mkPolicy #-}
mkPolicy :: POSIXTime -> ScriptContext -> Bool
mkPolicy dl ctx = (from dl) `contains` range
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange info

policy :: Scripts.MintingPolicy
policy = Plutus.mkMintingPolicyScript
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapMintingPolicy mkPolicy

plutusMintingScript :: Plutus.Script
plutusMintingScript =
  Plutus.unMintingPolicyScript policy

mintingValidator :: Plutus.Validator
mintingValidator =
  Plutus.Validator $ Plutus.unMintingPolicyScript policy

deadlineScriptShortBs :: SBS.ShortByteString
deadlineScriptShortBs = SBS.toShort . LBS.toStrict $ serialise mintingValidator

deadlineScript :: PlutusScript PlutusScriptV1
deadlineScript = PlutusScriptSerialised deadlineScriptShortBs
