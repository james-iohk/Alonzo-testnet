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
import           PlutusTx.Prelude hiding (Semigroup (..), unless)

{-# INLINABLE mkValidator #-}
mkValidator :: POSIXTime -> BuiltinData -> ScriptContext -> Bool
mkValidator dl _ ctx = range `contains` (to dl)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange info

data Deadline
instance Scripts.ValidatorTypes Deadline where
    type instance DatumType Deadline = POSIXTime
    type instance RedeemerType Deadline = BuiltinData

inst :: Scripts.TypedValidator Deadline
inst = Scripts.mkTypedValidator @Deadline
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @POSIXTime @BuiltinData

deadlineValidator :: Validator
deadlineValidator = Scripts.validatorScript inst

script :: Plutus.Script
script = Plutus.unValidatorScript deadlineValidator

deadlineScriptShortBs :: SBS.ShortByteString
deadlineScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

deadlineScript :: PlutusScript PlutusScriptV1
deadlineScript = PlutusScriptSerialised deadlineScriptShortBs
