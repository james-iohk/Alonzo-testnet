{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusExample.Witness
  ( witnessScript
  , witnessScriptShortBs
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

keyHash :: PubKeyHash
keyHash = "1ed5f183a76122092ac0f7f23427ca22385d5d6e8166300eb71a8b28" -- passed in as first arg at compilation

{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator h _ _ ctx = h P.== wit1
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    wit1 :: PubKeyHash
    wit1 = P.head $ txInfoSignatories info

validator :: PubKeyHash -> Plutus.Validator
validator h = Ledger.mkValidatorScript $
    $$(PlutusTx.compile [|| validatorParam ||])
     `PlutusTx.applyCode`
      PlutusTx.liftCode keyHash
    where validatorParam h = Scripts.wrapValidator (mkValidator h)

script :: Plutus.Script
script = Plutus.unValidatorScript (validator keyHash)

witnessScriptShortBs :: SBS.ShortByteString
witnessScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

witnessScript :: PlutusScript PlutusScriptV1
witnessScript = PlutusScriptSerialised witnessScriptShortBs
