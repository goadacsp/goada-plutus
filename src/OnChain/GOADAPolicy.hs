{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module OnChain.GOADAPolicy
  ( curSymbol,
    serialisedScript,
  )
  where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Plutus.Trace.Emulator as Emulator ()
import qualified PlutusTx
import PlutusTx.Prelude
    ( Bool(False), (||), (&&), ($), (.), Eq((==)), Ord((<)) )
import Ledger
    ( scriptAddress,
      scriptCurrencySymbol,
      ownCurrencySymbol,
      txSignedBy,
      mkMintingPolicyScript,
      unMintingPolicyScript,
      Address,
      Validator(Validator),
      CurrencySymbol,
      TxInfo(txInfoValidRange, txInfoMint),
      ScriptContext(scriptContextTxInfo),
      POSIXTime,
      PubKeyHash,
      after )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value ( flattenValue )
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Short as SBS
import Codec.Serialise ( serialise )

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx = do
    txSignedBy info pkh && (isBurning || isBeforeDeadline)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    isBeforeDeadline :: Bool
    isBeforeDeadline = deadline `after` txInfoValidRange info
    isBurning :: Bool
    isBurning = case flattenValue (txInfoMint info) of
            [(cs, _, amt)] -> cs == ownCurrencySymbol ctx && amt < 0
            _              -> False

-- Template Haskell
policy :: PubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||])
      `PlutusTx.applyCode` PlutusTx.liftCode pkh
      `PlutusTx.applyCode` PlutusTx.liftCode deadline

curSymbol :: PubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline

-- START : only for .plutus compilation
policyValidator :: PubKeyHash -> POSIXTime -> Validator
policyValidator pkh deadline = Validator $ unMintingPolicyScript $ policy pkh deadline

policyScriptAddress :: PubKeyHash -> POSIXTime -> Address
policyScriptAddress pkh deadline = Ledger.scriptAddress $ policyValidator pkh deadline

scriptAsCbor :: PubKeyHash -> POSIXTime -> LB.ByteString
scriptAsCbor pkh deadline = serialise $ policyValidator pkh deadline

serialisedScript :: PubKeyHash -> POSIXTime -> PlutusScript PlutusScriptV1
serialisedScript pkh deadline = PlutusScriptSerialised . SBS.toShort $ LB.toStrict $ scriptAsCbor pkh deadline
