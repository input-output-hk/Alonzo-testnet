{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusExample.MintingScriptPurple
  ( apiExamplePlutusMintingScriptPurple
  , mintingScriptPurpleShortBs
  ) where

import           Prelude                (IO, Semigroup (..), Show (..), String)

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Short  as SBS

import           Ledger hiding (singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import qualified PlutusTx
import           PlutusTx                 (Data (..))
import           PlutusTx.Prelude hiding (Semigroup (..), unless)

{- HLINT ignore "Avoid lambda" -}

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    }

PlutusTx.makeLift ''MintParams

{-# INLINABLE mkPolicy #-}
mkPolicy :: MintParams -> BuiltinData -> ScriptContext -> Bool
mkPolicy mp _ ctx = traceIfFalse "wrong amount minted" checkMintedAmount

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
      [(cs, tn, amt)] -> cs  == ownCurrencySymbol ctx && tn == (mpTokenName mp) && amt == (mpAmount mp)
      _                -> False

policy :: MintParams -> Scripts.MintingPolicy
policy mp = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp

plutusScript :: Script
plutusScript =
  unMintingPolicyScript (policy mp)
    where mp = MintParams { mpTokenName = "purple",
                            mpAmount    = 1
                          }

validator :: Validator
validator = Validator $ plutusScript

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusMintingScriptPurple :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScriptPurple = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptPurpleShortBs :: SBS.ShortByteString
mintingScriptPurpleShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
