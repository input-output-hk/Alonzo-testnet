{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusExample.HelloWorldByteString
  ( helloWorldSerialised
  , helloWorldSBS
  ) where

import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS

import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude as P hiding (Semigroup (..), unless)


{-
  The "hello world" message as a bytestring
-}

hello :: P.ByteString
hello = "Hello World!"

{-
   The Hello World validator script
-}

{-# INLINABLE helloWorld #-}

helloWorld :: P.ByteString -> P.ByteString -> P.ByteString -> ScriptContext -> P.Bool
helloWorld keyword datum _ _ = keyword P.== datum

{-
    As a ScriptInstance
-}

data HelloWorld
instance Scripts.ScriptType HelloWorld where
    type instance DatumType HelloWorld = P.ByteString
    type instance RedeemerType HelloWorld = P.ByteString

helloWorldInstance :: Scripts.ScriptInstance HelloWorld
helloWorldInstance = Scripts.validator @HelloWorld
    ($$(PlutusTx.compile [|| helloWorld ||]) `PlutusTx.applyCode` PlutusTx.liftCode hello)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @P.ByteString @P.ByteString

{-
    As a Validator
-}

helloWorldValidator :: Validator
helloWorldValidator = Scripts.validatorScript helloWorldInstance


{-
    As a Script
-}

helloWorldScript :: Script
helloWorldScript = unValidatorScript helloWorldValidator

{-
    As a Short Byte String
-}

helloWorldSBS :: SBS.ShortByteString
helloWorldSBS =  SBS.toShort . LBS.toStrict $ serialise helloWorldScript

{-
    As a Serialised Script
-}

helloWorldSerialised :: PlutusScript PlutusScriptV1
helloWorldSerialised = PlutusScriptSerialised helloWorldSBS

