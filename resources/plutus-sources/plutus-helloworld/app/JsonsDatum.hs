{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE DerivingStrategies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeApplications            #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}

import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import           Data.Aeson
import qualified Data.ByteString.Short as SBS

import           GHC.Generics

import           Data.Maybe

import qualified Plutus.V1.Ledger.Api
import           Ledger hiding (singleton)

import           PlutusTx
import           PlutusTx.Prelude as P (BuiltinByteString)

import           Cardano.PlutusExample.HelloWorldByteStringParametric (hello, helloWorldSBS, helloWorldSerialised)


data FlemingDatum = FlemingDatum
    { flemingDatumProjectAddress :: !PubKeyHash
    -- ^ address of the project we want to send ADA to
    , flemingDatumContributorAddress :: !PubKeyHash
    -- ^ address of the contributor 
    , flemingDatumAmt             :: !Integer
    -- ^ amount of ADA somebody wants to donate.
    -- From that amount we subtract 'flemingPercentage'
    , flemingDatumBondingAmt      :: !Integer
    -- ^ amount that is put on Fleming DAO script address
    , flemingDatumTokenAmt        :: !Integer
    -- ^ number of tokens received for donation
    , flemingDatumPreviousContributors  :: [(PubKeyHash, (Integer, POSIXTime))]
    } deriving stock (Prelude.Eq, Generic)
      deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''FlemingDatum

datum :: FlemingDatum
datum = FlemingDatum {
            flemingDatumProjectAddress = fromJust $ decode $ "{\"getPubKeyHash\" : \"d879909e57d267af861526550a1aa53a50b73d1a910700fb660d2beb\"}",
            flemingDatumContributorAddress = fromJust $ decode $ "{\"getPubKeyHash\" : \"07ec3f8ec650ee1d1b672d24e67180aafe4f4081e73a0683d505e6e3\"}",
            flemingDatumAmt = 100000,
            flemingDatumBondingAmt = 100,
            flemingDatumTokenAmt = 100,
            flemingDatumPreviousContributors = []
                     }

main :: IO ()
main = do
    print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusTx.toData datum)
    return ()
