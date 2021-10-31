## Plutus Smart Contract - Spin up local dev environment

1. spin up local testnet
	- explain about `cardano-node`, `cardano-cli`

2. lock and unlock a fund via a smart contract
    - lock fund
    - unlock fund


## others:
1. write smart contracts
    - stimulate on local
2. build smart contract (haskell) --> sm script (plutus code)
3. build smart contract script (plutus code) --> script address


```
export script_path=/home/longka/sandbox/Alonzo-testnet/resources/plutus-sources/plutus-alwayssucceeds/abc.script
cardano-cli address build \
    --payment-script-file $script_path \
    --testnet-magic $TESTNETMAGIC \
    --out-file abc.addr
```

4. lock fund/ submit smart contract to chain (testnet/ mainnet) (cardano-cli)

```
export script_datum_hash=`cardano-cli transaction hash-script-data --script-data-value 101`

# get protocol file
cardano-cli query protocol-parameters \
--testnet-magic ${TESTNETMAGIC} \
--out-file pparams.json

# build transaction
cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNETMAGIC} \
--change-address $(cat payment.addr) \
--tx-in 92218fb23155f01e542de05c710dfd8fd6004b95f787ad1ffd9cc99f38607d8f#0 \
--tx-out $(cat script.addr)+101000000 \
--tx-out-datum-hash ${script_datum_hash} \
--protocol-params-file pparams.json \
--out-file abc.script.build

# sign tnx
cardano-cli transaction sign \
--tx-body-file abc.script.build \
--signing-key-file payment.skey \
--testnet-magic ${TESTNETMAGIC} \
--out-file abc.script.signed

# submit transaction
cardano-cli transaction submit --testnet-magic ${TESTNETMAGIC} --tx-file abc.script.signed
```

5. unlock/ spend fund located at script address (locked at step 4) 

### check collateral balance

```
$ cardano-cli query utxo --address $(cat payment2.addr) --testnet-magic ${TESTNETMAGIC}

>>>                          TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bd13868bb8c7b2ab058d8055f492217d9a9e8e27e6d542d695b7b88fbbab524a     0        989831551 lovelace + TxOutDatumHashNone
```

```
export txCollateral="bd13868bb8c7b2ab058d8055f492217d9a9e8e27e6d542d695b7b88fbbab524a#0"
```

### tx in which contains hash datum
```
cac6439186526eb3d8491063d69ffccc3734a47917466919f4610084e5aca457     1        101000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "4250ea713ad7ba3b121621a8d14d8e39a4300065314b7ce9a40526acf992c8e3"
```

```
export plutusutxotxin=cac6439186526eb3d8491063d69ffccc3734a47917466919f4610084e5aca457#1
```

### build transaction
```
cardano-cli transaction build \
--alonzo-era \
--testnet-magic ${TESTNETMAGIC} \
--tx-in ${plutusutxotxin} \
--tx-in-script-file $script_path \
--tx-in-datum-value 101 \
--tx-in-redeemer-value 11 \
--tx-in-collateral ${txCollateral} \
--change-address $(cat payment.addr) \
--protocol-params-file pparams.json \
--out-file abc.script.unlock.tx
```

### signing tnx
```
cardano-cli transaction sign \
--tx-body-file abc.script.unlock.tx \
--signing-key-file payment2.skey \
--testnet-magic ${TESTNETMAGIC} \
--out-file abc.script.unlock.tx.signed
```

### submit tnx
```
cardano-cli transaction submit --testnet-magic ${TESTNETMAGIC} --tx-file abc.script.unlock.tx.signed 
```
