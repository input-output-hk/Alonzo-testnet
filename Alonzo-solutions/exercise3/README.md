# Alonzo-Blue exercise 3

## Part 1. Create new keys and addresses

Create:
- New verification & signing payment keys (`payment2.vkey` & `payment2.skey`) and
- Also new verifiation & signing stake keys (`stake.vkey` & `stake.skey`) 
- From those, build a new payment address `payment2.addr` & stake-address `stake2.addr`

```
cardano-cli address key-gen \
--verification-key-file payment2.vkey \
--signing-key-file payment2.skey

cardano-cli stake-address key-gen \
--verification-key-file stake2.vkey \
--signing-key-file stake2.skey

cardano-cli address build \
--payment-verification-key-file payment2.vkey \
--stake-verification-key-file stake2.vkey \
--out-file payment2.addr \
--testnet-magic 5

cardano-cli stake-address build \
--stake-verification-key-file stake2.vkey \
--out-file stake2.addr \
--testnet-magic 5
```

#### Part 1.1 Fund the new account
Build a simple transaction for funding the new account. Use the initial account of the testnet that claimed the goddy-bag of test-ADA and send some amount.

To initialize the transaction we have to [build a raw transaction](https://developers.cardano.org/en/testnets/cardano/transactions/creating-transactions/), we will use the `--mary-era` flag when needded

```
cardano-cli transaction build-raw 
```


