# Alonzo-Blue exercise 3

## Part 1. Submit a simple transaction

Before transacting we have to create:
- New verification & signing payment keys (`payment2.vkey` & `payment2.skey`) and
- Also new verifiation & signing stake keys (`stake.vkey` & `stake.skey`) 
- From those, build a new payment address `payment2.addr` & stake-address `stake2.addr`

make sure that the account with funds is in a subdir `./addrs/`, to keep things tidy we will create the new account there as well

```
cardano-cli address key-gen \
--verification-key-file ./addrs/payment2.vkey \
--signing-key-file ./addrs/payment2.skey

cardano-cli stake-address key-gen \
--verification-key-file ./addrs/stake2.vkey \
--signing-key-file ./addrs/stake2.skey

cardano-cli address build \
--payment-verification-key-file ./addrs/payment2.vkey \
--stake-verification-key-file ./addrs/stake2.vkey \
--out-file ./addrs/payment2.addr \
--testnet-magic 5

cardano-cli stake-address build \
--stake-verification-key-file ./addrs/stake2.vkey \
--out-file ./addrs/stake2.addr \
--testnet-magic 5
```

#### Part 1.1 Fund the new account
Build a simple transaction for funding the new account. Use the initial account of the testnet that claimed the goddy-bag of test-ADA and send some amount.

To initialize the transaction we have to [build a raw transaction](https://docs.cardano.org/projects/cardano-node/en/latest/stake-pool-operations/simple_transaction.html), we will use the `--mary-era` flag when needded

- i) Get protocol parameters

Save the parameters in a json file `protocol001.json`
```
cardano-cli query protocol-parameters \
--testnet-magic 5 \
--out-file ./txs/protocol001.json
```

- ii) Draft the transaction
Save the draft in `tx001.draft`

```
cardano-cli transaction build-raw \
--tx-in $TxHash#$TxIx \
--tx-out $(cat payment2.addr)+0 \
--tx-out $(cat payment.addr)+0 \
--invalid-hereafter $TTL \
--fee 0 \
--out-file ./txs/tx001.draft
```

where 
    - $TxHash & $TxIx : can be found after prinitng the utxo balance of some address (eg. `shell$ cardano-cli query utxo --testnet-magic 5 --address $(cat ./addrs/payment.addr)` )
    - $TTL : is the time to live for a transaction and is the slot height limit for our transaction to be included in a block. It is good to add ~200 slots for us to complete the submission of a transaction so **current-slot + 200** (to get the current slot query the tip)

- iii) Calculate fees

To calculate the fee we need to include the draft transaction `tx001.draft`
```
cardano-cli transaction calculate-min-fee \
--tx-body-file ./txs/tx001.draft \
--tx-in-count 1 \
--tx-out-count 2 \
--witness-count 1 \
--byron-witness-count 0 \
--testnet-magic 5 \
--protocol-params-file ./txs/protocol001.json
```

we should get the fees printed on the screen. Now we have to calculte how much change we have to return to the sender of the transaction 

```
expr <UTXO BALANCE> - <AMOUNT TO SEND> - <TRANSACTION FEES>
```

- iv) Build the transaction

```
cardano-cli transaction build-raw \
--tx-in 45f18bbd3f3454e230ced8244d6391c2a994c59dc810649b89f87b4fdc6cf805#1 \
--tx-out $(cat ./addrs/payment2.addr)+200000000 \
--tx-out $(cat ./addrs/payment.addr)+999799823455 \
--invalid-hereafter 1943280 \
--fee 176545 \
--out-file ./txs/tx001.raw
```

- ii) Sign the transaction

```
cardano-cli transaction sign \
--tx-body-file ./txs/tx001.raw \
--signing-key-file ./addrs/payment.skey \
--testnet-magic 5 \
--out-file ./txs/tx001.signed
```

iii) Submit the transaction

```
cardano-cli transaction submit \
--tx-file ./txs/tx001.signed \
--testnet-magic 5
```

## Part 2. Lock a transaction output (tx-out) using a plutus script.

The pre-build script [AlwaysSucceeds.hs](https://github.com/input-output-hk/cardano-node/blob/master/plutus-example/plutus-example/src/Cardano/PlutusExample/Untyped/AlwaysSucceeds.hs) is used to lock funds in the script and as the name suggests it always allows withdrawing them back. 

To lock funds in the Plutus script we have to create a tx output with **datum-hash** at the script address. One way to do this is to create a long random number and save it as `randomDatum.txt`, this can be done quickly using a simple python script

```python
#!/usr/bin/env python3
from numpy import random
randDatum = ''
for i in range(4):
    randDatum += str(random.randint(1e3,1e4-1))

with open("./txs/randomDatum.txt", "w") as f:
    f.write(randDatum)
print(randDatum)
```

save it as `randDatumGen.py`, make it executable `$ chmod u+x ./randDatumGen.py`.

Now we can hash it 

```
./txs/randomDatum.txt
echo $(cardano-cli transaction hash-script-data \
--script-data-value $(cat ./txs/randomDatum.txt)) \
> ./txs/randomDatumHash.txt
```

and save it to 









```
cardano-cli transaction 
```
