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

#### Part 1.2 Check balances
If the transaction was successful and we take a look at the addresses balances, the expected output shold look something like

```
$ cardano-cli query utxo --testnet-magic 5 --address $(cat ./addrs/payment.addr)
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f2f80bedba5ff3effbc70d188f365e528ae9c04b35acff4d006c6e28477710d4     1        999799823455 lovelace + TxOutDatumHashNone


$ cardano-cli query utxo --testnet-magic 5 --address $(cat ./addrs/payment2.addr)
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f2f80bedba5ff3effbc70d188f365e528ae9c04b35acff4d006c6e28477710d4     0        200000000 lovelace + TxOutDatumHashNone
```


## Part 2. Lock a transaction output (tx-out) using a plutus script.

#### Part 2.1. Generate a Datum-hash

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

save it as `randDatumGen.py`, make it executable `$ chmod u+x ./randDatumGen.py` and run it `./txs/randomDatum.txt`.

Now we can hash it 

```
echo $(cardano-cli transaction hash-script-data \
--script-data-value $(cat ./txs/randomDatum.txt)) \
> ./txs/randomDatumHash.txt
```

and save it to `randomDatumHash.txt` in the /txs folder. 

#### Part 2.2. Send funds to the Script

Before executing a transaction to the script we have to generate the Script address and save it in the /addr folder as `script.addr`

```
cardano-cli address build --payment-script-file untyped-always-succeeds-txin.plutus --testnet-magic 5 --out-file ./addrs/script.addr
```

Now we can build a transaction sending test-ADA to import

```shell
cardano-cli transaction build-raw \
--alonzo-era \
--tx-in 7d721d8a0cc2f3d87f44d4df22d6815f58fb67f421b283462ff3b823c36f34a6#1 \
--tx-out $(cat script.addr)+10000000000 \
--tx-out-datum-hash $(cat random_datum_hash.txt) \
--tx-out addr_test1qpkgeus5d5yhpj876f8vx68qp95ftkk6kxw7dq9fmluvlewgmkukektxh5dthk04uhcm90d7pz6njjcfd3y0jjn5klhsk8ghrl+14999000000 \
--fee 1000000 \
--protocol-params-file pparams.json \
--out-file tx.raw
```






```
cardano-cli transaction 
```
