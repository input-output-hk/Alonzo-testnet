# Alonzo-Blue exercise 3

# Part 1. Submit a simple transaction

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

### Part 1.1 Fund the new account
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

- v) Sign the transaction

```
cardano-cli transaction sign \
--tx-body-file ./txs/tx001.raw \
--signing-key-file ./addrs/payment.skey \
--testnet-magic 5 \
--out-file ./txs/tx001.signed
```

- vi) Submit the transaction

```
cardano-cli transaction submit \
--tx-file ./txs/tx001.signed \
--testnet-magic 5
```

### Part 1.2 Check balances
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


# Part 2. Lock a transaction output (tx-out) using a plutus script.

### Part 2.1. Generate a Datum-hash

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

Now we can hash it and save it as `randomDatumHash.txt` in the /txs folder.

```
echo $(cardano-cli transaction hash-script-data \
--script-data-value $(cat ./txs/randomDatum.txt)) \
> ./txs/randomDatumHash.txt
```


### Part 2.2. Send funds to the Script

Before executing a transaction to the script we have to generate the Script address and save it in the /addr folder as `script.addr`

```
cardano-cli address build --payment-script-file untyped-always-succeeds-txin.plutus --testnet-magic 5 --out-file ./addrs/script.addr
```

Now we can build a transaction sending test-ADA to the script

```
cardano-cli transaction build-raw \
--alonzo-era \
--tx-in f2f80bedba5ff3effbc70d188f365e528ae9c04b35acff4d006c6e28477710d4#0 \
--tx-out $(cat ./addrs/script.addr)+150000000 \
--tx-out-datum-hash $(cat ./txs/randomDatumHash.txt) \
--tx-out $(cat ./addrs/payment2.addr)+49824687 \
--fee 175313 \
--protocol-params-file ./txs/protocol001.json \
--out-file ./txs/tx002.raw
```


*Note.-we are re-using protocol-parameters001, since there isn't any changes to the protocol there is no point in generating a new one.*

**Important!** to follow the order of the parameters in the cli. Specially when **attaching Datum to a tx-out**, since txs are unique then attaching datum is bound to the specific utxo.

Finally, sign and submit the transaction

```
cardano-cli transaction sign \
--tx-body-file ./txs/tx002.raw \
--signing-key-file ./addrs/payment2.skey \
--testnet-magic 5 \
--out-file ./txs/tx002.signed


cardano-cli transaction submit \
--tx-file ./txs/tx002.signed \
--testnet-magic 5

```

### Part 2.3. Query the script balance

Just like querying any utxo being called from the `script.addr`. Expected output:

```
$ cardano-cli query utxo --testnet-magic 5 --address $(cat ./addrs/script.addr)
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2c80e8db5205ca6bc60688b951406c929c098daa2665d5767567c39ff58e80f9     0        10140000 lovelace + TxOutDatumHashNone
32fc0be89d9205881332fea97b56ddea39c3bf899dfb0dc5653dc56d6425086c     1        7919 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "530aed6dc4ef3e0801123ca4111d052908600f608de7a3263975e215e19a41d2"
589882385f627a2492bde09bc604dc3181b6273fb29e6f87fb0b7469746bc1df     1        2000000 lovelace + TxOutDatumHashNone
670818aa5e27fb3af009b002ebeb6c8300b70b6f753173aeade97a4cdf335508     0        99000000 lovelace + TxOutDatumHashNone
8a2c8f5b339411ad5b3319aa63bc88cd9a920e8da7a87e9317daa64956a09a2a     1        5000000 lovelace + TxOutDatumHashNone
9c541f9fbf5886853871d93ce22c81bc46438be126bb308eb21e361ea99083cb     0        150000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "59fbd8a237c62308fbe6f68ca1e27b433ca99f28db7b7d3635f1f5eb948826cb"
9fef4121976a73551a3945107af1b4b70c554f9b2004f42bf00d45444ebdf90a     1        100 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ae5703d02f1771ba1e1a5543e5b17a8ae257b7b2b5d446b4b3694084ecd7b80c"
aa1d18824746b58ce4ed242211f4e785d3b97f9fe6b255de27d5c3bb1056a55b     1        1337000 lovelace + TxOutDatumHashNone
b0d717751b6124322ca97c2c0877841b26ac5a1eb484636f134e825806e4237f     1        6000000 lovelace + TxOutDatumHashNone
c75d9f904199310bcff89568450ef5d406b341d139ef157c4ee784d39687add9     0        123456789 lovelace + TxOutDatumHashNone
ed3ab096e1ae38652c8c7e143a405b4836a65f75fe41e1fb67452dd811daeeea     1        5000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "fe0bbfca7377f7ae87929229d73a2f917fc5b1a4952eb9fc4e7ea2521cfc8b17"
f5f2abdd6bbabd470175d4e24c42d334bb276812a4c95b024023ee7c4bed3d4c     0        14141414 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "3fac877e3f1eaefb3cbb71eb8d49248b51571fd18dc02e74f464f038eb734935"
```

Lets focus on the elements that are unique here:

- **`TxHash`** is the identifier of the utxo

- **`TxOutDatumHash`** is the identifier of the datum bound to a script 

what this all mean is that only the address that owns the `TxOutDatumHash` will be able to spend the funds sitting on the script



# Part 3. Can we spend from the Script?

Lets build the transaction to withdraw some fraction of the funds that we deposited to the script. 

```
cardano-cli transaction build-raw \
--alonzo-era \
--fee 804000000 \
--tx-in 9c541f9fbf5886853871d93ce22c81bc46438be126bb308eb21e361ea99083cb#0 \
--tx-in-script-file untyped-always-succeeds-txin.plutus \
--tx-in-datum-value $(cat ./txs/randomDatum.txt) \
--tx-in-redeemer-value $(cat ./txs/randomDatum.txt) \
--tx-in-execution-units "(200000000,200000000)" \
--tx-in-collateral 9c541f9fbf5886853871d93ce22c81bc46438be126bb308eb21e361ea99083cb#1 \
--tx-out $(cat ./addrs/payment.addr)+(-679000000) \
--tx-out $(cat ./addrs/script.addr)+25000000 \
--tx-out-datum-hash $(cat ./txs/randomDatumHash.txt) \
--protocol-params-file ./txs/protocol001.json \
--out-file ./txs/tx003.raw


cardano-cli transaction sign --tx-body-file ./txs/tx003.raw --signing-key-file ./addrs/payment.skey --signing-key-file ./addrs/payment2.skey --testnet-magic 5 --out-file ./txs/tx003.signed


cardano-cli transaction submit --testnet-magic 5 --tx-file ./txs/tx003.signed

```

