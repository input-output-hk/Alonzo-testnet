# Alonzo Blue Testnet Exercise Sheet 3


## Submitting Transactions Containing Basic Plutus Scripts

In the first exercise, you set up and ran a passive Cardano node that was connected to the Alonzo Blue testnet.  You may also have participated in the Alonzo hard fork in the optional exercise 2.  In this exercise, you will build and submit transactions to the Testnet that contain pre-compiled Plutus script and use the node CLI to manage the test ada that you have obtained via payment addresses and UTxOs.

### Prerequisites

- Complete Exercise Sheet 1
- Start a passive Cardano node, ensure that you use the latest tagged version [alonzo-blue2.0](https://github.com/input-output-hk/cardano-node/releases/tag/alonzo-blue2.0) of the node and cli.
- Make sure you have some Alonzo Blue test ada
- Read the tutorial information on:
	- Payment addresses
	- How to build a Cardano transaction

### Objectives

In this set of exercises, we will make sure that you can:

- Create a new set of keys and address i.e. `wallet_payment.addr, wallet_payment.skey, wallet_payment.vkey`
- Fund your `wallet_payment.addr` using a simple transaction. (Use --mary-era flag for this).    
- Build and submit transactions to the testnet blockchain, including ones containing Plutus scripts.
- Spend funds from the script address.

This is the core of what is required to execute Plutus scripts on the Cardano chain.

## Exercise
### Part 1: Generating keys and building simple transactions

Use cardano_cli to create a new set of keys and addresses.

    cardano-cli address key-gen ...
    cardano-cli address build ...

Fund your newly created address with a simple transaction (use --mary-era flag when needed)

    cardano-cli transaction build-raw --mary-era ...

### Part 2:  Lock a transaction output (tx-out) using a plutus script.

We will use the pre-built [AlwaysSucceeds.plutus](/resources/plutus-scripts/untyped-always-succeeds-txin.plutus) Plutus script to lock some funds. This particular script will always allow you to redeem the funds - Always succeeds! -    

Create a tx ouput with a datum hash at the script address. To lock a tx ouput with a plutus script, it must have a datahash, so:  

- Use a random number generator, and get a number, make it hard.  
- Save it! you will need to provide the number to redeem funds from the script.
- Use `cardano-cli transaction hash-script-data` to hash it

Download the pre-built [AlwaysSucceeds.plutus](/resources/plutus-scripts/untyped-always-succeeds-txin.plutus) Plutus script   

Use `untyped-always-succeeds-txin.plutus` to create a script address

     plutusscriptaddr=$(cardano-cli address build --payment-script-file untyped-always-succeeds-txin.plutus --testnet-magic 5)

Before you build the transaction to lock the funds, you will need to take a look into the new command:

    cardano-cli transaction hash-script-data --script-data-value <random_number>

So you can add the datum hash to the script address.

    cardano-cli transaction build-raw \
      --tx-out-datum-hash <hash_of_your_random_number> \
      ...

Now you have all you need to build a raw transaction that will submit the `Always Succeeds` script;  pay for it using funds from __wallet_payment.addr__ from part 1.

When you succeed check the balance of the script address, for example:

    cardano-cli query utxo --address $plutusscriptaddr --testnet-magic 5

### Part 3: Can you spend the funds on the script?

This particular script always succeeds if you provide the correct datum. 

- Take a look into `cardano-cli transaction build-raw`

- This time the utxo of the script address is used as an input.

- What is the role of the `--tx-in-collateral`?

  It is part of the “2 phase validation” scheme introduced in Alonzo. It specifies inputs that are used as collateral in case any of the scripts in the transaction fail. The collateral inputs must cover the tx fees (times the collateralisation factor which is a protocol parameter) and be ordinary pubkey inputs, not themselves scripts. The collateral inputs will not be consumed in the normal case that all scripts are valid. They will only be consumed if the scripts fail (in which case the normal tx ins will not be consumed). Collateral inputs are only needed for txs with Plutus scripts.

  So you will need a `tx-in-collateral`.  

Some ideas:

    cardano-cli query utxo --address $plutusscriptaddr --testnet-magic 5 --out-file plutusutxo.json
    plutusutxotxin=$(jq -r 'keys[]' plutusutxo.json)

    cardano-cli query utxo --address $utxoaddr --testnet-magic 5 --out-file collateral_utxo.json
    txinCollateral=$(jq -r 'keys[]' collateral_utxo.json)

    receivingAddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4

Then

    cardano-cli transaction build-raw \
      --alonzo-era \
      --fee 400000000 \
      --tx-in $plutusutxotxin \
      --tx-in-collateral $txinCollateral \
      --tx-out "$receivingAddress+100000000" \
      --tx-in-script-file $plutusscriptinuse \
      --tx-in-datum-value <random_number>  \
      --protocol-params-file protocol.json\
      --tx-in-redeemer-value <random_number> \
      --tx-in-execution-units "(200000000,200000000)" \
      --out-file test-alonzo.body


- The tx-in from the script that you should use is the one that is "locked" with your datum hash.

- Take a look into `--tx-in-datum-value`, What if you use a different value?

- What about `--tx-in-redeemer-value` ? Did it play a role on this script?  

- So what is `--tx-in-execution-units` ?

  The units for how long a script executes for and how much memory it uses. This is used to declare the resources used by a particular use of a script. (INT, INT) The time and space units needed by the script. To learn more about this parameters take a look into the protocol parameters:

      cardano-cli query protocol-parameters --testnet-magic 5 --out-file protocol.json
      
       "executionUnitPrices": {
        "priceSteps": 1,
        "priceMemory": 1

- Can you guess someone else datum value and redeem their funds to your own address?

### Important notes on execution units and costs

- For now you have to figure out an upper bound on the execution units. Later on you won't need to specify execution units nor fees.

- In current implementation on Alonzo testnet, a **SAFE** value that would cover all typical scripts on the first stages of the testnet should be around **10 milliseconds (10,000,000,000 picoseconds)**.  

- Or more generally in the **range 100,000,000 -- 10,000,000,000 picoseconds**

- Yes, that creates EXTREMELY HIGH EXECUTIONS COSTS (up to 10,000,000,000 lovelace). **Don't worry, this does not reflect future real mainnet costs**. These large numbers will be resolved when the cost models is completed by the Plutus team.


### Optional Exercise (Moderate)

How can you make your scripts “parametric”? What ways do you have to record “state” information on-chain using Plutus scripts?  How can you set up your scripts to adapt to previously-stored information?

The next exercise will involve compiling and submitting some more complex Plutus scripts using your own node.


### You might be interested:

- [Plutus Pioneer Program - Lecture #2](https://youtu.be/E5KRk5y9KjQ)

- [alwaysSucceedingNAryFunction](https://github.com/input-output-hk/plutus/blob/ffa40bb7c91a575272f3b20b4628432396789360/plutus-ledger-api/src/Plutus/V1/Ledger/Examples.hs#L22)

- [Plutus integration formal specification](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.alonzo-ledger/latest/download-by-type/doc-pdf/alonzo-changes)

- https://input-output-hk.github.io/cardano-node/cardano-api/Cardano-Api-ProtocolParameters.html#g:4

Please let us know of any problems that you have encountered:

Via the Discord channel for general questions
Via the issue tracker at https://github.com/input-output-hk/cardano-node for any bugs.

CL @ 14/6/21
