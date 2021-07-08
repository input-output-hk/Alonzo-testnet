# Alonzo White Testnet Exercise Sheet 3 "Submitting Transactions Containing Basic Plutus Scripts"

In the first exercise, you set up and ran a passive Cardano node that was connected to the Alonzo White testnet.  You may also have participated in the Alonzo hard fork in the optional [Exercise 2](2_Alonzo-white-exercise-2.md).  In this exercise, you will build and submit transactions to the Testnet that contain pre-compiled Plutus script and use the node CLI to manage the test ada that you have obtained via payment addresses and UTxOs.

## Prerequisites

- Complete [Exercise Sheet 1](1_Alonzo-white-exercise-1.md)
- Ensure that you have the correctly tagged version of the node and CLI
- Make sure you have some Alonzo White test ada
- Read the tutorial information on:
	- Payment addresses
	- How to build and submit a Cardano transaction
- You may also want to watch [Plutus Pioneer Program - Lecture #2](https://youtu.be/E5KRk5y9KjQ) for background on Plutus scripts

## Objectives

In this set of exercises, we will make sure that you can:

- Create new Payment Addresses and the Associated Private and Public Keys
- Use Payment Addresses to Fund Private “Wallets”
- Build and Submit Transactions to the Testnet Blockchain, including ones containing Plutus scripts.
- Spend funds that are controlled by the script address.

This is the core of what is required to execute Plutus scripts on the Cardano blockchain.

## Exercises
### Part 1: Generating keys and building simple transactions

1. Start a passive Cardano Node if you need to, as you did in [Exercise Sheet 1](1_Alonzo-white-exercise-1.md).  Note that you will not need to change the configuration information unless the testnet has been reset, and the node will synchronise faster than when starting from scratch.

``cardano-node run --config …``

or 

``docker run cardano-node run --config …``


2.	Confirm the amount of White Test Ada that you have been sent in your goody bag or obtained from a faucet:

``cardano-cli query utxo …``

3.	Create a new payment address, `wallet.addr`.  Record your private and public keys. This will act as a “wallet” that you can use to fund transactions and to pay for Plutus script execution etc.

```
cardano-cli address key-gen …
…
```
4.	Transfer some of your test Ada to `wallet.addr` by building, signing and submitting a transaction to the blockchain, as described in the tutorial.

```
cardano-cli transaction build-raw …
…
cardano-cli transaction submit …
```
Confirm that you have successfully funded your wallet.  Remember that you will have to calculate the fee for the transaction (in Lovelace, a fraction of Ada) and account for it in the transaction (higher level wallets do this for you, of course!  We only need to do this because we are using the basic CLI commands).  You may need to wait a short while before the transaction is recorded on chain.  At this stage, all the nodes that are connected to the testnet chain will be able to verify that you have funded the address.

``cardano-cli query utxo …``


### Part 2:  Submit a transaction that uses a Plutus script.

We will first use a pre-built Plutus validator script that always returns `True`. This is the simplest possible validator script (though it is not very useful except as a placeholder/test script!).

5. Download the pre-built [AlwaysSucceeds.plutus](/resources/plutus-scripts/AlwaysSucceeds.plutus) Plutus script, and obtain the script address

``
cardano-cli address build …
``

6. Build a raw transaction that will submit the script and pay for it using funds from `wallet.addr`. You may need to top up the wallet before you do this if you did not transfer enough Ada initially.  For test purposes, assume that the transaction will cost 100 Ada (100,000,000 Lovelace) -- the cost of a real transaction on the Cardano Mainnet will be much less than this, of course.  Until we have improved the CLI, you will also need to specify the "budget" for executing the script in terms of time and memory execution units.  You may assume 10,000,000,000 units in each case (again, these numbers will be much larger than will be required on Mainnet).

``
cardano-cli transaction build-raw …
``

7. Sign the transaction as usual, using the secret key for `wallet.addr`

``
cardano-cli transaction sign …
``

8. Submit the transaction to the chain. 

``
cardano-cli transaction submit …
``

The Plutus script technically controls how the funds can be withdrawn, but it always succeeds, so there is effectively no restriction.

### Part 3:  "Locking" funds using a Plutus script.

We will now do something more useful, by using another pre-built script to "lock" some funds unless a valid input is supplied 

1. Download the pre-built [Lock.plutus](/resources/plutus-scripts/Lock.plutus) script.
2. Choose your favourite number and hash it:

``
cardano-cli transaction hash-script-data --script-data-value 17662
``

3. Build a transaction that includes this hash on the transaction output and submit it.

```
cardano-cli transaction build-raw \
      --tx-out-datum-hash <hash_of_your_chosen_number> \
      ...
```

4. Try spending the funds that are "locked" by the script. Note that you will need to provide some "collateral" to cover the fees in case the script fails to validate, and you will also need to provide a "redeemer".  What happens if you give the wrong value?


### Optional Exercises

7.	Optional Exercise (Easy)

Submit a transaction containing the [AlwaysFails.plutus](/resources/plutus-scripts/AlwaysFails.plutus) Plutus script.  How does the outcome differ from [AlwaysSucceeds.plutus](/resources/plutus-scripts/AlwaysSucceeds.plutus)?
 

8.	Optional Exercise (Easy)

What other kinds of data can you include you include in a datum value?

8.	Optional Exercise (Moderate)

Write your own version of the `AlwaysSucceeds` Plutus script, compile your script and submit it to the Alonzo Testnet.  Verify that it has succeeded.  (Note: we will do this properly in [Exercise Sheet 4](4_Alonzo-white-exercise-4.md) so completing this exercise will involve you reading ahead!)


8.	Optional Exercise (Moderate)

What ways do you have to record “state” information on-chain using Plutus scripts?

The next exercise will involve compiling and submitting some more complex Plutus scripts using your own node.

## Feedback

**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.