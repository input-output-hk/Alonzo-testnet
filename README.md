# Alonzo-testnet
This is the repository for the Cardano Alonzo testnet rollout program.

Here you will find a set of [exercises](https://github.com/input-output-hk/Alonzo-testnet/tree/main/Alonzo-exercises) and support material to use on the Alonzo testnet. We will gradually add materials to this repository through the testnet phases to help you understand the testnet and make use of its features.

The planned syllabus is outlined below.  Exercises will be deployed here and linked from this page as they are finalised.

## Target Audience

The exercises are designed to help those who are unfamiliar with the Cardano node to write and submit Plutus scripts on the initial Alonzo Blue and Alonzo White testnets using the node CLI, so that they can develop their own simple DApps.  Those who are already familiar with the node may choose to skip some of the earlier exercises, of course. Note that there will be no Cardano wallet or Plutus application backend support prior to Alonzo Purple, so functionality will be limited compared with the full Plutus experience.


## Useful Resources

[Instructions for using Docker with the Cardano node](docker.md)

[Plutus Scripts and other useful Resources](resources/)

## Exercise 1: Getting Started

1. Introduction to the Alonzo Testnet.
1. Setting up a node to connect to the testnet.
1. Obtaining test Ada.  


[Exercise 1](https://github.com/input-output-hk/Alonzo-testnet/tree/main/Alonzo-exercises/alonzo-blue/1_Alonzo-blue-exercise-1.md)
[(Model Solutions)](https://github.com/input-output-hk/Alonzo-testnet/tree/main/Alonzo-solutions/exercise1)

[Instructions for using Docker with the Cardano node](docker.md)

## Exercise 2: Hard Fork (Optional)

See an update proposal submitted to the chain and follow along as we go through the hard fork to the Alonzo era.  

(*Note that this may be a one-off event for each Testnet since we will need to start the network from scratch - the timing will be notified on the relevant Discord channel*).

[Exercise 2](Alonzo-exercises/alonzo-blue/2_Alonzo-blue-exercise-2.md)

## Exercise 3: Addresses and Transactions

1. Managing payment addresses.
1. Building, signing and submitting transactions on chain.  
1. Submitting a transaction containing a pre-built Plutus script.

[Exercise 3](Alonzo-exercises/alonzo-blue/3_Alonzo-blue-exercise-3.md)
[(Model Solution)](Alonzo-solutions/exercise3/e3SampleSolution.md)

## Exercise 4: Compiling and Submitting Simple Plutus Scripts (Coming Soon)


1. Compiling simple Plutus transactions. 
2. Submitting simple Plutus transactions. 
3. Calculating fees for Plutus transactions.
4. Determining what effect your Plutus transactions have had on the blockchain.

Exercise 4
(Model Solution)

## Exercise 5: Managing Native Tokens


1. Minting native tokens using both Mary-era forging scripts
1. Minting native tokens using Plutus forging scripts
1. Redeeming/burning native tokens
1. Minting non-fungible tokens
1. Time-based scripts


Exercise 5
(Model Solution)

## Exercise 6: DApps


1. Building more substantial DApps
2. Interacting with external events


Exercise 6
(Model Solution)

## Exercise 7: Interacting with the Wallet


1. The Cardano Wallet CLI
2. Submitting transactions with Plutus Scripts from the wallet CLI
3. Some simple DApps using the wallet CLI


Exercise 7
(Model Solution)

## Disclaimer for Alonzo Blue/White

Note that to ensure a good user experience we will be rolling the Alonzo testnets out in phases. At this point in time, we are restricting access to a few invited users. If you are not one of those users, please bear with us until we are ready to open up for general access.
