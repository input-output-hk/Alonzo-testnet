
# Alonzo White Testnet Exercise Sheet 5: "Multi-Asset Tokens"


In the fourth exercise, you wrote some simple transactions using datums and redeemers. In this exercise, we will practice managing multi-asset tokens.

## Prerequisites ##

1. Complete [Exercise Sheet 4](4_Alonzo-white-exercise-4.md)

2. Start a passive Cardano Node if you need to, and make sure that it has synced with the Testnet.

3. Make sure you have some Alonzo White Test Ada

1. Read the tutorial information on:

    1. Native tokens and Mary-era _forging policy scripts/monetary policy scripts_
    2. How to manage native tokens with Plutus, including Plutus _forging policy_ scripts

## Objectives ##

In the fifth set of exercises, we will make sure that you can:

1. Mint native tokens using both Mary-era _forging scripts_ and Plutus scripts
2. Redeem native tokens
3. Mint non-fungible tokens, including taking payments
4. Manage time-based scripts

## Exercises ##

1. Create a set of private/public signing keys, _shelley_, and two _payment addresses, mary_ and _percy_. Fund the addresses with some test Ada.

2. Define a _forging script_ that allows _shelley_ to create new **Ozymandian** tokens. Define a _monetary policy script_ for the **Shelley** currency that uses this _forging script_. _ **Do not use Plutus scripts at this stage – use a Mary-era forging script.** _

3. Mint 1000 new **Ozymandians** in the _percy_ address by building and submitting a transaction. Check that they have been successfully minted.

```
cardano-cli query utxo –address $(cat percy)
```

4. Define a second _forging script_ that allows _shelley_ to create new **SkyLark** tokens. Mint 100 **SkyLark** tokens and send them to _percy_. Check that the tokens have been received and then send 75 **SkyLark** tokens to _mary._

5. What is the least amount of **Ada** that you need to keep in the _mary_ and _percy_ addresses? What is the least amount of **Ozymandians** or **SkyLarks** that you can keep in an address?

6. You want to _burn_ some of your **Ozymandians** in the _percy_ address_._ How do you do this? What happens to your **Ada** balances when you burn your tokens?

7. Define a Plutus _forging script_ that allows you to mint a variable number of **Ozymandian** and **SkyLark** tokens (with the numbers supplied via a redeemer). Verify that this works as you expect.

8. Define a Plutus _forging script_ that allows you to mint a single instance of a _non-fungible token_. Your script should take a payment from a user-supplied address and pass this payment to an address of your choice.

9. Adapt your solution from Exercise 8 so that you conduct a Dutch auction on your _non-fungible token._ For example, start the bidding at 1000 Ada and reduce the price by 1 Ada every second. Sell the non-fungible token to the first client that offers to pay at least the current price. When the price falls below your hidden _reserve_, reject all future bids.

10. Adapt your solution from Exercise 9 so that the auction for the non-fungible token starts at a predetermined time. Reject all bids that arrive before the time.

11. **Optional Exercise (Easy to Moderate)**

	Publicise your _non-fungible token sale_ and participate in other token sales. Aim to collect the most interesting set of non-fungible tokens. When selling your tokens, you may want to record some metadata on the chain (e.g. representing a digital image or the purchaser&#39;s identity) as well as transferring the non-fungible token itself. How can you do this?

## Feedback


**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.


