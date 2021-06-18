#  Exercise 1
## Alonzo Blue testnet

Building a Cardano passive-node and the **cardano-cli** using Nix

#### 1. Clone/fork [cardano-node](https://github.com/input-output-hk/cardano-node)
Clone the IOHK repo for the cardano-node

    git clone git@github.com:input-output-hk/cardano-node.git

#### 2. Checkout to the right tag

For the Alonzo Blue testnet we will be working with a particular tag, checkout to `tag\alonzo-blue2.0` and create a new branch `alonzo-blue2_0` (note that using underscore for the 2_0 in the branch will prevent ambiguitiy)

    git checkout tag/alonzo-blue2.0 -b alonzo-blue2_0

#### 3. Build node and cli

We now can use Nix to build the node and the cli

    nix-build -A scripts.alonzo-blue.node -o result/alonzo-blue/cardano-node-alonzo-blue
    nix-build -A cardano-cli -o result/alonzo-blue/cardano-cli

#### 4. Add the cli SOCKET

To make quicker access to the cli, we have to add its socket to our `bashrc`. Replace `yourPath` with the path where you have cloned the repo in step 1

    echo export CARDANO_NODE_SOCKET_PATH=~/yourPath/cardano-node/result/alonzo-blue/state-node-alonzo-blue/node.socket

#### 5. Run a node

We will start a passive-node, this is a relay that can communicate with the testnet but will not participate in the creation of blocks. 

If you are joining before the Hard Fork (HF) of June 4, 2021 you can log all the info of the node to capture the transition of the HF, you will still see that we are in the Mary era.

    cd result/alonzo-blue
    ./cardano-node-alonzo-blue/bin/cardano-node-alonzo-blue
    
You should see a lot of information being printed on the screen.

#### 6. Query last block

To verify that we are in sync with the testnet we will use the cli and query the tip of the blockchain

    ./cardano-cli/bin/cardano-cli query tip --testnet-magic 5

we should see something like this

```
{
    "epoch": 194,
    .
    .
    .
    "era": "Alonzo"
}
```

wait a moment and query the newest tip again

```
{
    "epoch": 264,
    .
    .
    .
    "era": "Alonzo"
}
```

Great, the node is up and running!

#### 7. Generate keys

Lastly we will generate payment and staking addresses. 


