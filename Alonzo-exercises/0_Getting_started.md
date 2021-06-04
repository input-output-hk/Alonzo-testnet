#Getting started


##Welcome to the Alonzo Blue Testnet!  

We are excited to have you join us on the journey to delivering Plutus scripts for Cardano.  We will be running through a series of structured tests on the brand-new Alonzo-era Cardano Node so that we can improve the experience for other Plutus users.  At this stage, the software and documentation are both very new, so there will certainly be improvements that can be made.  You will also be using the Cardano node directly rather than the Plutus Application Backend that we are developing to simplify the process of submitting and executing Plutus scripts.  Your role in helping us identify and make these improvements is very important. Your feedback is essential and will be listened to!


##Prerequisites

- Attend the Alonzo Testnet Briefing Meeting or Read the Testnet Briefing Notes.
- Read the Alonzo Testnet Code of Conduct.  This governs your behaviour and responsibilities on the Alonzo Testnets.
- Register yourself on Discord and join the designated channel.
- Make sure you can access:

  1. The IOHK Tutorial Documentation at: https://docs.cardano.org/en/latest/
  2. The Alonzo Blue Testnet configuration information at: [https://hydra.iohk.io/build/6510764/download/1/index.html](https://hydra.iohk.io/build/6510764/download/1/index.html)
  3. Either, the Linux Build at: [https://hydra.iohk.io/build/6538899/download/1/cardano-node-1.27.0-linux.tar.gz
](https://hydra.iohk.io/build/6538899/download/1/cardano-node-1.27.0-linux.tar.gz)
  4. or the Docker Image at: [https://hydra.iohk.io/build/6538875
](https://hydra.iohk.io/build/6538875)

**Note that these are the correctly tagged versions for alonzo-blue1.0 – please check the Discord channel for any updates to these tags.**

- You should be comfortable with using Shell commands and have a basic understanding of the Cardano ecosystem.

- Set up your platform. Unless you are experienced with building the node from source, you should use the pre-built Node and CLI binaries that are supplied by IOG.

- You will need a Physical Host Machine, Virtual Machine or AWS instance with **at least 2 cores, 8GB of RAM, and at least 10GB of free storage.**
- If you are using Docker.
Download and install the latest Docker for your system.  If you are using Docker Desktop, ensure that you allow **at least 6GB of memory.**
- If you are using the binary files.
Make sure you are running a recent stable version of Linux (eg Ubuntu 20.04 or 18.04, Windows 10, or MacOS 10.14 or later).  

**Note that there is no support for Windows or MacOS at this stage.**

- You should need only a bare version of the system (no additional installation).

- Make sure you are on a network that is not firewalled. In particular, we will be using TCP/IP port 3001 by default to establish connections with other nodes, so this will need to be open to the world.

**This should be everything that you need to get you up and running.**

##Objectives

In the first set of exercises, we will make sure that you can:

1. Set up and run an Alonzo-capable node;
2. Connect your node to the Alonzo Blue Testnet blockchain;
3. Obtain some test Ada to use in the Testnet.
