#!/bin/sh

export NODE_TAG=1.29.0-rc2
CONFIG=https://hydra.iohk.io/build/7654130/download/1/testnet-config.json
BYRON_GENESIS=https://hydra.iohk.io/build/7654130/download/1/testnet-byron-genesis.json
SHELLEY_GENESIS=https://hydra.iohk.io/build/7654130/download/1/testnet-shelley-genesis.json
ALONZO_GENESIS=https://hydra.iohk.io/build/7654130/download/1/testnet-alonzo-genesis.json
TOPOLOGY=https://hydra.iohk.io/build/7654130/download/1/testnet-topology.json

##Making some folders
mkdir -p ./configuration/config/
mkdir -p ./configuration/topology/
mkdir -p ./configuration/sockets/

##Making DB Folder
mkdir -p ./databases/

##Touch for a Socket
touch ./configuration/sockets/node.socket

##Getting Config
echo "--getting config"
wget  $CONFIG -P ./configuration/config
wget  $BYRON_GENESIS -P ./configuration/config/
wget  $SHELLEY_GENESIS -P ./configuration/config/
wget  $ALONZO_GENESIS -P ./configuration/config/

##Getting Topology
echo "--getting topology"
wget $TOPOLOGY -P ./configuration/topology/

##Starting Docker-Compose
docker-compose up
