#!/bin/sh

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
wget  https://hydra.iohk.io/build/6510764/download/1/alonzo-blue-config.json -P ./configuration/config/
wget  https://hydra.iohk.io/build/6510764/download/1/alonzo-blue-byron-genesis.json -P ./configuration/config/
wget  https://hydra.iohk.io/build/6510764/download/1/alonzo-blue-shelley-genesis.json -P ./configuration/config/
wget  https://hydra.iohk.io/build/6510764/download/1/alonzo-blue-alonzo-genesis.json -P ./configuration/config/

##Getting Topology
echo "--getting topology"
wget  https://hydra.iohk.io/build/6510764/download/1/alonzo-blue-topology.json -P ./configuration/topology/

##Starting Docker-Compose
docker-compose up
