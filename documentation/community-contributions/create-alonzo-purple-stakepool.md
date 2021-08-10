# Notes Alonzo Purple

## Prepare account with funds

### Spin up an Alonzo Purple node with docker

```bash
docker run --name alonzonet-node --rm -v alonzonet-data:/data -v alonzonet-ipc:/ipc -v %cd%:/work -w /work -e NETWORK=alonzo-purple inputoutput/cardano-node:alonzo-purple-1.0.1
```

### Open CLI

```bash
docker exec -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket -ti -w /work alonzonet-node bash
# query tip to check sync status
cardano-cli query tip --testnet-magic 8
# get protocol parameters
cardano-cli query protocol-parameters --testnet-magic 8 --out-file protocol.json
```

### Create stake address

```bash
# generate stake address
cardano-cli stake-address key-gen --verification-key-file stake.vkey --signing-key-file stake.skey
# extract readable address
cardano-cli stake-address build --stake-verification-key-file stake.vkey --out-file stake.addr --testnet-magic 8
```

### Create normal address

```bash
# generate address keys
cardano-cli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey
# extract readable address
cardano-cli address build --payment-verification-key-file payment.vkey --out-file payment.addr --testnet-magic 8
# check balance of address
cardano-cli query utxo --address $(cat payment.addr) --testnet-magic 8
```

### Connect normal address with stake address

```bash
# create combined address
cardano-cli address build --payment-verification-key-file payment.vkey --stake-verification-key-file stake.vkey --out-file paymentwithstake.addr --testnet-magic 8
# check balance of address
cardano-cli query utxo --address $(cat paymentwithstake.addr) --testnet-magic 8
```

### get funds

```bash
# request funds
curl -k -v -XPOST "https://faucet.alonzo-purple.dev.cardano.org/send-money/$(cat paymentwithstake.addr)?apiKey=<SEEDISCORD>"
# check funds are received (may take several minutes)
cardano-cli query utxo --address $(cat paymentwithstake.addr) --testnet-magic 8
```

## Spin up stake pool

### Create stake certificate
```bash
cardano-cli stake-address registration-certificate --stake-verification-key-file stake.vkey --out-file stake.cert
```

### Publish stake certificate
```bash
cardano-cli query utxo --address $(cat paymentwithstake.addr) --testnet-magic 8
> 1000000000000

# Create transaction draft
cardano-cli transaction build-raw --tx-in 500c11b65020cefe9227719ea0d7a2d249c6719b24380949ff433c7d8f949399#0 --tx-out $(cat paymentwithstake.addr)+1000000000000 --fee 0 --out-file tx_stake.raw --certificate-file stake.cert

# Calculate fee
cardano-cli transaction calculate-min-fee --tx-body-file tx_stake.raw --tx-in-count 1 --tx-out-count 1 --witness-count 1 --testnet-magic 8 --protocol-params-file protocol.json
> 172849 Lovelace

# Calculate change (2 ada must be deposited for staking cert)
expr 1000000000000 - 172849 - 2000000
> 999997827151

# Final transaction
cardano-cli transaction build-raw --tx-in 500c11b65020cefe9227719ea0d7a2d249c6719b24380949ff433c7d8f949399#0 --tx-out $(cat paymentwithstake.addr)+999997827151 --fee 172849 --out-file tx_stake.raw --certificate-file stake.cert

# Sign
cardano-cli transaction sign --tx-body-file tx_stake.raw --signing-key-file stake.skey --signing-key-file payment.skey --testnet-magic 8 --out-file tx_stake.signed

# Submit
cardano-cli transaction submit --tx-file tx_stake.signed --testnet-magic 8
```


### Find current KES Period

Lookup `slotsPerKESPeriod` and `maxKESEvolutions` at https://hydra.iohk.io/build/7189190/download/1/alonzo-purple-shelley-genesis.json

Should be 
```
"slotsPerKESPeriod": 129600,
"maxKESEvolutions": 62,
```

Find current slot with
```bash
cardano-cli query tip --testnet-magic 8
```

Calculate current KES Period
```
expr 425273 / 129600
```

Sould result in 3

### Create pool certificate
```bash
cardano-cli node key-gen --cold-verification-key-file cold.vkey --cold-signing-key-file cold.skey --operational-certificate-issue-counter-file cold.counter
cardano-cli node key-gen-VRF --verification-key-file vrf.vkey --signing-key-file vrf.skey
cardano-cli node key-gen-KES --verification-key-file kes.vkey --signing-key-file kes.skey
cardano-cli node issue-op-cert --kes-verification-key-file kes.vkey --cold-signing-key-file cold.skey --operational-certificate-issue-counter cold.counter --kes-period 3 --out-file node.cert
```

### Create pool registration
```bash

# get metadata hash
cardano-cli stake-pool metadata-hash --pool-metadata-file pool-metadata-euro.json
> ab850709ce456fb39fac6bc5fa18ef2c86c53e0d6720d5b4e5ca3a493577b71e

# create pool-registration cert
cardano-cli stake-pool registration-certificate --cold-verification-key-file cold.vkey --vrf-verification-key-file vrf.vkey --pool-pledge 900000000000 --pool-cost 340000000 --pool-margin 0 --pool-reward-account-verification-key-file stake.vkey --pool-owner-stake-verification-key-file stake.vkey --testnet-magic 8 --pool-relay-ipv4 49.12.86.157 --pool-relay-port 3007 --metadata-url https://cardano-tools.io/pool-metadata-euro.json --metadata-hash ab850709ce456fb39fac6bc5fa18ef2c86c53e0d6720d5b4e5ca3a493577b71e --out-file pool-registration.cert

# create delegation cert for pledge
cardano-cli stake-address delegation-certificate --stake-verification-key-file stake.vkey --cold-verification-key-file cold.vkey --out-file delegation.cert

# Submit certs
cardano-cli query utxo --address $(cat paymentwithstake.addr) --testnet-magic 8
> 999997827151

cardano-cli transaction build-raw --tx-in 05b8a9b7accaaafd541ae43f792d12f8f525bb2151460a37c08ff343663f8c42#0 --tx-out $(cat paymentwithstake.addr)+999997827151 --fee 0 --out-file tx_pool.raw --certificate-file pool-registration.cert --certificate-file delegation.cert

# Calculate fee
cardano-cli transaction calculate-min-fee --tx-body-file tx_stake.raw --tx-in-count 1 --tx-out-count 1 --witness-count 1 --testnet-magic 8 --protocol-params-file protocol.json
> 173025 Lovelace

# Calculate change (500 ada must be deposited for pool registration)
expr 999997827151 - 187677 - 500000000
> 999497639474

# build final transaction
cardano-cli transaction build-raw --tx-in 05b8a9b7accaaafd541ae43f792d12f8f525bb2151460a37c08ff343663f8c42#0 --tx-out $(cat paymentwithstake.addr)+999497639474 --fee 187677 --out-file tx_pool.raw --certificate-file pool-registration.cert --certificate-file delegation.cert

# Sign
cardano-cli transaction sign --tx-body-file tx_pool.raw --signing-key-file payment.skey --signing-key-file stake.skey --signing-key-file cold.skey --testnet-magic 8 --out-file tx_pool.signed

# Submit, when submit fails just use the fee from the error meesage and create/sign TX again
cardano-cli transaction submit --tx-file tx_pool.signed --testnet-magic 8

# Check if pool is registred
cardano-cli stake-pool id --cold-verification-key-file cold.vkey --output-format hex
cardano-cli stake-pool id --cold-verification-key-file cold.vkey

# Check rewards
cardano-cli query stake-distribution --testnet-magic 8 > stake-distribution.json
cardano-cli query ledger-state --testnet-magic 8 > ledger-state.json
cardano-cli query stake-address-info --testnet-magic 8 --address $(cat stake.addr)

```

### Start pool
```bash
docker run --name alonzonet-node --rm -v alonzonet-data:/data -v alonzonet-ipc:/ipc -v %cd%:/work -w /work -e CARDANO_CONFIG=/work/config/alonzo-purple-config.json -e CARDANO_TOPOLOGY=/work/config/alonzo-purple-topology.json -e CARDANO_DATABASE_PATH=/data/db -e CARDANO_BIND_ADDR=0.0.0.0 -e CARDANO_PORT=3007 -e CARDANO_SOCKET_PATH=/ipc/node.socket -e CARDANO_BLOCK_PRODUCER=true -e CARDANO_SHELLEY_KES_KEY=/work/kes.skey -e CARDANO_SHELLEY_VRF_KEY=/work/vrf.skey -e CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE=/work/node.cert inputoutput/cardano-node:alonzo-purple-1.0.1 run
```

### As docker-compose
```yaml
version: '3'

services:

  alonzo-pool:
      image: inputoutput/cardano-node:alonzo-purple-1.0.1
      restart: always
      volumes:
      - .:/work
      - alonzonet-data:/data
      - alonzonet-ipc:/ipc
      hostname: alonzo-pool.local
      environment:
      - CARDANO_CONFIG=/work/config/alonzo-purple-config.json 
      - CARDANO_TOPOLOGY=/work/config/alonzo-purple-topology.json 
      - CARDANO_DATABASE_PATH=/data/db 
      - CARDANO_BIND_ADDR=0.0.0.0 
      - CARDANO_PORT=3007 
      - CARDANO_SOCKET_PATH=/ipc/node.socket 
      - CARDANO_BLOCK_PRODUCER=true 
      - CARDANO_SHELLEY_KES_KEY=/work/kes.skey 
      - CARDANO_SHELLEY_VRF_KEY=/work/vrf.skey 
      - CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE=/work/node.cert
      command: run

volumes:
  alonzonet-data:
  alonzonet-ipc:

```

## fix file permissions

```bash
chmod og-rwx *
```