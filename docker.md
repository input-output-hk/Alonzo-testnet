# How to Run the Cardano Node and CLI using Docker

## Set up Docker

Download and Install Docker/Docker Desktop from [https://docs.docker.com/get-docker/]()

Make sure you have allocated enough RAM to Docker (eg set the RAM to 8GB in the Docker Desktop requirements) and start the docker daemon, as described in the Docker instructions.

## Set up the Cardano node

Download the correct `cardano-node` image

``docker image pull inputoutput/cardano-node:<TAG>``

where `<TAG>` is the tag for the version that you require (e.g. `latest` for the most recent stable version, or `1.27.0` for node version 1.27.0).

Create local `data` and `node-ipc` volumes

```
docker volume create data
docker volume create node-ipc
```

## Run the Cardano node

Run a passive node that is connected to the correct network. For example, for a `mainnet` node:

``docker run -e NETWORK=mainnet -v node-ipc:/ipc -v data:/data inputoutput/cardano-node``

This creates a persistent docker environment for the node, where `/ipc` in the Docker container is connected to the logical `node-ipc`volume, and `/data` is connected to the `data` volume.  Change `NETWORK=mainnet` if you are connecting to a different network (eg a testnet).  

You may also run the node with specific parameters:

``docker run -v node-ipc:/ipc -v data:/data inputoutput/cardano-node run --help``

### Passing Explicit Configuration Parameters

If there is no pre-defined network configuration, you will need to download the specified configuration files, copy these to the persistent Docker volume, and pass the parameters on the command line:

``docker run -e NETWORK=mainnet -v node-ipc:/ipc -v data:/data inputoutput/cardano-node run --config ... --topology ... --``

## Run Cardano CLI commands

You can now run normal Cardano CLI commands.  For example,

```
export CLI='docker run -it --entrypoint cardano-cli -e NETWORK=mainnet -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket -v node-ipc:/ipc inputoutput/cardano-node'
$CLI version
$CLI query tip --mainnet
$CLI transaction build-raw ... --mainnet
```

You need to specify `CARDANO_NODE_SOCKET_PATH` to point to the correct location in the container (`/ipc/node.socket` is the standard location).

## Run a Shell in the Docker Container

To run a shell in the container (so you can inspect or change settings, for example), use the `bash` or `sh` entry point.

``docker run -it --entrypoint bash -v node-ipc:/ipc -v data:/data inputoutput/cardano-node``

You may now run any commands that you require with full access to the container's file systems.

```
bash-4.4# cd /data/db
bash-4.4# ls
immutable  ledger  lock  protocolMagicId  volatile
bash-4.4# 
```

## Copy files to/from the Docker Container

To copy files to/from the docker container use `docker cp`.

For example, if the process id of the running container is `760199bf3561`

```
kh@vulcan:~$ docker ps
CONTAINER ID   IMAGE                      COMMAND                  CREATED       STATUS       PORTS     NAMES
760199bf3561   inputoutput/cardano-node   "/nix/store/p435ajna…"   ...
```

Then to copy the `immutable` directory (if you have a cached version), for example, you can:

``docker cp db/immutable 760199bf3561:/data/db/immutable``

You can also mount local directories for use by the container.  For example to share the `db` and `node-ipc` directories, you could:

``docker run ---mount type=bind,source="$(pwd)/db/",target=/data/db  --mount type=bind,source="$(pwd)/node-ipc",target=/ipc inputoutput/cardano-node ...``


