This directory is adapted from the Plutus examples in [cardano-node](https://github.com/input-output-hk/cardano-node).  This version includes `plutus-helloworld` which instantiates the example with a different script (the `HelloWorld` script needed for the Exercises).

The original sources and further examples can be found in:

[https://github.com/input-output-hk/cardano-node/tree/master/plutus-example](https://github.com/input-output-hk/cardano-node/tree/master/plutus-example)

[https://github.com/input-output-hk/cardano-node/tree/master/scripts/plutus](https://github.com/input-output-hk/cardano-node/tree/master/scripts/plutus)

# Building and running the examples with nix

0. Assuming you have nix installed on your system, configure the nix cache

    If you are using [NixOS](https://nixos.org/) add the snippet below to your
    `/etc/nixos/configuration.nix`:

    ```
    nix.binaryCaches = [
    "https://cache.nixos.org"
    "https://hydra.iohk.io"
    ];

    nix.binaryCachePublicKeys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    ```

    If you are using the `nix` package manager next to another operating system put
    the following in `/etc/nix/nix.conf` if you have a system-wide `nix`
    installation , or in `~/.config/nix/nix.conf` if you have a local installation:

    ```
    substituters        = https://hydra.iohk.io https://cache.nixos.org/
    trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
    ```
1. Start a nix shell
    ```
    nix-shell
    ```
2. Run cabal to build or run subprojects, for example:
    ```
    cabal run exe:plutus-alwayssucceeds
    ```
3. If you encounter errors mentioning "crypto_vrf_proofbytes", create a file called `cabal.project.local` with the following content:
    ```
    package cardano-crypto-praos
    flags: -external-libsodium-vrf
    ```
4. If you encounter errors mentioning libsystemd, then run:
   ```
    ./scripts/gen-cabal-nosystemd.sh
    ```
    This will create a new file called `cabal.nosystemd.project`.

5. If you need a `cabal.project.local` file (for example, because of step 3), then you have to
   rename it to `cabal.nosystemd.project.local`
6. Run cabal by using the new project file:
    ```
    cabal --project-file=cabal.nosystemd.project run exe:plutus-alwayssucceeds
    ```

# plutus-example

This directory demonstates end-to-end examples of creating and executing Plutus scripts on chain.

This is done roughly in the following steps:

1. Write your Plutus **on chain** code.
2. Serialize your Plutus on chain code to the text envelope format (`cardano-cli` expects this format).
3. Create your transaction with the accompanying Plutus script(s).
4. Submit transaction to execute Plutus script.

# plutus-alwayssucceeds

This directory contains sources for a Plutus script that "always succeeds"

# plutus-helloworld

This directory contains a few "Hello World" script examples. There are three versions: the latest addition uses bytestring literal. The other two are using an integer literal and a bytestring parameter (both are alternative workarounds for when the Plutus interpreter didn't yet accept byte string literals).

# plutus-deadline

This directory contains some timelocking examples. There are three versions: [Deadline.hs](./plutus-deadline/src/Cardano/PlutusDeadline/Deadline.hs) is a validation script using a constant posix time deadline, [DeadlinePolicy.hs](./plutus-deadline/src/Cardano/PlutusDeadline/DeadlinePolicy.hs) is a minting policy with a constant posix time deadline, and [DeadlineRedeemer.hs](./plutus-deadline/src/Cardano/PlutusDeadline/DeadlineRedeemer.hs) is a minting policy that applies the numeric redeemer as the posix time deadline.

## FAQ

### Where is the off chain code?

The off chain code is used for transaction construction. In this case we construct the transaction with `cardano-cli` and therefore we don't need to write any off chain code.

### Where can I learn about Plutus scripts in more detail?

Our education director, Lars Brünjes, has an excellent series of [tutorials](https://youtu.be/IEn6jUo-0vU) on youtube. We will not attempt to provide an indepth explanation of Plutus in this repository.
