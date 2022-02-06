# GOADA Stake Pool
![CI](https://github.com/goadacsp/goada-plutus/actions/workflows/test.yml/badge.svg?branch=main)


This project contains all the open source Plutus code created by the GOADA Stake Pool.

## Setting up

### VSCode devcontainer

Use the provided VSCode devcontainer to get an environment with the correct tools set up.

- Install Docker
- Install VSCode
  - Install the [Remote Development extension pack](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack)
  - You do *not* need to install the Haskell extension
- Ensure you have a `~/.cabal/packages` folder. You can create this via `mkdir -p ~/.cabal/packages`; it's used to cache cabal packages.
- Clone this repository and open it in VSCode
  - It will ask if you want to open it in the container, say yes.
  - The first time it will take a few minutes to download the devcontainer image from dockerhub,
  - `cabal build` from the terminal should work (unless you didn't have a `~/.cabal` folder, in which case you'll need to run `cabal update` first.)
  - Opening a Haskell file should give you IDE features (it takes a little while to set up the first time)

Note: This uses the [plutus-starter-devcontainer image on dockerhub](https://hub.docker.com/r/inputoutput/plutus-starter-devcontainer), if
you wish to build the image yourself, you can do so as follows:
  - Clone https://github.com/input-output-hk/plutus,
  - Set up your machine to build things with Nix, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!),
  - Build and load the docker container: `docker load < $(nix-build default.nix -A devcontainer)`,
  - Adjust the `.devcontainer/devcontainer.json` file to point to your local image.

### Cabal+Nix build

Alternatively, use the Cabal+Nix build if you want to develop with incremental builds, but also have it automatically download all dependencies.

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
$ nix-shell
```

Otherwise, you can use [direnv](https://github.com/direnv/direnv) which allows you to use your preferred shell. Once installed, just run:

```
$ echo "use nix" > .envrc # Or manually add "use nix" in .envrc if you already have one
$ direnv allow
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor.
See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.

## The GOADA Plutus policy script

Open VSCode with devcontainer and run this command in the terminal:

```sh
[devcontainer]$ cabal update && cabal build
```

Once the project was built you can create the policy script running:

```
[devcontainer]$ cabal run goada-plutus-app 85724832a16308a6c13e93a953d55d60b7d1b23e1e72052d4ca1bb10 1642807859000
Up to date
Creating the plutus policy script
Created the plutus policy script => minting-policy-85724832a16308a6c13e93a953d55d60b7d1b23e1e72052d4ca1bb10-1642807859000.plutus
```

Please note that the given date is in millisecond:

If you want ten minutes from now you can use ```$(expr $(date +"%s") + $(expr 60 \* 10))000```

And see the produced plutus cbor:

```sh
[devcontainer]$ cat minting-policy-85724832a16308a6c13e93a953d55d60b7d1b23e1e72052d4ca1bb10-1642807859000.plutus
```

```json
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "590d31590d2e0100...483c37353bf3d7c1"
}
```

## Build uplc to plofile the script

Clone the plutus repository

```sh
git clone https://github.com/input-output-hk/plutus.git
```

Install nix [as described here](https://nixos.org/download.html)

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Enter nix-shell

```sh
cd plutus
nix-shell
```

Inside nix-shell

```sh
cd plutus-core
cabal update
cabal build
```

Test uplc

```sh
cabal run uplc
```

FIXME: TO BE COMPLETED

## Support/Issues/Community

If you're looking for support, or would simply like to report a bug, feature
request, etc please do so over on the main [goada plutus repository](https://github.com/goadacsp/goada-plutus).

Thanks!
