# Carnap

Carnap is a free and open-source Haskell framework for creating and exploring
formal languages, logics, and semantics. It lets you quickly and
straightforwardly define languages, construct logics for those languages, and
stipulate their semantics. Carnap then uses your specifications to figure out
how to check proofs in a variety of formal systems using your logic, how to
find the meanings of compound expressions, and a whole lot more.

Carnap's primary application at the moment is powering
[Carnap.io](https://carnap.io), a website supporting online logic instruction
and learning. If  you'd like to seem some demos and more general information,
head over to [Carnap.io/about](https://carnap.io/about).

If you'd like to learn more about using the server as an instructor, head over
to the [documentation collection.](https://carnap.io/srv/doc). You can also
find documentation about administration-focused topics such as deployment and
server configuration there. Documentation is updated nightly from the
[Carnap-Documentation](https://github.com/Carnap/Carnap-Documentation)
repository, in case you'd like to contribute or suggest an edit.

If you'd like to discuss development and administration of Carnap instances in
real time, we have a chat room on
[Matrix](https://matrix.to/#/!AqFOGENiPssQgsjxfE:matrix.org?via=matrix.org),
bridged to [libera.chat in #carnap](ircs://irc.libera.chat:6697/carnap).

There is also a mailing list, primarily focused on instructor support, on
[Google Groups](https://groups.google.com/g/carnap-users).

If you're interested in contributing to software development or modifying the
software, read on. This README will help you set up a development environment
for building the server-side and client-side components used on carnap.io.

## Development

### General information

The current development environment is based on [Nix](https://nixOS.org). For
general background on Nix, take a look at
[nixos.org/learn.html](https://nixos.org/learn.html) or [nix.dev](https://nix.dev).

The Nix package manager runs on any Linux distribution or macOS and can be
installed without disrupting the rest of your system.

Building on Windows natively is not supported, however, Carnap works well in
the [Windows Subsystem for Linux](https://aka.ms/wsl), and has been tested and
confirmed working in [WSL 2](https://aka.ms/wsl2). The steps for Linux work
as-written in WSL 2.

Nix is used to speed up builds and avoid having to compile dependencies,
instead using cached built versions from the NixOS build farm. It also makes it
easy to [build lean Docker images](https://nixos.org/nixpkgs/manual/#sec-pkgs-dockerTools)
and implement CI.

Carnap has a GHCJS (client side) and a GHC (server side) part.

Nix does not support incremental package builds, so the suggested development
workflow is to use the standard Haskell build system `cabal` in a `nix-shell`.
A `Makefile` has been provided to make this easier by shortening the commands
needed to be typed. It will output the commands it runs, and you can also read
the `Makefile` itself for more details.

In this documentation, lines starting with `$` mean that the part after the `$`
is run in a shell natively on your system. Lines starting with `[nix-shell]$`
are intended to be run in a `nix-shell` shell, which will have that prompt.

### Nix setup

Install Nix with your *non-root* user account:

```
$ bash <(curl -L https://nixos.org/nix/install)
```

macOS 10.15 (Catalina) users may need to follow additional steps, documented at
[nixos.org/nix/manual/#sect-macos-installation](https://nixos.org/nix/manual/#sect-macos-installation).

You can significantly speed up builds by using binaries for Carnap dependencies
from Carnap's [Cachix](https://cachix.org/) instance, which has support for
both Linux and macOS. To use it:

```
$ # Install Cachix:
$ nix-env -iA cachix -f https://cachix.org/api/v1/install
$ # Use the Carnap Cachix
$ cachix use carnap
```

Then build as normal.

A quick aside (for those who are curious about what these commands are doing):
Nix, by default, reads `shell.nix` if you call it through `nix-shell`, and
`default.nix` if you call it with nix-build or others. That's why there are
attribute names being used with `-A` for the `nix-build` commands, but not for
`nix-shell`.

### I'd like to work on Carnap-Server

This creates a shell in an environment providing the required dependencies for
Carnap-Server, along with development tools such as `cabal`, `ghc` and others
appropriate for server side development. Nix shells use versions of Haskell
components and tools isolated from those available on the host system, sort of
like Python's `virtualenv`, but for Haskell packages, and in fact, the entire
machine.

```
$ nix-shell
[nix-shell:Carnap]$ # Run a development server
[nix-shell:Carnap]$ make run
```

You should then be able to access your development server at
`http://localhost:3000`.

#### Next steps

To become an instructor, log in with the dummy login using an email you'll
remember. Set your first name and last name. Then, go to
<http://localhost:3000/admin_promote> and press the button. Your account will
be promoted to administrator of the local instance.

From there, you can go to <http://localhost:3000/master_admin> and promote your
account to instructor. You can log out and log back in with another email if
you want to simulate a student, for example.

If you'd like to dig into the code,
[Haskell Language Server](#Haskell-Language-Server) is highly recommended.

### I'd like to work on the GHCJS client side components of Carnap

This process will replace the symlink to the Nix-built client automatically
built while working on the server with one to a `cabal`-built client, more
suitable for development. You can delete `client-out` and `make run` will give
you a Nix-built client again.

You can get a shell with a `ghcjs` compiler like so:

```bash
$ make shell-ghcjs
$ # This will invoke cabal to build the GHCJS components
[nix-shell:Carnap]$ make build-ghcjs
```

### I'd like to work on truth-tree/Rudolf

The truth tree components are built by Nix as well, in a nearly identical way
to the client. If you'd like to replace them with a locally built one, replace
the symlink with one pointing to a local checkout that's been built:

Say, you have a checkout of [the repo](https://github.com/ubc-carnap-team/Rudolf/)
at `~/dev/Rudolf` that you've already run `yarn install` and `yarn build-lib`
in, you'd want to run, in your Carnap directory:

```bash
Carnap$ ln -sfn ~/dev/Rudolf ./truth-tree-out
```

If you'd like to go back to the Nix-built truth tree, just delete the
`truth-tree-out` symlink.

### Special build outputs

#### Release builds

If you'd like to build a release build of the Nix package, suitable for
production, use:

```bash
$ nix-build -A server -o server-out
```

This builds the derivation at the attribute `server` of the attribute set
returned by the function at the top level of `default.nix`. You can also invoke
this function yourself if you want to use Carnap in a NixOps deployment, for
example:

```nix
let carnap-server = (import ./carnap/default.nix {}).server;
in {
  # use it
  # ...
}
```

Here's a sample of
[NixOps files using this for a deployment of Carnap at UBC](https://github.com/ubc-carnap-team/carnap-nixops/blob/49f1bf814d6e3466ca3ca1c1fc5e56fccbb5e76c/carnap.nix#L41-L73)

#### Building Docker images

If you'd like to build an image locally for development/testing, run:

```
$ nix-build release.nix -A docker -o docker-out
```

then load it into the docker daemon with:

```
$ docker image load -i docker-out
```

It will be available under the image name `carnap:latest`.

The docker building setup does not require KVM support in the host kernel, and
if you wish to build your images on a machine without it, run nix-build with
the added argument `--arg hasKvm false`.

#### Haskell Language Server

The Nix infrastructure for Carnap provides the Haskell Language Server by
default for development on Carnap-Server, allowing for completion, type
information and more. It is not yet working on the exclusively-GHCJS-built
components.

To use it:

Make the following shell script somewhere in `$PATH`, called `hls-nix`:

```sh
#!/bin/sh

nix-shell --run "haskell-language-server-wrapper $@"
```

Then, configure per the [information in the
README](https://github.com/haskell/haskell-language-server). For example, for
neovim with `Coc`, the following configuration should be put in the file
brought up by `:CocConfig`:

```json
{
"languageserver": {
  "haskell": {
    "command": "hls-nix",
    "args": ["--lsp"],
    "rootPatterns": ["*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"],
    "filetypes": ["haskell", "lhaskell"]
  }
}
}
```

NOTE: we are using the `hls-nix` wrapper script as the command here, so HLS
picks up the dependencies for Carnap-Server from Nix since it is run in a
`nix-shell`.

NOTE: `Carnap-Server/config/settings.yml` needs to be present for Carnap-Server to
work in HLS (just copy it from the example settings file).

#### Working on documentation locally

The documentation is available using the `/srv/` route, which can also be used
locally. If you want to do this, you can make a symbolic link like the following:

```
carnap$ ln -s /path/to/your/clone/of/Carnap-Documentation ./dataroot/srv
```

You can then browse to `http://localhost:3000/srv` to have an immediate preview
of the documentation as you work on it.

## Maintainer information

### CI requirements

CI requires that a Cachix signing key for the Cachix cache be supplied in the
`CACHIX_SIGNING_KEY` Secret in the GitHub repository settings for artifacts to
be pushed to Cachix.

### Updating nixpkgs

The version of nixpkgs used is pinned to a fixed git commit to ensure that no
unexpected issues happen with our builds. Occasionally it should be updated,
then Carnap build reattempted. Currently we use a system of two versions of
nixpkgs, `nixpkgs` and `nixpkgs-stable`, with `nixpkgs` used for Carnap-Server,
having the latest packages including haskell-language-server, and
`nixpkgs-stable` providing a working ghcjs (since it is supposedly broken in
the latest nixpkgs). `nixpkgs-stable` as of the time of this writing is pinned
to `nixos-20.03` whereas `nixpkgs` is on `nixpkgs-unstable`, a
tested-but-rolling-release build.

We now use [niv](https://github.com/nmattia/niv) to manage our nixpkgs
versions. It is available from either development shell.

Run `niv update nixpkgs` or `niv update nixpkgs-stable` to update the
respective pinned nixpkgs versions to the latest in their branch.

### Issues

Every instance of `doJailbreak` is disabling version checking in Cabal. Most of
the time this works fine, but it indicates bugs in the packages' Cabal
version restrictions.

Carnap currently uses an older ghcjs-dom from 2016, which is largely fine on
web but requires webkitgtk24x-gtk3, which was been removed from Nix in late
2019. It is thus currently not possible to build the Carnap front end natively
(i.e. for embedding in non-browser applications) pending this update.

A bunch of packages have broken unit tests on ghcjs. All tests on ghcjs are
accordingly disabled (also, for performance reasons).

### Suggested Nix Resources

Blog post on Carnap's nix implementation: https://lfcode.ca/blog/nix-and-haskell

Tutorial some of the Nix here is loosely based on:
https://github.com/Gabriel439/haskell-nix

User's guide to the Haskell infrastructure:
https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure

Source code to the Haskell infrastructure (has good doc comments):
https://github.com/NixOS/nixpkgs/tree/nixos-20.03/pkgs/development/haskell-modules

Basic information about the builder and e.g. how to build manually:
https://nixos.wiki/wiki/Nixpkgs/Create_and_debug_packages
