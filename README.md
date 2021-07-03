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
[Matrix](https://matrix.to/#/!AqFOGENiPssQgsjxfE:matrix.org?via=matrix.org) or
[freenode (#carnap)](irc://irc.freenode.net/carnap).

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

Building on Windows natively is not supported, however, Carnap works well in
the [Windows Subsystem for Linux](https://aka.ms/wsl), and has been tested and
confirmed working in [WSL 2](https://aka.ms/wsl2). The steps for Linux work
as-written in WSL 2.

Nix is used to speed up builds and avoid having to compile dependencies,
instead using cached built versions from nixpkgs. It also makes it easy to
[Dockerize](https://nixos.org/nixpkgs/manual/#sec-pkgs-dockerTools) (see also
NixOps) and implement CI.

Carnap has a GHCJS (client side) and a GHC (server side) part.

Building Carnap-Server depends on the client side components being available at
`client-out` or otherwise linked into its static folder. This will be handled
automatically by the approaches to building the client listed below, but the
client needs to be built before the server.

Nix does not support incremental package builds, so the suggested development
workflow is to use `cabal` in a shell. A `Makefile` has been provided to make
this easier by shortening the commands needed to be typed.

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
components and tools isolated from those available on the host system.

**NOTE**: Building the client side, which is done in the `nix-shell` invocation
the first time, takes about 12GB of memory. If you would like to use a built
version from CI, pass `--arg useClientFromCi true` to `nix-shell` and
`nix-build` (if you are building a server to run).

You can run the server from a fresh checkout with:

```
# we use -j4 here to use 4 jobs which will go faster, but potentially use more
# RAM to compile. If it hits an out of memory condition, consider reducing this
# to `-j1`.

$ nix-shell -j4

# copy the default settings and make a dataroot (you only have to do this once)
[nix-shell]$ cp Carnap-Server/config/settings{-example,}.yml
[nix-shell]$ mkdir dataroot

# build a client (only has to be done once; you may want to use the option to
# grab it from CI)
[nix-shell]$ nix-build -A client -o client-out

# to just run the server
[nix-shell]$ make run

# this is the fastest way to check for compile errors without actually compiling
[nix-shell]$ cabal repl Carnap-Server

# you can also build it into dist-newstyle if you wish...
[nix-shell]$ make build-ghc
```

You can alternatively do this manually with cabal:

<details>
<summary>
Steps to do it manually with cabal
</summary>
<pre><code>
$ nix-shell
[nix-shell:Carnap]$ cabal new-build -f dev all
[nix-shell:Carnap]$ cd Carnap-Server
[nix-shell:Carnap-Server]$ APPROOT="http://localhost:3000" DATAROOT="../dataroot" BOOKROOT="../Carnap-Book/" cabal run -f dev Carnap-Server
</code></pre>
</details>

Then go to http://localhost:3000 and login with an arbitrary email address.
After logging in, go to http://localhost:3000/admin_promote to become an
administrator.

Now that you are an administrator, you can become an instructor at
https://localhost:3000/master_admin.


### I'd like to work on the client side proof checker

As in the section above, this will make a shell with Haskell development tools
and the required dependencies to work on Carnap components. This one provides
the GHCJS tooling and dependencies for the client side.

Using `make` aliases:

```
$ make shell-ghcjs
[nix-shell:Carnap]$ make build-ghcjs
```

Manually:

```
$ nix-shell --arg ghcjs true
$ cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
```

### Server development: Haskell Language Server

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

### Working on documentation locally

The documentation is available using the `/srv/` route, which can also be used
locally. If you want to do this, you can make a symbolic link like the following:

```
carnap$ ln -s /path/to/your/clone/of/Carnap-Documentation ./dataroot/srv
```

You can then browse to `http://localhost:3000/srv` to have an immediate preview
of the documentation as you work on it.

--------------------------

## Other development workflows

### Build a release version of Carnap-Server with Nix

This package is what is included in the Docker image and is used by the [UBC
NixOps scripts](https://github.com/ubc-carnap-team/carnap-nixops). It is
intended for production use and has compiler optimizations and Google login
enabled.

It will build more slowly than `make run` from above, and is thus probably not
what you want.

This will also automatically build the client and any dependencies if
necessary.

```
$ nix-build -j4 -A server -o server-out
```

### Building Docker images

If you'd like to build an image locally for development/testing, run:

```
nix-build release.nix -A docker -o docker-out
```

then load it into the docker daemon with:

```
docker image load -i docker-out
```

It will be available under the image name `carnap:latest`.

The docker building setup does not require KVM support in the host kernel, and
if you wish to build your images on a machine without it, run nix-build with
the added argument `--arg hasKvm false`.


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
