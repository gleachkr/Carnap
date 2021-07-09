.PHONY: help devel run shell-ghc build-ghc shell-ghcjs build-ghcjs tags build-production hoogle
TARGET := all

CABALFLAGS := -f dev
APPROOT := "http://localhost:3000"
DATAROOT := "../dataroot"
BOOKROOT := "../books/Carnap-Book/"

help:
	@echo "Usage: make [options] [target]"
	@echo ""
	@echo "Supported targets:"
	@echo "    build-production    Build and bundle everything needed to run the server in production"
	@echo "    build-docker        Build a production docker container"
	@echo "    build-ghcjs         Build a development client JavaScript with GHCJS"
	@echo "    build-ghc           Build a development server with GHC"
	@echo "    shell-ghcjs         Enter a nix shell for GHCJS client development"
	@echo "    shell-ghc           Enter a nix shell for GHC server development"
	@echo "    devel               Build and run a development server locally, with data saved in this directory"
	@echo "    tags                Generate source code tags for development"
	@echo "    hoogle              Starts a hoogle server with Carnap-Server's dependencies"
	@echo ""
	@echo "For more usage instructions, see README."


build-production:
	@echo "building client..."
	nix-build -j4 -A client -o client-out
	@echo "building server..."
	nix-build -j4 -A server -o server-out
	@echo "creating bundle directory..."
	mkdir carnap-bundle
	@echo "copying files..."
	cp -r server-out/share/static carnap-bundle
	mkdir carnap-bundle/dataroot
	cp server-out/bin/Carnap-Server carnap-bundle
	chmod -R +w carnap-bundle
	@echo "compressing..."
	tar -hczf carnap-bundle.tar.gz carnap-bundle
	@echo "cleaning up..."
	rm -r carnap-bundle

build-docker:
	nix-build release.nix -A docker -o docker-out

devel: build-ghcjs run

APPROOT := "http://localhost:3000"
DATAROOT := "../dataroot"
BOOKROOT := "../Carnap-Book/"

run: client-out truth-tree-out
ifeq ($(origin NIX_STORE),undefined)
	nix-shell --run 'make run'
else
	cd Carnap-Server && \
	cp -n config/settings-example.yml config/settings.yml && \
	mkdir -p ../dataroot && \
	APPROOT=$(APPROOT) DATAROOT=$(DATAROOT) \
		BOOKROOT=$(BOOKROOT) \
		cabal run $(CABALFLAGS) Carnap-Server $(CONFIGFILE)
endif

# builds the client files
client-out:
	nix-build -A client -o client-out

# builds the truth tree module
truth-tree-out:
	nix-build -A truth-tree -o truth-tree-out

shell-ghc:
	nix-shell

build-ghc: client-out truth-tree-out
ifeq ($(origin NIX_STORE),undefined)
	nix-shell --run 'make build-ghc'
else
	cp -n Carnap-Server/config/settings-example.yml Carnap-Server/config/settings.yml && \
	cabal new-build -f dev $(TARGET)
endif

shell-ghcjs:
	nix-shell --arg ghcjs true

# I don't think I can easily enter a shell for the user if they forget unfortunately :(
build-ghcjs:
ifeq ($(origin NIX_STORE),undefined)
	nix-shell --arg ghcjs true --run 'make build-ghcjs'
else
	cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build $(TARGET)
	# make a fake nix output directory so we don't have to change the symlinks from a nix-built
	# client (allowing people working only on the server to download a client from nix cache)
	mkdir -p .cabal-fake-client-out/bin
	ln -rsf dist-ghcjs/build/*/ghcjs-*/Carnap-GHCJS-*/x/AllActions/build/AllActions/AllActions.jsexe .cabal-fake-client-out/bin/
	ln -sfn .cabal-fake-client-out client-out
endif

tags:
	nix-shell --run "hasktags -R ."

hoogle:
	nix-shell --run "hoogle server --local"
