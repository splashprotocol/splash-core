{
  description = "WhalePools Cardano Dex Contracts";

  inputs = {
    # general inputs 
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-upstream.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # NOTE: ideally we would upgrade to the most recent iohk haskell.nix but currently 
    #       protolude doesn't build, throwing an obscure error. Until that's fixed we're 
    #       relying on an older version of haskell.nix 
    haskell-nix.follows = "plutarch/haskell-nix";
    haskell-nix-extra-hackage.url = "github:mlabs-haskell/haskell-nix-extra-hackage";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "plutarch/nixpkgs";
    };

    ouroboros-network = {
      url = "github:input-output-hk/ouroboros-network/cb9eba406ceb2df338d8384b35c8addfe2067201";
      flake = false;
    };

    ekg-json = {
      url = "github:vshabanov/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
      flake = false;
    };

    hw-aeson = {
      url = "github:sevanspowell/hw-aeson/b5ef03a7d7443fcd6217ed88c335f0c411a05408";
      flake = false;
    };

    optparse-applicative = {
      url = "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };

    io-sim = {
      url = "github:input-output-hk/io-sim/57e888b1894829056cb00b7b5785fdf6a74c3271";
      flake = false;
    };

    cardano-base = {
      url = "github:input-output-hk/cardano-base/0f3a867493059e650cda69e20a5cbf1ace289a57";
      flake = false;
    };

    goblins = {
      url = "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };

    flat = {
      url = "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };

    cardano-ledger = {
      url = "github:input-output-hk/cardano-ledger/c7c63dabdb215ebdaed8b63274965966f2bf408f";
      flake = false;
    };

    cardano-prelude = {
      url = "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };

    typed-protocols = {
      url = "github:input-output-hk/typed-protocols/181601bc3d9e9d21a671ce01e0b481348b3ca104";
      flake = false;
    };

    cardano-crypto = {
      url = "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };

    ply = {
      url = "github:mlabs-haskell/ply/staging";
      inputs.extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };


    # onchain inputs
    # NOTE: we could now remove the `staging` part as all the
    #       flake should stay compatible with the newest `master`
    #       since the release of plutarch 1.2
    plutarch.url = "github:Plutonomicon/plutarch/staging";

    # offchain inputs
    # NOTE: this is the pendant to what was specified in the `cabal.project` file
    #       We just want the directory at the specific commit, so we set `flake = false`
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix/9a604d01bd4420ab7f396f14d1947fbe2ce7db8b";
      flake = false;
    };

    # v1.0.0-alpha1
    plutus-apps = {
      url = "github:input-output-hk/plutus-apps/19e1e6cf0e567c0222d723b57438e9a8efa878fb";
      flake = false;
    };

    win32-network = {
      url = "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };

    plutus = {
      url = "github:input-output-hk/plutus/a56c96598b4b25c9e28215214d25189331087244";
      flake = false;
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , nixpkgs-upstream
    , haskell-nix
    , haskell-nix-extra-hackage
    , plutarch
    , pre-commit-hooks
    , ...
    }:
    let
      # GENERAL

      plainNixpkgsFor = system: import nixpkgs-upstream { inherit system; };

      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      # NOTE: This adds 
      # - a shellHook (pre-commit-check.shellHook) attribute you can just add to your shell with the // operator 
      #   if you add the shellHook, in your shell a new command becomes available "`pre-commit`" which you can run 
      #   by hand with `pre-commit run --all` and gets run automatically before each commit
      # - a check (pre-commit-check) that will check formatting according to what you specified
      # - for further tooling this makes available please refer to https://github.com/cachix/pre-commit-hooks.nix
      #   (it is also quite easy to add tools by yourself)
      preCommitCheckFor = system:
        pre-commit-hooks.lib.${system}.run
          {
            src = ./.;
            hooks = {
              cabal-fmt.enable = false;
              stylish-haskell.enable = false;
              nixpkgs-fmt.enable = false;
              # FIXME: this is currently not part of the pre-commit-hooks
              #        as the hlint currently is not happy.
              #        Apart from that the below tools are useful but not in the 
              #        current set of used tools
              hlint.enable = false;
              statix.enable = false;
              markdownlint.enable = false;
              shellcheck.enable = false;
            };

            tools = {
              hlint = (plainNixpkgsFor system).haskellPackages.hlint_3_4_1;
            };
          };

      haskellToolsForPkgs = pkgs: [
        pkgs.hlint
        pkgs.cabal-install
        pkgs.stylish-haskell
        pkgs.ghcid
        pkgs.haskellPackages.cabal-fmt
      ];

      # ONCHAIN
      # everything that is part of the onchain project; 
      # we have to use ghc923+ because all other ghc versions are unsupported by 
      # plutarch
      onchain' = rec {
        nixpkgsFor = system: import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay (import "${plutarch.inputs.iohk-nix}/overlays/crypto") ];
        };

        ghcVersion = "923";
        compiler-nix-name = "ghc" + ghcVersion;

        myhackages = system: compiler-nix-name: haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name
          [
            "${plutarch}"
            "${plutarch}/plutarch-extra"
            "${plutarch}/plutarch-test"
            "${inputs.ply}/ply-core"
            "${inputs.ply}/ply-plutarch"
          ];

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = plainNixpkgsFor system;
            hackages = myhackages system compiler-nix-name;
            modules = [
              ({ _, ... }: {
                packages.protolude.flags.dev = true;
              })
            ] ++ hackages.modules;

          in
          pkgs.haskell-nix.cabalProject' (plutarch.applyPlutarchDep pkgs {
            src = ./.;
            inherit compiler-nix-name modules;
            inherit (hackages) extra-hackages extra-hackage-tarballs;
            cabalProjectFileName = "cabal.project.onchain";
            shell = {
              shellHook = ''
                ln -fs cabal.project.onchain cabal.project
              '' + (preCommitCheckFor system).shellHook + "
              export LC_CTYPE=C.UTF-8
              export LC_ALL=C.UTF-8
              export LANG=C.UTF-8
              ";
              withHoogle = true;
              exactDeps = true;
              nativeBuildInputs = (haskellToolsForPkgs pkgs') ++ [
                pkgs'.nixpkgs-fmt
                plutarch.project.${system}.hsPkgs.hspec-discover.components.exes.hspec-discover
                (plutarch.hlsFor compiler-nix-name system)
              ];
            };
          });
      };
    in
    rec {
      # NOTE: this is convenient for use in nix repl
      inherit plainNixpkgsFor;

      onchain = rec {
        # NOTE: this is the only place where the outputs use 
        #       the "onchain'" attributeset defined in the let
        project = perSystem onchain'.projectFor;
        flake = perSystem (system: project.${system}.flake { });
      };

      packages = perSystem (system:
        onchain.flake.${system}.packages
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        tooling =
          let
            pkgs = plainNixpkgsFor system;
          in
          pkgs.mkShell {
            inherit (preCommitCheckFor system) shellHook;
            nativeBuildInputs = (haskellToolsForPkgs pkgs) ++ [
              pkgs.fd
              pkgs.nixpkgs-fmt
            ];
          };
      });

      checks = perSystem (system:
        self.onchain.${system}.flake.checks
        // { formatCheck = preCommitCheckFor system; }
      );

      hydraJobs = {
        inherit (self) checks packages devShells;
      };
    };
}