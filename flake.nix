{
  description = "argsem-soundness -- check an extension of an abstract argumentation framework against the Dung semantics";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # myam, the test framework, lives in pauldist.
    pauldist.url = "github:kisp/pauldist-nix-importer";
  };

  outputs = { self, nixpkgs, pauldist }:
    let
      # x86_64-linux only: that is what this is developed and built on.
      systems = [ "x86_64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (system:
        f nixpkgs.legacyPackages.${system} system);

      sbclFor = pkgs: pkgs.sbcl.withOverrides (pauldist.lib.overlayFor {
        lisp = pkgs.sbcl;
        inherit (pkgs) fetchzip;
      });
    in {
      checks = forAll (pkgs: system:
        let
          sbcl = sbclFor pkgs;
          run = { name, packages, script }:
            pkgs.runCommand name
              {
                nativeBuildInputs = [ (sbcl.withPackages packages) ];
                src = ./.;
              }
              ''
                cp -r "$src" ./argsem-soundness
                chmod -R u+w ./argsem-soundness
                export CL_SOURCE_REGISTRY="$PWD/argsem-soundness//"
                export HOME="$PWD"
                sbcl --script "./argsem-soundness/${script}"
                touch "$out"
              '';
        in {
          # The suite. The tests build graph objects, so they get GRAPH.
          tests = run {
            name = "argsem-soundness-tests";
            packages = ps: [ ps.alexandria ps.trivial-garbage ps.graph ps.myam ];
            script = "nix/run-tests.lisp";
          };

          # The library's own dependencies, and nothing else: GRAPH must not
          # be needed to load or to solve.
          without-graph = run {
            name = "argsem-soundness-without-graph";
            packages = ps: [ ps.alexandria ps.trivial-garbage ];
            script = "nix/run-without-graph.lisp";
          };
        });

      devShells = forAll (pkgs: system: {
        default = pkgs.mkShell {
          packages = [
            ((sbclFor pkgs).withPackages
              (ps: [ ps.alexandria ps.trivial-garbage ps.graph ps.myam ]))
          ];
          shellHook = ''export CL_SOURCE_REGISTRY="$PWD//"'';
        };
      });
    };
}
