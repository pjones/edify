{
  description = "A Markdown processor/compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      # List of supported systems:
      supportedSystems = [ "x86_64-linux" ];

      # Function to generate a set based on supported systems:
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Attribute set of nixpkgs for each system:
      nixpkgsFor = forAllSystems (system:
        import nixpkgs { inherit system; });

      # Run-time dependencies:
      deps = system:
        import nix/deps.nix { pkgs = nixpkgsFor.${system}; };
    in
    {
      packages = forAllSystems
        (system:
          let
            pkgs = nixpkgsFor.${system};
            hlib = pkgs.haskell.lib;
            haskell = pkgs.haskellPackages;
          in
          {
            # Full Haskell package with shared/static libraries:
            lib = haskell.callCabal2nix "edify" self { };

            # Just the edify executable:
            bin =
              haskell.generateOptparseApplicativeCompletions [ "edify" ]
                (hlib.justStaticExecutables
                  (self.packages.${system}.lib.overrideAttrs (orig: {
                    buildInputs =
                      (orig.buildInputs or [ ])
                      ++ [ pkgs.makeWrapper ];

                    # Wrap the edify executable so it can access all of the run-time
                    # dependencies:
                    postInstall =
                      (orig.postInstall or "")
                      + ''
                        mkdir -p "$out/wrapped"
                        mv "$out/bin/edify" "$out/wrapped/edify"

                        makeWrapper "$out/wrapped/edify" "$out/bin/edify" \
                          --prefix PATH : "${nixpkgs.lib.makeBinPath (deps system)}"
                      '';
                  })));

            # Default package:
            default = self.packages.${system}.bin;
          });

      overlays.default = final: prev: {
        pjones = (prev.pjones or { }) //
          { edify = self.packages.${prev.system}.bin; };
      };

      devShells = forAllSystems (system: {
        default = nixpkgsFor.${system}.haskellPackages.shellFor {
          packages = _: [ self.packages.${system}.lib ];
          withHoogle = true;
          buildInputs = with nixpkgsFor.${system}; [
            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
          ] ++ deps system;
        };
      });
    };
}
