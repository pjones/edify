{
  description = "A Markdown processor/compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
  };

  outputs = { self, nixpkgs, ... }:
    let
      # List of supported systems:
      supportedSystems = [ "x86_64-linux" ];

      # Function to generate a set based on supported systems:
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Attribute set of nixpkgs for each system:
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });

      # Run-time dependencies:
      deps = system: import nix/deps.nix { pkgs = nixpkgsFor.${system}; };
    in
    {
      packages = forAllSystems
        (system:
          let
            pkgs = nixpkgsFor.${system};
            hlib = pkgs.haskell.lib;

            haskell = pkgs.haskellPackages.override
              (orig: {
                overrides =
                  nixpkgs.lib.composeExtensions
                    (orig.overrides or (_: _: { }))
                    (self: super: {
                      aeson = super.aeson_1_5_2_0;
                      optparse-applicative = super.optparse-applicative_0_16_0_0;
                    });
              });
          in
          {
            # Full Haskell package with shared/static libraries:
            lib = haskell.callCabal2nix "edify" self { };

            # Just the edify executable:
            bin =
              hlib.generateOptparseApplicativeCompletion "edify"
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
          });

      defaultPackage =
        forAllSystems (system: self.packages.${system}.bin);

      overlay = final: prev: {
        pjones = (prev.pjones or { }) //
          { edify = self.packages.${prev.system}.bin; };
      };

      devShell = forAllSystems (system:
        nixpkgsFor.${system}.haskellPackages.shellFor {
          packages = _: [ self.packages.${system}.lib ];
          withHoogle = true;
          buildInputs = with nixpkgsFor.${system}; [
            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
          ] ++ deps system;
        });
    };
}
