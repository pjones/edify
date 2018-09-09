self: super:

let

  # Function to change build options for all Haskell packages:
  fixHaskell = base: { mkDerivation = args:
                         base.mkDerivation (args // { doHaddock = false;
                                                    });
                     };
in

{
  # Inject package changes to nixpkgs:
  haskellPackages = super.haskellPackages.override (orig:
    { overrides = super.lib.composeExtensions (orig.overrides or (_: _: {}))
                                                                 (_: fixHaskell);
    });
}
