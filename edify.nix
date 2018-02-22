{ mkDerivation, attoparsec, base, containers, data-default
, directory, fgl, filepath, Graphalyze, mtl, network-uri
, optparse-applicative, pandoc, pandoc-types, parsec, process
, semigroups, shake, stdenv, tasty, tasty-hunit, text, transformers
}:
mkDerivation {
  pname = "edify";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    attoparsec base containers data-default directory fgl filepath
    Graphalyze mtl network-uri optparse-applicative pandoc pandoc-types
    parsec process semigroups shake text
  ];
  executableHaskellDepends = [
    base directory filepath optparse-applicative pandoc pandoc-types
    shake text transformers
  ];
  testHaskellDepends = [ base containers tasty tasty-hunit text ];
  homepage = "https://github.com/pjones/edify";
  description = "Markdown (via pandoc) processing and content tools for Devalot.com";
  license = stdenv.lib.licenses.bsd3;
}
