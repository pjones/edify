{ mkDerivation, base, containers, directory, filepath, mtl
, optparse-applicative, pandoc, pandoc-types, parsec, process
, semigroups, stdenv, tasty, tasty-hunit, text, transformers
}:
mkDerivation {
  pname = "edify";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory filepath mtl pandoc pandoc-types parsec
    process semigroups text
  ];
  executableHaskellDepends = [
    base directory optparse-applicative pandoc pandoc-types text
    transformers
  ];
  testHaskellDepends = [ base containers tasty tasty-hunit text ];
  homepage = "https://github.com/pjones/edify";
  description = "Markdown (via pandoc) processing and content tools for Devalot.com";
  license = stdenv.lib.licenses.bsd3;
}
