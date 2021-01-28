let sources = import ./sources.nix;
in
{ pkgs ? import sources.nixpkgs { }
}:

with pkgs; [
  inkscape # For SVG -> PDF
  graphviz_2_32 # For DOT -> PDF
  mscgen # For MSC -> PDF
  imagemagick # For image manipulation (mostly PDF -> PNG)

  # Markdown -> PDF:
  pandoc
  haskellPackages.pandoc-citeproc
  haskellPackages.pandoc-crossref

  # TeX:
  (texlive.combine {
    inherit (texlive)
      beamer
      collection-binextra
      pgf
      scheme-small
      standalone
      xcolor-solarized;
  })
]
