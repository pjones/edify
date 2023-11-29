{ pkgs }:

with pkgs; [
  inkscape # For SVG -> PDF
  graphviz # For DOT -> PDF
  mscgen # For MSC -> PDF
  imagemagick # For image manipulation (mostly PDF -> PNG)

  # Markdown -> PDF:
  pandoc
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
