{ pkgs ? import ./nixpkgs.nix
}:

# Import library functions from nixpkgs:
with pkgs.lib;

let

  # The edify package:
  edify = pkgs.haskellPackages.callPackage ../edify.nix { };

  # All build dependencies:
  buildDeps = with pkgs; [
    inkscape      # For SVG -> PDF
    graphviz_2_32 # For DOT -> PDF
    mscgen        # For MSC -> PDF

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

    # For packaging:
    zip

  ] ++ [ edify ];

  # Helper function to copy files to a destination:
  copyFiles = files: dest:
    optionalString (length files > 0) ''
      cp -rpv ${concatStringsSep " " files} ${dest}
    '';

  # Helper function to build a course:
  mkDerivation =
    { name         ? "generic-course"
    , courses      ? "courses/*.md"
    , extraFiles   ? [ "LICENSE" "README.md" ]
    , phases       ? [ "unpackPhase" "buildPhase" "installPhase" ]
    , buildInputs  ? [ ]
    , buildPhase   ? ""
    , installPhase ? ""
    , ...
    }@args: pkgs.stdenv.mkDerivation (args // {
      inherit phases;

      buildInputs = buildDeps ++ buildInputs;

      buildPhase = ''
        echo "==> edify ${courses}"
        edify build --top "$(pwd)" ${courses}
        ${buildPhase}
      '';

      installPhase = ''
        dest=$out/${name}
        mkdir -p $dest/handouts -p $dest/slides

        # Copy extra files to $dest:
        ${copyFiles extraFiles "$dest"}

        # Copy PDF files into the correct locations:
        find build -type f -name '*.handout.pdf' -exec cp '{}' $dest/handouts ';'
        find build -type f -name '*.slides.pdf'  -exec cp '{}' $dest/slides ';'

        # Rename PDF files:
        for file in $dest/{handouts,slides}/*.pdf; do
          mv $file $(echo $file | sed -E 's/[.](handout|slides)[.]pdf/.pdf/')
        done

        ${installPhase}

        # Build archives:
        ( cd $out && zip -9 -y -r -q ${name}.zip ${name} )
      '';
    });

in { inherit pkgs mkDerivation; }
