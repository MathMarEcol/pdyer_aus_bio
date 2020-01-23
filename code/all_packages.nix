{pkgs}:
let
  rpackages = (import ./r_packages.nix { pkgs = pkgs; });
in
with pkgs;[
          #(stdenv.mkDerivation{
          #  name = "${R.name}-no-save";
          #  inherit (rWrapper.override {
          #      packages = [
          #      ] ++ rpackageslist;
          #    }) meta;
          #  nativeBuildInputs = [ makeWrapper ];
          #  buildCommand = ''
          #    mkdir -p $out/bin
          #    for item in ${R}/bin/*; do
          #      ln -s $item $out/bin/
          #    done
          #    wrapProgram $out/bin/R --add-flags "--no-save"
          #'';})

          # clang
          # rustc
          # cargo
          binutils
          coreutils

          pandoc #needed for rMarkdown

          #gnutar
          #gzip
          #gnumake
          #gcc
          #gawk
          #gnused
          #glibc
          #glibcLocales

          which #explicity include which that R compiled against, rather than fall back to system `which``, for some reason the Rshell which and system which are not identical

          #needs a shell in the container
          bashInteractive
          


          (rWrapper.override {
              packages = [
              ] ++ rpackages;
            })

          #don't need rStudio, won't run on server anyway
          # (rstudioWrapper.override {
          # packages = [
          # ] ++ rpackageslist;
          #   })
    ]
