{ pkgs
  ? import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-01-20";
  url = https://github.com/nixos/nixpkgs-channels/;
  # Commit hash for nixos-unstable as of 2020-01-20
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/nixos-unstable"; #using bleeding edge packages
  rev = "bea1a232c615aba177e0ef56600d5f847ad3bbd9"; #fixed revision, latest release as of 2020-01-20

	}) { #the attributes to import
	overlays = [ 
		(import /vmshare/cust-nix/Rshell/packages/rpackages_overlay.nix)
	];
  }
}:

let
  name = "r-singularity-aus_bio";
      rpackageslist = with pkgs.rPackages; [#All  R packages that I use in my code.
        #SDMs and CLMS
        gradientforest
        #(pkgs.callPackage ./packages/multabund.nix {myRPackages = myRPackages;})

        #personal packages
        phil_rutilities
        phil_rmethods
        rphildyerphd

        #Image processing
        EBImage

        #provenance
        rdtLite
        provSummarizeR
        provViz
        provParseR
        drake

        #Maths and statistics
        emmix 
        EMMIXcskew
        #(pkgs.callPackage ./packages/emmix_mfa.nix {})
        mclust
        mixtools
        NbClust
        vegan
        fields
        anocva
        kernlab
        WGCNA
        apcluster
        tnet
        interp
        akima
        mvpart
 

        #data sources
        sdmpredictors
        robis
        seaaroundus
        raster

        #simulated data
        coenocliner
        coenoflex

        #Plotting
        ggthemes
        maptools
        ggplot2
        ellipse
        ggcorrplot
        ggforce
        gganimate
        plotly
        RColorBrewer
        R_devices

        #Data manipulation
        tidyverse
        data_table
        sf
        sp
        rgeos
        rlist


        #Parallel processing
        foreach
        doParallel
        doRNG
        future
        future_apply
        furrr
        doFuture
        randomForestSRC
        snow
        SOAR
        bigmemory
        ff
        (Rmpi.overrideDerivation(attrs:{
          configureFlags  = ["--with-Rmpi-include=${pkgs.openmpi}/include"
              "--with-Rmpi-libpath=${pkgs.openmpi}/lib"
              "--with-Rmpi-type=OPENMPI"];
        }))
        #doMPI #installation breaks, do to issues in mpi setup,
                #possibly because R is in a container but
                #nixos-update is called from the host OS
                #used install.packages()

        #Reproducible research
        R_cache #reduce re-evaluation of function calls
        archivist #store results, could be important for tracking outputs in JSON
        packrat

        fst
        feather
        jsonlite
        yaml
        mongolite

        #Support tools
        devtools
        roxygen2
        rmarkdown
        knitr
        microbenchmark
        assertthat
        testthat
        RUnit
        pryr
        profvis
        caTools
        qwraps2
        skimr
        janitor
        stringr
        

        #vim support
        nvimcom
		    languageserver
        lintr

        ##fixing RcppArmadillo
        RcppArmadillo

	];


   r-contents = with pkgs;[
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

          clang
          rustc
          cargo
          binutils
          coreutils

          #gnutar
          #gzip
          #gnumake
          #gcc
          #gawk
          #gnused
          #glibc
          #glibcLocales

          which #explicity include which that R compiled against, rather than fall back to system `which``, for some reason the Rshell which and system which are not identical

          #needs a shell
          bashInteractive
          

          (rWrapper.override {
              packages = [
              ] ++ rpackageslist;
            })

          #don't need rStudio, won't run on server anyway
          # (rstudioWrapper.override {
          # packages = [
          # ] ++ rpackageslist;
          #   })
    ];

in
with pkgs; dockerTools.buildImage { 
  #breaks because buildImage evals extraCommands BEFORE mkdir $out
  #in addition, the builder is isolated and I won't be able to look up the result
  #make singularity

  extraCommands = '' 
###stuff to make layer singularity compatible here

###
'';
  config = {
    Cmd = [];
    Entrypoint = [
      (writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}
  #    set -e
    #!${pkgs.stdenv.shell}
  #    exec /bin/bash
  ''
      )
    ];
  };
  contents = r-contents;
  name = name;
  }
