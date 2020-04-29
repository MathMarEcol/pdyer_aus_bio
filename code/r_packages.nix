{pkgs}:
with pkgs.rPackages; [#All  R packages that I use in my code.
        #SDMs and CLMS
        gradientforest
        #(pkgs.callPackage ./packages/multabund.nix {myRPackages = myRPackages;})

        #personal packages
        phil_rutilities
        phil_rmethods
        rphildyerphd
        gfbootstrap
        castcluster

        #Image processing
        EBImage

        #provenance
        rdtLite
        provSummarizeR
        provViz
        provParseR
        drake
        visNetwork
        networkD3

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
        clusterCrit
        Hotelling
        ICSNP
        rrcov
 

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
        ggraph
        maptools
        ggplot2
        ellipse
        ggcorrplot
        ggforce
        gganimate
        plotly
        RColorBrewer
        R_devices
        quantreg

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
        future_callr
        furrr
        doFuture
        randomForestSRC
        snow
        SOAR
        bigmemory
        ff
        clustermq 
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
        renv
        callr

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
        pryr
        profvis
        proftools
        profr
        caTools
        qwraps2
        skimr
        janitor
        stringr
        here

        #vim support
        nvimcom
		    languageserver
        lintr

        ##fixing RcppArmadillo
        RcppArmadillo

	]
