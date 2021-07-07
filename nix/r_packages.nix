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
        targets
        rdtLite
        provSummarizeR
        provViz
        provParseR
        drake
        visNetwork
        networkD3

        #Maths and statistics
        emmix 
        # EMMIXcskew
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
        clustsig
        fpc



        #data sources
        sdmpredictors
        wdpar
        robis
        #seaaroundus #removed from CRAN
        ncdf4

        #simulated data
        coenocliner
        coenoflex
        MixSim

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
        tmap

        #Data manipulation
        tidyverse
        data_table
        sf
        sp
        rgeos
        rlist
        lutz
        lubridate
        ClimateOperators
        raster
        terra
        stars


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

        qs
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
        formatR
        r_import

        #vim support
        nvimcom
        languageserver
        lintr

        ##fixing RcppArmadillo
        RcppArmadillo

        #Prioritizr
        lwgeom
        rfishbase
        readr
        kader
        rgdal
        exactextractr
        VoCC
        prioritizr
        prioritizrdata
        gurobi
        slam
        rnaturalearth
        patchwork
        spatstat
        mapview
        remotes
        gridExtra
        lpsymphony
        #Rsymphony
        units
        reldist
        codetools

]
