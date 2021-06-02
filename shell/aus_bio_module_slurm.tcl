#%Module1.0
module-whatis "Singularity container for Aus_bio project"
module-whatis "philip.dyer1@uqconnect.edu.au"
module-whatis "20200122"

set aus_bio_sif  "/data/uqpdyer/resources/hpc_scratch/r-singularity-aus-bio.img" # as recommended by RCC, /scratch for references, and input data files

setenv SINGULARITY_BIND  [concat {getenv TMPDIR} \
                              ",/data" \
                         ]
setenv SINGULARITYENV_APPEND_PATH "/home/uqpdyer/bin"

system "shopt -s expand_aliases"

set_alias Rscript [concat "singularity exec "  $aus_bio_sif   " Rscript" ]
set_alias R [concat "singularity exec "  $aus_bio_sif  " R --vanilla"  ]
set_alias drake_build [concat "singularity exec "  $aus_bio_sif  " Rscript --vanilla"  ]
set_alias shell [concat "singularity shell "  $aus_bio_sif  ]


proc ModulesHelp { } {
puts stderr "  Modulefile to provide a bespoke R package set and system library."
puts stderr "  The application runs within a NixOS based container."
puts stderr ""
puts stderr "  Run"
puts stderr "  module load renv_aus_bio"
puts stderr "  R"
puts stderr "  Rscript -e "getwd()""
puts stderr "  cat  \"getwd()\" | Rscript  - "
puts stderr "  shell"
}


