#%Module1.0
module-whatis "Singularity container for Aus_bio project"
module-whatis "philip.dyer1@uqconnect.edu.au"
module-whatis "20200122"

# as recommended by RCC, /scratch for references, and input data files
set aus_bio_sif  "/data/uqpdyer/resources/hpc_scratch/r-singularity-aus-bio.img"

if {[info exists env(TMPDIR)]} { set bindpath $env(TMPDIR) } else { set bindpath "" }
append bindpath ",/data"
setenv SINGULARITY_BIND $bindpath
setenv SINGULARITYENV_APPEND_PATH "/home/$env(USER)/bin"

system "shopt -s expand_aliases"

set-alias Rscript [concat "singularity exec "  $aus_bio_sif   " Rscript" ]
set-alias R [concat "singularity exec "  $aus_bio_sif  " R --vanilla"  ]
set-alias drake_build [concat "singularity exec "  $aus_bio_sif  " Rscript --vanilla"  ]
set-alias shell [concat "singularity shell "  $aus_bio_sif  ]


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
