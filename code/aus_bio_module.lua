--------------------------------------------------------------------------------------------------------------------
-- Singularity container for Aus_bio project
  --------------------------------------------------------------------------------------------------------------------
  whatis(" ")
  whatis("Singularity container for Aus_bio project")
whatis("philip.dyer1@uqconnect.edu.au")
whatis("20200122 ")
whatis("For more detail, run ")
whatis(" ")

setenv("SINGULARITY_BIND", "$TMPDIR")

load("singularity/3.5.0")
-- Run Rscript on stdin
set_alias("Rscript","/sw/Containers/singularity/bin/run_singularity exec /30days/uqpdyer/Q1216/pdyer/pdyer_aus_bio/code/8kzygmabzzpzys35d728dri80r89rhws-docker-image-r-singularity-aus_bio_singularity_conversion.sif Rscript")
set_alias("R","/sw/Containers/singularity/bin/run_singularity exec /30days/uqpdyer/Q1216/pdyer/pdyer_aus_bio/code/8kzygmabzzpzys35d728dri80r89rhws-docker-image-r-singularity-aus_bio_singularity_conversion.sif R --vanilla" )
set_alias("drake_build","/sw/Containers/singularity/bin/run_singularity exec /30days/uqpdyer/Q1216/pdyer/pdyer_aus_bio/code/8kzygmabzzpzys35d728dri80r89rhws-docker-image-r-singularity-aus_bio_singularity_conversion.sif Rscript --vanilla" )
set_alias("shell","/sw/Containers/singularity/bin/run_singularity shell /30days/uqpdyer/Q1216/pdyer/pdyer_aus_bio/code/8kzygmabzzpzys35d728dri80r89rhws-docker-image-r-singularity-aus_bio_singularity_conversion.sif " )
help([[
  Modulefile to provide a bespoke R package set and system library.
  The application runs within a NixOS based container.

  Run
  module load renv_aus_bio
  R
  Rscript -e "getwd()"
  cat "getwd()" | Rscript - 
  shell

  ]])
