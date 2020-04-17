--------------------------------------------------------------------------------------------------------------------
-- Singularity container for Aus_bio project
  --------------------------------------------------------------------------------------------------------------------
  whatis(" ")
  whatis("Singularity container for Aus_bio project")
whatis("philip.dyer1@uqconnect.edu.au")
whatis("20200122 ")
whatis("For more detail, run ")
whatis(" ")

setenv("SINGULARITY_BIND", os.getenv("TMPDIR") .. ",/opt/pbs,/sw7") 


load("singularity/3.5.0")
singularity = "/sw/Containers/singularity/bin/run_singularity"
aus_bio_sif = "/30days/uqpdyer/Q1216/pdyer/pdyer_aus_bio/code/gqy77ym0w7hzpk37h4y40mhknfprsz63-docker-image-r-singularity-aus_bio_singularity_conversion.sif"
-- Run Rscript on stdin
set_alias("Rscript", singularity .. " exec " .. aus_bio_sif ..  " Rscript")
set_alias("R", singularity .. " exec " .. aus_bio_sif .. " R --vanilla" )
set_alias("drake_build", singularity .. " exec " .. aus_bio_sif .. " Rscript --vanilla" )
set_alias("shell", singularity .. " shell " .. aus_bio_sif )
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
