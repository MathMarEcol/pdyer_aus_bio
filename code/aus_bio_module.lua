--------------------------------------------------------------------------------------------------------------------
-- Singularity container for Aus_bio project
  --------------------------------------------------------------------------------------------------------------------
  whatis(" ")
  whatis("Singularity container for Aus_bio project")
whatis("philip.dyer1@uqconnect.edu.au")
whatis("20200122 ")
whatis("For more detail, run ")
whatis(" ")

setenv("SINGULARITY_BIND", os.getenv("TMPDIR") ..
         ",/gpfs1/scratch/30days:/30days" ..
         ",/gpfs1/scratch/90days:/90days" ..
         ",/gpfs1/groups:/groups" ..
         ",/gpfs1/sw1:/sw" ..
         ",/gpfs1/sw7:/sw7" ..
         ",/QRISdata"

       )
--setenv("SINGULARITYENV_PREPEND_PATH", "/opt/pbs/bin")

load("singularity/3.5.0")
singularity = "/sw/Containers/singularity/bin/run_singularity"
aus_bio_sif = "/90days/uqpdyer/rdm_mirror/Q1216/pdyer/pdyer_aus_bio/code/ywlmr8r5qmyiyhghh4zxa4skgcllc7m9-singularity-image-r-singularity-aus_bio.img" --as recommended by RCC, 90days for references, and input data files

-- Run Rscript on stdin
execute{cmd="shopt -s expand_aliases", modeA={"all"}}
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
