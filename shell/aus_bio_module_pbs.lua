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
         ",/scratch/uqpdyer:/usrscratch" ..
         ",/scratch/uqpdyer" ..
         ",/scratch" ..
         ",/mnt/sw/legacy/sw1:/sw" ..
         ",/mnt/sw/legacy/sw7:/sw7" ..
         ",/QRISdata"

       )
setenv("SINGULARITYENV_APPEND_PATH", "/home/uqpdyer/bin")

load("singularity/3.5.0")
-- singularity = "/sw/Containers/singularity/3.5.0/bin/run_singularity"
aus_bio_sif = "/home/uqpdyer/r-singularity-aus-bio.img" --as recommended by RCC, /scratch for references, and input data files

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
