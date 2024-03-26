# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
## Must contain the following controller names:
## - "small" - needs 1 core and <4GB RAM
## - "gpu" - needs a GPU.
##   -Falls back to BLAS multithreading, if no nvidia gpu, get lots of cores
## - "multicore" - needs many cores and lots of ram
## - "ram" - needs 1 core and lots of RAM
## - "smallram" tuned for nbclust, about 50GB ram and 1 core

## Matches names in aus_bio_submit.sh
host_trunc <- regmatches(R.utils::System$getHostname(), regexpr(pattern = "(^prime-ai|^bun)", R.utils::System$getHostname()))

r_alias <- paste(
  sep = "\n",
  "shopt -s expand_aliases",
  "alias R='srun nix develop github:PhDyellow/nix_r_dev_shell/dc0d948b1fd6c49bd4ba4c61e86ce90b19b37e30#devShells.x86_64-linux.r-shell -c $NIX_GL_PREFIX R'"
)

ccg <- switch(host_trunc,
  "prime-ai" = {
    crew::crew_controller_group(
      crew.cluster::crew_controller_slurm(
        name = "small",
        workers = 20,
        seconds_timeout = 10,
        seconds_idle = 10,
        seconds_wall = 22 * 60 * 60, # 1 day
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = 4,
        slurm_cpus_per_task = 1,
        slurm_time_minutes = 24 * 60,
        slurm_partition = "cpu"
      ),
      crew.cluster::crew_controller_slurm(
        name = "gpu",
        workers = 1,
        seconds_timeout = 10,
        seconds_idle = 10,
        seconds_wall = 22 * 60 * 60, # 1 day
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          "#SBATCH --gpus=nvidia:1",
          "export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export BLIS_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export TF_FORCE_GPU_ALLOW_GROWTH='true'",
          "export CUDA_MODULE_LOADING=LAZY",
          "export TENSOR_CPU_MEM_MAX=50000000000", #Certain operations that do bulk operations over matricies will batch to keep RAM usage within this amount (in bytes)
          "export TENSOR_GPU_MEM_MAX=4500000000", #Certain operations that do bulk operations over matricies will batch to keep GPU memory usage within this amount (in bytes)
          "export TENSOR_DEVICE=CUDA", # Set to CUDA to attempt to use nvidia graphics card, any other value will use CPU
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = 50,
        slurm_cpus_per_task = 1,
        slurm_time_minutes = 24 * 60,
        slurm_partition = "gpu"
      ),
      crew.cluster::crew_controller_slurm(
        name = "multicore",
        workers = 1,
        seconds_timeout = 10,
        seconds_idle = 10,
        seconds_wall = 22 * 60 * 60, # 1 day
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          "export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export BLIS_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = 5.5,
        slurm_cpus_per_task = 9,
        slurm_time_minutes = 24 * 60,
        slurm_partition = "cpu"
      ),
      crew.cluster::crew_controller_slurm(
        name = "ram",
        workers = 1,
        seconds_timeout = 10,
        seconds_idle = 10,
        seconds_wall = 22 * 60 * 60, # 1 day
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          "export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export BLIS_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export TENSOR_CPU_MEM_MAX=55000000000",
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = 5.5,
        slurm_cpus_per_task = 9,
        slurm_time_minutes = 24 * 60,
        slurm_partition = "cpu"
        ),
      crew.cluster::crew_controller_slurm(
        name = "smallram",
        workers = 1,
        seconds_timeout = 10,
        seconds_idle = 10,
        seconds_wall = 22 * 60 * 60, # 1 day
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          "export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export BLIS_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export TENSOR_CPU_MEM_MAX=55000000000",
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = 5.5,
        slurm_cpus_per_task = 9,
        slurm_time_minutes = 24 * 60,
        slurm_partition = "cpu"
      )
    )
  },
  "bun" = {
    crew::crew_controller_group(
      crew.cluster::crew_controller_slurm(
        name = "small",
        host = R.utils::System$getHostname(),
        workers = 100,
        seconds_timeout = 100,
        seconds_idle = 60,
        seconds_wall = 22 * 60 * 60, # 1 day
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          paste0("#SBATCH --account=", Sys.getenv("SLURM_JOB_ACCOUNT")),
          "export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export BLIS_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = 8,
        slurm_cpus_per_task = 1,
        slurm_time_minutes = 24 * 60 + 10,
        slurm_partition = "general"
      ),
      crew.cluster::crew_controller_slurm(
        name = "gpu",
        host = R.utils::System$getHostname(),
        workers = 3,
        seconds_timeout = 100,
        seconds_idle = 60,
        seconds_wall = 22 * 60 * 60, # 24 hours
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          paste0("#SBATCH --account=", Sys.getenv("SLURM_JOB_ACCOUNT")),
          "#SBATCH --gpus=l40:1",
          "#SBATCH --nodes=1",
          "#SBATCH --ntasks-per-node=1",
          "#SBATCH --mem=800G",
          "export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export BLIS_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export TF_FORCE_GPU_ALLOW_GROWTH='true'",
          "export CUDA_MODULE_LOADING=LAZY",
          "export TENSOR_CPU_MEM_MAX=190000000000", #Certain operations that do bulk operations over matricies will batch to keep RAM usage within this amount (in bytes)
          "export TENSOR_GPU_MEM_MAX=20000000000", #Certain operations that do bulk operations over matricies will batch to keep GPU memory usage within this amount (in bytes)
          "export TENSOR_DEVICE=CUDA", # Set to CUDA to attempt to use nvidia graphics card, any other value will use CPU
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = NULL,
        slurm_cpus_per_task = 36,
        slurm_time_minutes = 24 * 60 + 10,
        slurm_partition = "gpu_cuda"
      ),
      crew.cluster::crew_controller_slurm(
        name = "multicore",
        host = R.utils::System$getHostname(),
        workers = 20,
        seconds_timeout = 100,
        seconds_idle = 60,
        seconds_wall = 164 * 60 * 60, # 1 week
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          paste0("#SBATCH --account=", Sys.getenv("SLURM_JOB_ACCOUNT")),
          "#SBATCH --nodes=1",
          "#SBATCH --ntasks-per-node=1",
          "#SBATCH --mem=1200G",
          "export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export BLIS_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export TENSOR_CPU_MEM_MAX=990000000000",
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = NULL,
        slurm_cpus_per_task = 100, ## Most efficient use of CPU workers if
        ## this number divides
        ## gf_bootstrap_iters evenly
        slurm_time_minutes = 164 * 60 + 10,
        slurm_partition = "general"
      ),
      crew.cluster::crew_controller_slurm(
        name = "ram",
        host = R.utils::System$getHostname(),
        workers = 50,
        seconds_timeout = 100,
        seconds_idle = 60,
        seconds_wall = 164 * 60 * 60, # 1 week
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          paste0("#SBATCH --account=", Sys.getenv("SLURM_JOB_ACCOUNT")),
          "#SBATCH --nodes=1",
          "#SBATCH --ntasks-per-node=1",
          "#SBATCH --mem=300G",
          "export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export BLIS_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export TENSOR_CPU_MEM_MAX=299000000000",
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = NULL,
        slurm_cpus_per_task = 10,
        slurm_time_minutes = 164 * 60 + 10, # 1 week
        slurm_partition = "general"
        ),
      crew.cluster::crew_controller_slurm(
        name = "smallram",
        host = R.utils::System$getHostname(),
        workers = 600,
        seconds_timeout = 100,
        seconds_idle = 60,
        seconds_wall = 164 * 60 * 60, # 1 week
        reset_globals = FALSE,
        garbage_collection = TRUE,

        ## This is sufficient to make runnable slurm workers
        script_lines = paste(
          sep = "\n",
          paste0("#SBATCH --account=", Sys.getenv("SLURM_JOB_ACCOUNT")),
          "#SBATCH --nodes=1",
          "#SBATCH --ntasks-per-node=1",
          "#SBATCH --mem=55G",
          "export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export BLIS_NUM_THREADS=$SLURM_CPUS_PER_TASK",
          "export TENSOR_CPU_MEM_MAX=5000000000",
          r_alias
        ),
        slurm_log_output = file.path(Sys.getenv("LOGDIR", "."), "crew_log_%A.txt"),
        slurm_log_error = file.path(Sys.getenv("LOGDIR", "."), "crew_log_error_%A.txt"),
        slurm_memory_gigabytes_per_cpu = NULL,
        slurm_cpus_per_task = 1,
        slurm_time_minutes = 164 * 60 + 10, # 1 week
        slurm_partition = "general"
      )
    )
  }
)
