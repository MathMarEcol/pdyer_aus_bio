configure_parallel <- function(default_clustermq = TRUE, future_plan = future.callr::callr){
  ## Needed by targets::tar_make_future()
  ## Update, must ONLY be used by tar_make_future()
  ## Need to know if there is a flag or check,
  ## will add if there is.
  ## future::plan(future_plan)

  scheduler <- Sys.getenv("SCHEDULER")
  if (scheduler != "") {
      options(
        clustermq.scheduler = scheduler
      )
  } else {
      options(
        clustermq.scheduler = "LOCAL"
      )
  }


  switch(scheduler,
 "multiprocess" = {
      options(
        clustermq.defaults = list(log_file = "cmq_worker_%i.log"),
      )
			future::plan(future_plan, workers = as.numeric(Sys.getenv("FUTURE_WORKERS", "1")))
   },
  "pbs" = {
      options(
        ## Created by drake_hpc_template_file("pbs_clustermq.tmpl") and modified:
        clustermq.template = file.path(rprojroot::find_root(is_targets_project), "..", "shell", "pbs_clustermq.tmpl"),
        clustermq.defaults = list(work_dir =  getwd(),
                                  memory =  Sys.getenv("WORKER_MEM", "20GB"),
                                  cores = Sys.getenv("WORKER_CORES", "1"),
                                  log_file = "cmq_worker_${PBS_JOBID}_${PBS_ARRAY_INDEX}.log",
                                  runtime =  Sys.getenv("WORKER_RUNTIME", "7-00:00:00"))
      )
  },

  "slurm" = {
      options(
        ## Created by drake_hpc_template_file("pbs_clustermq.tmpl") and modified:
        clustermq.template = file.path(rprojroot::find_root(is_targets_project), "..", "shell", "slurm_clustermq.tmpl"),
        clustermq.defaults = list(work_dir =  getwd(),
                                  memory =  Sys.getenv("WORKER_MEM", "20GB"),
                                  cores = Sys.getenv("WORKER_CORES", "1"),
                                  log_file = "cmq_worker_%j_%a.log",
                                  runtime =  Sys.getenv("WORKER_RUNTIME", "7-00:00:00"))
      )
  },
 NA
 )

}
