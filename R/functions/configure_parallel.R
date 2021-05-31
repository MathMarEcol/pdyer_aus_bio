configure_parallel <- function(default_clustermq = TRUE, future_plan = future.callr::callr){
  ## Needed by targets::tar_make_future()
  future::plan(future_plan)

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

  if(scheduler == "pbs") {
      options(
        ## Created by drake_hpc_template_file("pbs_clustermq.tmpl") and modified:
        clustermq.template = here::here("code", "shell", "pbs_clustermq.tmpl")
      )
  }
  if (scheduler == "slurm") {
      options(
        ## Created by drake_hpc_template_file("pbs_clustermq.tmpl") and modified:
        clustermq.template = here::here("code", "shell", "slurm_clustermq.tmpl")
      )
  }
}
