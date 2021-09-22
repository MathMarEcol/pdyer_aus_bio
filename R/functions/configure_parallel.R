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

  if(scheduler == "pbs") {
      options(
        ## Created by drake_hpc_template_file("pbs_clustermq.tmpl") and modified:
        clustermq.template = file.path(rprojroot::find_root(is_targets_project), "..", "shell", "pbs_clustermq.tmpl")
      )
  }

  if (scheduler == "slurm") {
      options(
        ## Created by drake_hpc_template_file("pbs_clustermq.tmpl") and modified:
        clustermq.template = file.path(rprojroot::find_root(is_targets_project), "..", "shell", "slurm_clustermq.tmpl")
      )
  }
}
