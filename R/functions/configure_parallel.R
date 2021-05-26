configure_parallel <- function(default_clustermq = TRUE, future_plan = future.callr::callr){
  ## Needed by targets::tar_make_future()
  future::plan(future_plan)

  ## Needed by targets::tar_make_clustermq()
  ## By default, will use PBS if detected,
  ## multiprocess if PBS is not detected,
  ## and LOCAL if future::availableCores fails
  if(default_clustermq) {
    if(system2("qstat") == 0 ){
      ##We are in a system where qstat is present and working
      options(
        clustermq.scheduler = "PBS",
        ## Created by drake_hpc_template_file("pbs_clustermq.tmpl") and modified:
        clustermq.template = here::here("code", "pbs_clustermq.tmpl")
      )
    } else {
      ##no access to qstat/qsub, run multicore/serial
      options(
        clustermq.scheduler = "multiprocess"
      )
      workers <- future::availableCores(which="all", na.rm=FALSE) ## number of cores, leave one for master
      jobs <- max(workers, na.rm=TRUE)
      if (jobs <= 0) {
        options(
          clustermq.scheduler = "LOCAL"
        )
      }
    }
  }
}
