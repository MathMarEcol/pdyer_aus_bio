#' Libraries
##Drake
library(drake)
## Tidyverse
library(purrr)
##Parallel
library(future)
library(furrr)
##Analysis
library(cluster)
library(readr)
library(tidyverse)

#' Custom Functions
split_surv <- function(combined_copepod, matching){
  return(dplyr::filter(combined_copepod, PROJECT_ID %in% matching))
}

remove_meso <- function(surv, depth){
  dplyr::filter(surv, SAMPLE_DEPTH < 200 | is.na(SAMPLE_DEPTH))
}

## state<-rutilities::track_all_states()
## seed <- 20190703
## set.seed(seed)

## I am taking my code from here:
## [[file:/vmshare/phd/projects/aus_bioregions_paper/experiments/2019-06-28-1618_gf_models_kmeans/method_copepod.Rmd][file:/vmshare/phd/projects/aus_bioregions_paper/experiments/2019-06-28-1618_gf_models_kmeans/method_copepod.Rmd]]
#' Plan

## Set up variables


#This should really be a drake plan too!
## env_data <- archivist::areadLocal("eca58d95fc8d82de9750864ad2c82adf", "../2019-09-05-0908_clip_env/archivist")

pl <- drake::drake_plan(
               ##parameters
               epi_depth = 200,
               freq_range = c(0.05, 1),
               min_occurrence = 6,

               cov_min = 1.0,
               gf_trees = 500,
               gf_bins = 201,
               gf_corr_thres = 0.5,
         ##here I have referred to a variable defined above, copepod_csv
         ##copepod_csv is just a string, which will be passed to read_csv.
         ##first, I wrap the string inside file_in, so that drake knows it is a filename,
         ##that I read from the file, and that the file should be tracked.
         ##files cannot be stored in variables, must be a string.
               combined_copepod = readr::read_csv(file_in("../../../../Q1215/AusCPR/combined_copeped_jul19.csv"),
                                             na = c("(null)", "."),
                                             col_types = readr::cols(PROJECT_ID = col_character(),
                                                                     SAMPLE_DEPTH = col_number()),
                                             ),
         ##I had a lambda (unnamed) function here, but moved it to the
         ##custom funtion section

         ##drake_plan() forces you to put commas everywhere, this is not an R block.

         surv = target( split_surv(combined_copepod, proj),         ##supplying the initial splitting of the data here
            transform = map(proj = proj_grid$matching, .id = proj_grid$names)
         ),

         #From now on, every call to surv should give me one survey at a time.
         surv_epi = remove_meso(surv)

         #Keep going, but get some outputs eventually
         )

#' Make
drake::make(pl)

drake::vis_drake_graph(drake_config(pl), file = "../outputs/drake_graph.html", selfcontained = TRUE, hover = TRUE,)
drake::sankey_drake_graph(drake_config(pl), file = "../outputs/drake_graph_sankey.html", selfcontained = TRUE)
ggsave(filename = "../outputs/drake_ggplot.png", drake::drake_ggraph(drake_result))

