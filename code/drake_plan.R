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

         surv = target( split_surv(combined_copepod, matching),         ##supplying the initial splitting of the data here
                       ## How to use transform parameters
                       ## the map() function steps through "rows of a grid"
                       ## the nth target uses the nth entry of each object in map()
                       ## so the second target below uses names = "cpr" and matching = "CPR"
                       ## If you want all combinations, see cross().
                       ##
                       ## Notice that variables passed to split_surv() can come from all
                       ## different places in drake_plan().
                       ## combined_copepod is another target
                       ## matching is provided by the tranform = map(...) just below.
                       ## within map(), .id is provided by another parameter from map()
                       ## In the surv_epi target map(), .id is using the names object from
                       ## the surv target map().
                       ## So most map() params can be hard coded or targets, but .id must be
                       ##assigned another map() param
                       transform = map(
                         names = c("nrs", "cpr", "mkinnon", "goc", "nyan", "anita"),
                         .id = names,
                         matching = list(nrs = "NRS",
                                         cpr = "CPR",
                                         mckinnon = as.character(c(4, 5, 7, 9, 12, 15, 16, 24)), #McKinnon surveys
                                         goc = "1", #Gulf of Capentaria
                                         nyan = "21", #SE Tasmania
                                         anita = "18") #Tasmania data
                         )
                       ),

         #From now on, every call to surv should give me one survey at a time.
         surv_epi = target(
           remove_meso(surv, depth = epi_depth),
           transform = map(
             surv,
             .id = names
           )
         )

         #Keep going, but get some outputs eventually
         )

#' Make
drake::make(pl)

drake::vis_drake_graph(drake_config(pl), file = "../outputs/drake_graph.html", selfcontained = TRUE, hover = TRUE,)
drake::sankey_drake_graph(drake_config(pl), file = "../outputs/drake_graph_sankey.html", selfcontained = TRUE)
ggsave(filename = "../outputs/drake_ggplot.png", drake::drake_ggraph(drake_result))

