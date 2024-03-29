# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
#' Global objects that control the
#' behaviour of the analysis.
#' Each object is kept separate
#' so that changing one value
#' does not invalidate the entire
#' analysis every time.
#' If all parameters were
#' bundled together, one changed
#' parameter would count as all the
#' parameters changing for dependency
#' tracking.


#' Phytoplankton data params
phy_names = c(
  "nrs",
  "cpr",
  "brett",
  "other_1050",
  "other_1051",
  "other_1054",
  "other_1056",
  "other_1057",
  "other_1058",
  "other_1059",
  "other_1066",
  "other_479",
  "other_1068",
  "other_1067",
  "other_1069",
  "other_54",
  "other_591",
  "other_782",
  "other_786",
  "other_790",
  "other_796",
  "other_795",
  "other_806",
  "other_801",
  "other_805",
  "other_804",
  "other_807",
  "other_517",
  "other_519"
)
phy_matching = list(
  nrs = 599,
  cpr = 597,
  brett = 794,
  other_1050 = 1050,
  other_1051 = 1051,
  other_1054 = 1054,
  other_1056 = 1056,
  other_1057 = 1057,
  other_1058 = 1058,
  other_1059 = 1059,
  other_1066 = 1066,
  other_479 = 479,
  other_1068 = 1068,
  other_1067 = 1067,
  other_1069 = 1069,
  other_54 = 54,
  other_591 = 591,
  other_782 = 782,
  other_786 = 786,
  other_790 = 790,
  other_796 = 796,
  other_795 = 795,
  other_806 = 806,
  other_801 = 801,
  other_805 = 805,
  other_804 = 804,
  other_807 = 807,
  other_517 = 517,
  other_519 = 519
)

#' Zooplankton data params

zoo_names = c(
  "nrs",
  "cpr",
  "mckinnon",
  "goc",
  "nyan",
  "anita"
)
zoo_matching = list(
  nrs = 599,
  cpr = 597,
  mckinnon =
    c(4, 5, 7, 9, 12, 15, 16, 24), #McKinnon surveys
  goc = 1, #Gulf of Capentaria
  nyan = 21, #SE Tasmania
  anita = 18 #Tasmania data
)

#' Reg fish dataset params
fish_years = 2007:2017

description_to_survey =
  data.table::setDT(tibble::tribble(
    ~Descript, ~trophic,
    "bathydemersal 30 - 90 cm", "demersal",
    "bathydemersal <30 cm", "demersal",
    "bathydemersal >=90 cm", "demersal",
    "bathypelagic 30 - 90 cm", "pelagic",
    "bathypelagic <30 cm", "pelagic",
    "bathypelagic >=90 cm", "pelagic",
    "benthopelagic 30 - 90 cm", "pelagic",
    "benthopelagic <30 cm", "pelagic",
    "benthopelagic >=90 cm", "pelagic",
    "cephalopods", "cephalopods",
    "demersal 30 - 90 cm", "demersal",
    "demersal <30 cm", "demersal",
    "demersal >=90 cm", "demersal",
    "demersal mollusc", "demersal",
    "flatfish <90 cm", "demersal",
    "flatfish >=90 cm", "demersal",
    "krill", "arthropod",
    "lobsters crab", "arthropod",
    "pelagic 30 - 90 cm", "pelagic",
    "pelagic <30 cm", "pelagic",
    "pelagic >=90 cm", "pelagic",
    "rays <90 cm", "shark_ray",
    "rays >=90 cm", "shark_ray",
    "reef-associated 30 - 90 cm", "demersal",
    "reef-associated <30 cm", "demersal",
    "reef-associated >=90 cm", "demersal",
    "shark <90 cm", "shark_ray",
    "shark >=90 cm", "shark_ray",
    "shrimp", "arthropod"
  ))

#' Depth filtering params

depth_names = c(
  "epi",
  "meso",
  "bathy"
)

depth_range = list()

## Include lower bound, exclude upper bound d \in [a,b)
depth_range[[depth_names[1]]] <- c(0, 200)
depth_range[[depth_names[2]]] <- c(200, 1000)
depth_range[[depth_names[3]]] <- c(1000, Inf)

#' Species sampling filter params
freq_range = c(0.00, 1)
min_occurrence = 6
cov_min = -Inf
max_taxa = 2000

#' Site species abundance merge function
agg_fun <- mean

#' Environment params
spatial_vars = c("lon", "lat")
env_id_col = "env_id"
map_layer =  "World_EEZ_v8_2014_HR"
max_depth = 1500
env_offset = 0
## in lat lon degrees, use 1/integer fraction
## for proper rastering later,
## currently 1/12 to allign with BioORACLE
res_clust = c(1, 1 / 2, 1 / 4)
res_env = 1 / 12 # matches BioORACLE
res_gf = res_env # match res_env for now

##Extent chosen to match the largest extents of
##the Aus EEZ polygon and the FRDC benthic data
##FRDC is not being used, but previous effort
##has used this extent and the full sampling of the GoC is useful
env_bounds = list(
  x = c(109 + 1 / 24, 163 + 23 / 24),
  y = c(-47 - 23 / 24, -8 - 1 / 24)
)

#' MPA polygon params
country_code <- "AUS"
iucn_categories <- data.table::data.table(name = c("no_take", "all"),
                              categories = list(
                                  c("Ia", "Ib", "II"),
                                  c("Ia", "Ib", "II", "III", "IV", "V", "VI")))
marine_categories <- c("marine")

#' BioORACLE params
env_vars = c(
  "temp",
  "chlo",
  "salinity",
  "curvel"
)
env_modes = c(
  "min",
  "max",
  "mean",
  "range"
)

env_present_only = c("nitrate", "silicate", "iron")

## BioORACLE strings to match layer codes
env_year = c("2050", "2100")
env_pathway = c("RCP26", "RCP45", "RCP60", "RCP85")
env_bathy = "MS_bathy_5m"
## "present" is also always included
env_fitting <- "present"

#' Environmental clipping params
##For each predictor, I have specified limits.
##Not all variables hit the limits, shwon in comment
##The limits are generally around 3 standard deviations
##from the mean, unless specified in comments
env_limits_sd = 300
env_limits = list(
  ##no clipping
  "BO2_tempmin_ss" = c(0, 30),
  ##Log, within limits
  "BO2_nitratemin_ss" = c(-16, 3),
  ##Log, upper limits in SO and in a few coastal sites,
  ##lower limits off east coast Tas, ~2std.
  ##Setting scale to avoid clipping lower limits
  "BO2_silicatemin_ss" = c(-Inf, 2.5),
  ##Clipping upper extremes, mostly off southern
  ##Tas coast, ~1.7std
  "BO2_chlomin_ss" = c(0, 0.25),
  ##Log, clipping upper extremes, all coastal ~2.6std
  "BO2_ironmin_ss" = c(-Inf, -5.75),
  ##Upper extreme in Bight, clipped ~1.4std, lower
  ##extremes on tropical coasts ~2std
  "BO2_salinitymin_ss" = c(32.15, 35.72),
  ##Log, lower extreme in bight and other southern
  ##coasts, ~3std. No upper extremes
  "BO2_curvelmin_ss" = c(-3.32, Inf),
  ##no clipping
  "BO2_tempmax_ss" = c(8, 32),
  ##Log, no upper clipping due to lack of extremes,
  ##no lower clipping as almost all low values are
  ##deep water and don't look like outliers. Log transform
  ##brings out tropical patterns, without log, the SO
  ##gradient is prominent.
  "BO2_nitratemax_ss" = c(-13, 4),
  ##Log, no lower extremes, upper extremes are
  ##tropical coastal ~2std
  "BO2_silicatemax_ss" = c(0, 3.16),
  ## very long upper tail, mostly along south australian
  ##shelf, but not coastal ~2std
  "BO2_chlomax_ss" = c(0, 1.12),
  ##Log, no lower extremes, some upper extremes around
  ##PNG ~2.5std
  "BO2_ironmax_ss" = c(-11, -5.2),
  ##Upper and lower extremes along coasts ~2.5std
  "BO2_salinitymax_ss" = c(34.2, 36.6),
  ##Log, no upper extremes, lower extremes are randomly
  ##placed ~2.5std
  "BO2_curvelmax_ss" = c(-2.5, 0.5),
  ##No clipping
  "BO2_tempmean_ss" = c(6, 30),
  ##Log, no clipping, extremes are in deep waters in
  ##GoC but are part of the long tail
  "BO2_nitratemean_ss" = c(-14, 3.1),
  ##Log, no lower extremes just gradient into SO,
  ##upper extremes on tropical coasts
  "BO2_silicatemean_ss" = c(0.3, 2.8),
  ##Very long upper tail, some in tropical coasts,
  ##most along southern shelf edge and Tas ~2std
  "BO2_chlomean_ss" = c(0, 0.48),
  ##Log, no lower extremes, upper extremes are coastal,
  ##mostly in GoC and Bight
  "BO2_ironmean_ss" = c(-12, -5.6),
  ##extremes are all coastal
  "BO2_salinitymean_ss" = c(33.7, 36.1),
  ##Log, lower extremes are randomly placed, no upper extremes,
  "BO2_curvelmean_ss" = c(-4.7, 0),
  ##no lower extremes, upper extremes are coastal
  "BO2_temprange_ss" = c(0, 9.5),
  ##Log, no clipping, extremes are deep water in GoC
  "BO2_nitraterange_ss" = c(-14, 3),
  ##Log, no lower extremes, upper extremes are tropical
  ##coastal ~2std
  "BO2_silicaterange_ss" = c(-1.2, 2.65),
  ##very long upper tail, around Indonesia, south shelf
  ##edge of Australia and Tas ~2std
  "BO2_chlorange_ss" = c(0, 0.96),
  ##Log, lower extreme in SO, upper extreme in GoC ~2std
  "BO2_ironrange_ss" = c(-10.7, -6),
  ##no lower extreme, upper extremes are all tropical coastal
  ##~2std
  "BO2_salinityrange_ss" = c(0, 2.9),
  ##Log, no upper extreme, lower extremes are random ~2std
  "BO2_curvelrange_ss" = c(-4.6, 0.4),
  ##lower extremes in deep trences. no upper extremes
  "MS_bathy_5m" = c(-6000, 0)
)
##The following variables work better in log scale.
##If a variable is log transformed,
##clipping will take place on the log scale
env_log = c(
  "BO2_nitratemin_ss",
  "BO2_silicatemin_ss",
  "BO2_ironmin_ss",
  "BO2_curvelmin_ss",
  "BO2_nitratemax_ss",
  "BO2_silicatemax_ss",
  "BO2_ironmax_ss",
  "BO2_curvelmax_ss",
  "BO2_nitratemean_ss",
  "BO2_silicatemean_ss",
  "BO2_ironmean_ss",
  "BO2_curvelmean_ss",
  "BO2_nitraterange_ss",
  "BO2_silicaterange_ss",
  "BO2_ironrange_ss",
  "BO2_curvelrange_ss"
)

#' Gradient Forest params
gf_trees = 600
gf_bootstrap_iters = 600
cgf_bootstrap_combinations = 2000
gf_bins = 201
gf_corr_thres = 0.5
gf_compact = FALSE
extrap = 0


## Specific survey combinations for creating
## combined gfbootstrap objects
## not covered by combining all surveys
## along each "dimension" of trophic level,
## survey and depth.
## env_domains will always be mapped over
## and should not be specified here.
custom_combinations <- list()

## IMOS datasets
custom_combinations$imos <- list(
    descriptions = data.table::data.table(
      trophic = "plankton",
      depth_cat = "all",
      survey = "imos"
      ),
    ## Use CJ to avoid having to balance lengths
    ## Impossible combinations will be excluded
    ## by failing to match
    ## If CJ creates unwanted combinations, then just
    ## use data.table directly.
    matches = data.table::CJ(
      trophic = c("zoo", "phy"),
      survey = c("nrs", "cpr")
      )
)

custom_combinations$all_plankton <- list(
  descriptions = data.table::data.table(
    trophic = "plankton",
    depth_cat = "epi",
    survey = "all"
  ),
  matches = data.table::CJ(
    trophic = c("zoo", "phy"),
    depth_cat = c("epi")
  )
)





## Survey combinations to compare cluster counts
## As for `custom_combinations` needs a `matches`
## data.table. Matches are found from gfbootstrap_combined
## so custom surveys from `custom_combinations` and `all`
## are allowed for match.
## Also needs `descriptions`, but only set `compare_group`
## Env domains will always be mapped over
cluster_compare_methods = c("apclustdefault", "casterdefault")

surveys_for_cluster_compare <- list()

surveys_for_cluster_compare$all <- list(
    descriptions = data.table::data.table(
      compare_group = "universal"
      ),
    matches = data.table::CJ(
                              ## Simplest way to match all rows
      depth_cat = c(depth_names, "all")
    ))


surveys_for_cluster_compare$best_epi <- list(
    descriptions = data.table::data.table(
                                   compare_group = "best_epi"
                               ),
    matches = data.table::CJ(
                              survey = c("bac", "watson", "cpr"),
                              trophic = c("microbe", "pelagic", "zoo", "phy"),
                              depth_cat = c("epi")
                          ))

surveys_for_cluster_compare$best_all_depths <- list(
    descriptions = data.table::data.table(
                                   compare_group = "best_all_depths"
                               ),
    matches = data.table::CJ(
                              survey = c("bac", "watson", "cpr"),
                              trophic = c("microbe", "pelagic", "zoo", "phy")
                          ))



#' K-medoids params
min_clust_thres = 0.01

cluster_fixed_k = 41

k_range = c(2,300)
cluster_reps = seq.int(1,3)
cluster_reps_test = seq.int(1,10)

clara_samples = 20
clara_sampsize = 50
clara_trace = 0
clara_rngR = TRUE
clara_pamLike = TRUE
clara_correct.d = TRUE

pca_n_vars = 5
pca_scale = 1/20

nbclust_dist = "manhattan"
nbclust_method = "kmeans"


## NbClust has a GPL-2 licence

## NbClust Beale threshold
## Package defaults to 0.1
nbclust_alphabeale = 0.1

## Return the index of the best cluster
## Assumes scores are sorted from smallest k to largest
nbclust_custom_bestnc <- list(
  duda = function(clust_scores, crit_scores) {
    passes <- clust_scores >= crit_scores
    ind <- which(passes)
    if (length(ind) > 0) {
      return(min(ind))
    } else {
      return(NA)
    }
  },
  pseudot2 = function(clust_scores, crit_scores) {
    passes <- clust_scores <= crit_scores
    ind <- which(passes)
    if (length(ind) > 0) {
      return(min(ind))
    } else {
      return(NA)
    }
  },
  beale = function(clust_scores) {
    passes <- clust_scores >= nbclust_alphabeale ## closure
    ind <- which(passes)
    if (length(ind) > 0) {
      return(min(ind))
    } else {
      return(NA)
    }
  },
  frey = function(clust_scores) {
    passes <- clust_scores < 1
    ind <- which(passes)
    if (length(ind) > 0) {
      return(min(ind) - 1)
    } else {
      return(NA)
    }
  },
  gap = function(clust_scores, crit_scores) {
    passes <- 0 <= crit_scores
    ind <- which(passes)
    if (length(ind) > 0) {
      return(min(ind))
    } else {
      return(NA)
    }
  }
)
## serial indicates the index uses second order information
## between index scores to calculate the best cluster,
## and therefore must be run in serial
## If FALSE, then each k can be calculated in parallel easily using
## the optima_func and the vector of index scores.
## runtime is a rank of how quickly results are calculated,
## from 1 (fast) to 4 (very slow). Use to filter to keep
## runtime manageable.
## Fortunately, all serial indicies are also fast.
## Graphical methods require manual interpretation of a plot
## so are usually disabled
## crit methods need the crit scores for each k to find optima
nbclust_index_metadata = data.table::setDT(tibble::tribble(
                                     ~index, ~serial, ~runtime,  ~optima_func, ~crit, ~graphical,
                                     "kl", FALSE, 1, which.max, FALSE, FALSE,
                                     "ch", FALSE, 1, which.max, FALSE, FALSE,
                                     "ccc", FALSE, 1, which.max, FALSE, FALSE,
                                     "db", FALSE, 1, which.min, FALSE, FALSE,
                                     "silhouette", FALSE, 2, which.max, FALSE, FALSE,
                                     "duda", FALSE, 1, nbclust_custom_bestnc$duda, TRUE, FALSE,
                                     "pseudot2", FALSE, 1, nbclust_custom_bestnc$pseudot2, TRUE, FALSE,
                                     "beale", FALSE, 1, nbclust_custom_bestnc$beale, FALSE, FALSE,
                                     "ratkowsky", FALSE, 1, which.max, FALSE, FALSE,
                                     "ptbiserial", FALSE, 2, which.max, FALSE, FALSE,
                                     "gap", FALSE, 4, nbclust_custom_bestnc$gap, TRUE, FALSE,
                                     "frey", FALSE, 3, nbclust_custom_bestnc$frey, FALSE, FALSE,
                                     "mcclain", FALSE, 3, which.min, FALSE, FALSE,
                                     "gamma", FALSE, 4, which.max, FALSE, FALSE,
                                     "gplus", FALSE, 4, which.min, FALSE, FALSE,
                                     "tau", FALSE, 4, which.max, FALSE, FALSE,
                                     "dunn", FALSE, 1, which.max, FALSE, FALSE,
                                     "sdindex", TRUE, 1, which.min, FALSE, FALSE, ## uses max k
                                     "sdbw", FALSE, 2, which.min, FALSE, FALSE,
                                     "hartigan", TRUE, 1, NA, FALSE, FALSE,
                                     "scott", TRUE, 1, NA, FALSE, FALSE,
                                     "marriot", TRUE, 1, NA, FALSE, FALSE,
                                     "trcovw", TRUE, 1, NA, FALSE, FALSE,
                                     "tracew", TRUE, 1, NA, FALSE, FALSE,
                                     "friedman", TRUE, 1, NA, FALSE, FALSE,
                                     "rubin", TRUE, 1, NA, FALSE, FALSE,
                                     "cindex", TRUE, 1, NA, FALSE, FALSE,
                                     "ball", TRUE, 1, NA, FALSE, FALSE,
                                     "hubert", TRUE, 1, NA, FALSE, TRUE, ## Graphical
                                     "dindex", TRUE, 1, NA, FALSE, TRUE## Graphical
                                     ))

nbclust_max_runtime = 3
nbclust_include_graphical = FALSE


##Clustering settings
##Currently implemented clustering methods are "caster" and "apclust"
clust_methods = c("casteroptimal", "apclustoptimal", "casterdefault", "apclustdefaultmedian", "apclustdefaultmin", "casternonzero", "casterohfive", "casterohfivebonferroni", "casterquantile25", "casterquantile50", "casterquantile75", "casterquantile90", "casterquantile95")
clust_m = 4
min_gamma_tol = 0.005
min_range_tol = 0.001
keep_all_clusts = FALSE

# Limiting importance
# If an integer >= 1, then uses that number of predictors, sorted so that more
# important predictors are used first, in for clustering.
# If in range (0,1) then take as many predictors as needed to include that
# fraction of the importance. eg 0.8 maens keep 80% of importance.
                                        # To include all predictors, set to Inf
## Do not take n predictors to cover X amount of importance.
## This can easily obscure differences between surveys, because
## more predictors means more dimensions, in general similarity will
## go down as number of predictors goes up.
## 20 predictors is slightly below typical selection from my existing
## datasets.
pred_importance_top = Inf

is_targets_project <- rprojroot::root_criterion(function(path){
  testfile <- file.path(path,"_targets.R")
  file.exists(testfile)
}, "Find the _targets.R file")

plot_clust_labels <- FALSE
output_folder <- file.path(rprojroot::find_root(is_targets_project), "..", "..", "outputs")

max_clust_prob_plot <- 30
