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
freq_range = c(0.05, 1)
min_occurrence = 6
cov_min = 1.0
max_taxa = 2000

#' Environment params
spatial_vars = c("lon", "lat")
env_id_col = "env_id"
map_layer =  "World_EEZ_v8_2014_HR"
max_depth = 1500
env_offset = 0
##in lat lon degrees, use 1/integer fraction
##for proper rastering later,
##currently 1/12 to allign with BioORACLE
regrid_resolution = 1 / 4 #TODO: 1 / 12,
##Extent chosen to match the largest extents of
##the Aus EEZ polygon and the FRDC benthic data
##FRDC is not being used, but previous effort
##has used this extent and the full sampling of the GoC is useful
env_bounds = list(
  x = c(109 + 1 / 24, 163 + 23 / 24),
  y = c(-47 - 23 / 24, -8 - 1 / 24)
)

#' BioORACLE params
bio_oracle_str_template = "BO2_%s%s_ss"
env_vars = c(
  "depth",
  "temp",
  "nitrate",
  "silicate",
  "chlo",
  "iron",
  "salinity",
  "curvel"
)
env_modes = c(
  "min",
  "max",
  "mean",
  "range"
)

#' Environmental clipping params
##For each predictor, I have specified limits.
##Not all variables hit the limits, shwon in comment
##The limits are generally around 3 standard deviations
##from the mean, unless specified in comments
env_limits_sd = 3
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
gf_trees = 200
gf_bins = 201
gf_corr_thres = 0.5
gf_compact = FALSE
extrap = 1 / 4

#' K-medoids params
min_clust_thres = 0.01

k_range = seq.int(2,20)
cluster_reps = seq.int(1,3)
cluster_reps_test = seq.int(1,10)

clara_samples = 20
clara_sampsize = 50
clara_trace = 0
clara_rngR = TRUE
clara_pamLike = TRUE
clara_correct.d = TRUE
