library(drake)
cache_dir <- "/vmshare/PARA/resources/hpc_sandbox/Q1216/pdyer/pdyer_aus_bio/drake_cache/"
cache_ob <- drake::drake_cache(cache_dir)
loadd(env_round,
      zooplank_cast_sub_full_clust_ind,
      marine_map,
      env_poly,
      cache = cache_ob)

## wdpar is not working on NixOS, due to issues with wdman and phantomjs, which installs binaries for me.
## library(wdpar)
## aus_mpa <- wdpa_fetch("Australia", wait = TRUE)

aus_eez <- marine_map[marine_map$Country == "Australia",]
## Using preexisting work from RDM
library(terra)
mpas <- terra::vect("/vmshare/PARA/resources/hpc_sandbox/Q1215/MPAs_v2020")
mpas_au <- mpas[mpas$PARENT_ISO == "AUS",]
mpas_au_sf <- st_as_sf(mpas_au)
library(tidyverse)
clust_locations <- left_join(data.frame(x_row = seq.int(1, nrow(env_round)), env_round[, c("lon", "lat")]), zooplank_cast_sub_full_clust_ind)

clust_vect <-terra::vect(clust_locations[,-1])
clust_vect_sf <- st_as_sf(clust_vect)
st_crs(clust_vect_sf) <- st_crs(aus_eez)
library(sf)

plot(clust_vect, "cl", col = rainbow(10))
plot(mpas_au)

## Clip clust locations to aus_eez, then count
clip_clust<-clust_vect_sf[lengths(st_intersects(clust_vect_sf, aus_eez)) >0,]


clust_counts <-table(clip_clust$cl)
## Clip mpas_au_sf
#clip_mpa <- st_intersection(mpas_au_sf, aus_eez)


## Finally, clip clusters to clipped mpas, and count again.

clust_in_mpa <- clip_clust[lengths(st_intersects(clip_clust, mpas_au_sf)) > 0,]
clust_counts_mpa <-table(clust_in_mpa$cl)

clust_ratio <- clust_counts_mpa / clust_counts[-9] ## cluster 9 has 0 coverage


library(tmap)
tmap_save(qtm(mpas_au_sf), "mpas.png")
pl <-tm_shape(clip_clust) +
  tm_dots("cl", style = "cat", size = 0.5,palette = rainbow(10))
tmap_save(pl, "clusters_eez.png")
pl <-tm_shape(clust_in_mpa) +
  tm_dots("cl", style = "cat", size = 0.5,palette = rainbow(10))
tmap_save(pl, "clusters_mpas.png")
library(ggplot2)
library(ggthemes)
clust_ratio_df <- as.data.frame(clust_ratio)
clust_ratio_df$Var1 <- as.numeric(as.character((clust_ratio_df$Var1)))
clust_ratio_df <- rbind(clust_ratio_df, c(9, 0))
pl <-ggplot(clust_ratio_df, aes(x = Var1, y = Freq)) + geom_bar(stat= "identity") +theme_tufte() + ylim(0,1)

ggsave("ratio.png", pl)
