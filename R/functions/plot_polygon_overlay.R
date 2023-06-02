plot_polygon_overlay <- function(
                                 extrap_polygons,
                                 env_poly,
                                 gfbootstrap_combined,
                                 plot_description,
                                 output_folder
                                 ) {

    ## Permutations of env_domain, trophic, survey, depth_cat, clust_method.

    ## env_domain is not mixed

    ## All has a different meaning here to gfbootstrap_combined.
    ## Do I actually want all crosses?
    ## Not even combinations. I want sets of rows that "make sense"
    ## a better verb is "across"
    ## "across" auz_eez fittings
    ## "across" auz_eez epi fittings

    ## Think of R array notation: [1, , 3]
    ## Blanks means "take all of these"

    ## I think I can permute over the levels for each dimension, with an added
    ## "" level, which means take all of these.

    trophic_levels <- list(unique(extrap_polygons$trophic))
    trophic_levels$any <- c(unique(extrap_polygons$trophic))

    poly_groups <- data.table::CJ(trophic = trophic_levels,
                   survey = survey_levels,
                   depth_cat = depth_cat_levels,
                   clust = clust_method_levels,
                   env_domain = env_domain_levels)

    ## For each row in poly_groups, get the subset of
    ## extrap_polygons that match.
    furrr::future_walk(seq.int(nrow(poly_groups)),
                       function(r, poly_groups, extrap_polygons, env_poly, output_folder) {
                           polys <- extrap_polygons[
                               env_domain %in% poly_groups$env_domain[r][[1]] &
                               trophic %in% poly_groups$trophic[r][[1]] &
                               survey %in% poly_groups$survey[r][[1]] &
                               depth_cat %in% poly_groups$depth_cat[r][[1]] &
                               clust %in% poly_groups$clust[r][[1]] &
                               !is.na(polygons),
                               ]
                           if (nrow(polys) > 1) {
                               poly_name <- make_poly_name(poly_groups[r,], output_folder, plot_description)
                               save_polys_overlays(polys, env_poly, poly_name)
                           }
                       }, poly_groups = poly_groups,
                           extrap_polygons = extrap_polygons,
                       env_poly = env_poly,
                       output_folder = output_folder)
}

make_poly_name <- function(poly_groups, output_folder, plot_description) {

    poly_l <- lapply(poly_groups, function(x) {length(x[[1]])})
    poly_groups[ , poly_l] <- list(c("any"))

    title <- glue::glue("Overlay of cluster polygons for domain {env_domain}\n",
               "for trophic levels [{trophic}], survey [{survey}],\n",
               "depth [{depth_cat}] and clustering technique [{clust}]")

    filename <- glue::glue("{env_domain}_{trophic}_{survey}_{depth_cat}_{clust}_{plot_description}.png")
    file_out <- file.path(output_folder, filename)
    return(list(title = title, file_out = file_out))

}

save_polys_overlays <- function(polys, env_poly, poly_name ) {

    ## Take all valid polygons
    ## Plot them onto the map with transparency
    ## Observe trends

    domain <- polys$env_domain

    n_models <- nrow(polys)

    env_bbox <-  sf::st_bbox(env_poly[name == domain, data][[1]],
                             crs = "+proj=longlat +datum=WGS84")

    pl_tm <- tm_shape(env_poly[name == domain, data][[1]], bbox = env_bbox) +
        tm_borders(lwd = 1, col = "black")

    for (poly in polys$polygons) {
        pl_tm + tm_shape(poly, bbox = env_bbox) +
            tm_borders(lwd = 0.5, col = "darkblue", alpha = 1/n_models)
    }

    pl_tm + title(poly_name$title)

    tmap_save_wrapper(tm = pl_tm, filename = poly_name$file_out, scale = 0.1, dpi = 1200)

}
