plot_polygon_overlay <- function(
                                 extrap_polygons,
                                 env_poly,
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

    trophic_levels <- lapply(unique(extrap_polygons$trophic), c)
    trophic_levels$any <- c(unique(extrap_polygons$trophic))
    survey_levels <- lapply(unique(extrap_polygons$survey), c)
    survey_levels$any <- c(unique(extrap_polygons$survey))
    depth_cat_levels <- lapply(unique(extrap_polygons$depth_cat), c)
    depth_cat_levels$any <- c(unique(extrap_polygons$depth_cat))
    clust_method_levels <- lapply(unique(extrap_polygons$clust), c)
    clust_method_levels$any <- c(unique(extrap_polygons$clust))
    env_domain_levels <- lapply(unique(env_poly$name), c)

    depth_cat_levels <- lapply(depth_cat_levels, as.character)

    poly_groups <- data.table::CJ(trophic = trophic_levels,
                                  survey = survey_levels,
                                  depth_cat = depth_cat_levels,
                                  clust = clust_method_levels,
                                  env_domain = env_domain_levels,
                                  sorted = FALSE)



    ## For each row in poly_groups, get the subset of
    ## extrap_polygons that match.
    purrr::walk(seq.int(nrow(poly_groups)),
                       function(r, poly_groups, extrap_polygons, env_poly, output_folder, plot_description) {
                           polys <- extrap_polygons[
                               env_domain %in% poly_groups$env_domain[r][[1]] &
                               trophic %in% poly_groups$trophic[r][[1]] &
                               survey %in% poly_groups$survey[r][[1]] &
                               depth_cat %in% poly_groups$depth_cat[r][[1]] &
                               clust_method %in% poly_groups$clust[r][[1]] &
                               !is.na(polygons),
                               ]
                           if (nrow(polys) > 1) {
                               poly_name <- make_poly_name(poly_groups[r,], output_folder, plot_description)
                               save_polys_overlays(polys, env_poly, poly_name)
                           }
                       }, poly_groups = poly_groups,
                           extrap_polygons = extrap_polygons,
                       env_poly = env_poly,
                       output_folder = output_folder,
                       plot_description = plot_description)

}

make_poly_name <- function(poly_group, output_folder, plot_description) {

    poly_l <- vapply(poly_group, function(x) {
      length(x[[1]]) > 1
    }, logical(1))
    poly_group[, names(poly_group)[poly_l]] <- "any"

    title <- glue::glue_data(poly_group, "Overlay of cluster polygons for domain [{env_domain}]\n",
               "for trophic levels [{trophic}], survey [{survey}],\n",
               "depth [{depth_cat}] and clustering technique [{clust}]")

    filename <- glue::glue_data(poly_group, "{env_domain}_{trophic}_{survey}_{depth_cat}_{clust}_{plot_description}.png")
    file_out <- file.path(output_folder, filename)
    return(list(title = title, file_out = file_out))

}

save_polys_overlays <- function(polys, env_poly, poly_name ) {

    ## Take all valid polygons
    ## Plot them onto the map with transparency
    ## Observe trends

    domain <- polys$env_domain[[1]]

    n_models <- nrow(polys)

    env_bbox <-  sf::st_bbox(env_poly[name == domain, data][[1]],
                             crs = "+proj=longlat +datum=WGS84")

    pl_tm <- tm_shape(env_poly[name == domain, data][[1]], bbox = env_bbox) +
        tm_borders(lwd = 1, col = "black")
    for (poly in polys$polygons) {
        if(inherits(poly, "sf")) {
            pl_tm <- pl_tm + tm_shape(poly, bbox = env_bbox) +
                tm_borders(lwd = 2, col = "blue", alpha = 0.5 / (n_models * 2))
        }
    }
    for (poly in polys$polygons_no_clust) {
        if(inherits(poly, "sf")) {
            pl_tm <- pl_tm + tm_shape(poly, bbox = env_bbox) +
                tm_borders(lwd = 2, col = "blue", alpha = 0.5 / (n_models * 2))
        }
    }
    pl_tm <- pl_tm + tm_layout(main.title = poly_name$title, main.title.size = 0.5)

    tmap_save_wrapper(tm = pl_tm, filename = poly_name$file_out, scale = 0.5, dpi = 1200)

}
