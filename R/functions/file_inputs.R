#' List of targets that
#' track input files
file_inputs <- function() {
  list(
    tar_target(
      bac_site_csv,
      "./data/aus_microbiome/marine_bacteria/contextual.csv",
      format = "file"
    ),
    tar_target(
      bac_otu_csv,
      "./data/aus_microbiome/marine_bacteria/Bacteria.csv",
      format = "file"
    ),
    tar_target(
      zoo_data_dir,
      "./data/AusCPR",
      format = "file"
    ),
    tar_target(
      zoo_load_script,
      "./data/AusCPR/zooDataPhilMar21.R",
      format = "file"
    ),
    tar_target(
      phy_data_dir,
      "./data/AusCPR",
      format = "file"
    ),
    tar_target(
      phy_load_script,
      "./data/AusCPR/PhytoDataPhilFeb21.R",
      format = "file"
    ),
    tar_target(
      fish_taxon_file,
      "./data/Watson_Fisheries_Catch_Data/Version5/Output/TaxonomicData.rds",
      format = "file"
    ),
    tar_target(
      fish_data_dir,
      "./data/Watson_Fisheries_Catch_Data/Version5/Output/Annual_TotalCatchSpecies/",
      format = "file"
    ),
    tar_target(
      mapfile_location,
      "./data/ShapeFiles/World_EEZ_v8",
      format = "file"
    ),
    tar_target(
      biooracle_folder,
      "./data/bioORACLE",
      format = "file"
    )

  )
}
