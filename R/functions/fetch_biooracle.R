fetch_biooracle <- function(biooracle_layers)    {
      tmp_timeout <- getOption("timeout")
      options(timeout = 60000)

      sdmpredictors::load_layers(biooracle_layers,
        datadir = biooracle_folder,
        rasterstack = FALSE
        )

      options(timeout = tmp_timeout)

      out <- if (biooracle_layers == "MS_bathy_5m") {
        file.path(biooracle_folder, paste0(biooracle_layers, "_lonlat.tif"))
      } else {
        file.path(biooracle_folder, paste0(biooracle_layers, "_lonlat.zip"))
      }
    }
