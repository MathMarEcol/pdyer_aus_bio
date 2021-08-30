ggsave_wrapper <- function(filename, plot,
                           units = "cm",
                           width = 16,
                           height = 9,
                           dpi = 300,
                           scale = 2
                           ){
  ggsave(filename =  filename,
         plot = plot,
         units = units,
         width = width,
         height = height,
         dpi = dpi,
         scale = scale
         )
}

tmap_save_wrapper <- function(tm, filename,
                           units = "cm",
                           width = 16,
                           height = 9,
                           dpi = 300,
                           scale = 2
                           ){
  tmap_save(tm = tm,
            filename = filename,
            units = units,
            width = width,
            height = height,
            dpi = dpi,
            scale = scale
            )
}
