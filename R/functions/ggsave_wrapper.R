ggsave_wrapper <- function(filename, plot,
                           units = "cm",
                           width = 9,
                           height = 9,
                           scale = 1,
                           device = cairo_pdf,
                           family = "serif",
                           ...
                           ){
  ggplot2::ggsave(filename =  filename,
         plot = plot,
         units = units,
         width = width,
         height = height,
         scale = scale,
         device = device,
         family = family,
         ...
         )
}

tmap_save_wrapper <- function(tm, filename,
                           units = "cm",
                           width = 9,
                           height = 9,
                           scale = 1,
                           device = cairo_pdf,
                           family = "serif",
                           ...
                           ){
  tmap::tmap_save(tm = tm,
            filename = filename,
            units = units,
            width = width,
            height = height,
            scale = scale,
            device = device,
            family = family,
            ...
            )
}
