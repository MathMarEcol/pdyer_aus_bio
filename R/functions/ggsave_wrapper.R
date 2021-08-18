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
