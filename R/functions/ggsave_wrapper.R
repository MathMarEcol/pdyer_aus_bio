# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
ggsave_wrapper <- function(filename, plot,
                           units = "cm",
                           width = 9,
                           height = 9,
                           scale = 1,
                           device = cairo_pdf,
                           family = "sans",
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
                           family = "sans",
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
