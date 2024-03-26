# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only


#' Getting my feet wet with drake

#' Import packages
library(drake)
library(dplyr)
library(ggplot2)

#' define/source functions not already in packages
# a trivial function.
create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram()
}
#' The demo does this to fail fast if anything is missing
#' I won't do this regularly
file.exists("raw_data.xlsx")
file.exists("report.Rmd")
#' Here the plan is defined.
plan <- drake_plan(
  ##These are arguments to drake_plan()
  raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
  data = raw_data %>%
    mutate(Species = forcats::fct_inorder(Species)),
  hist = create_plot(data),
  fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  ##does order matter? Probably not!
  report = rmarkdown::render(
                        knitr_in("report.Rmd"),
                        output_file = file_out("report.html"),
                        quiet = TRUE
                      )
)

make(plan)
ggsave("hist_test.png" , readd(hist) )
