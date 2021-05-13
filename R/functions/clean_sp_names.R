clean_sp_names <- function(x) {
length_pre <- length(unique(x))
x %>%
    stringr::str_replace_all("\\(.*\\)", "") %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("-", "_") %>%
    stringr::str_replace_all("/", "") %>%
    stringr::str_replace_all("__", "_") -> y
length_post <- length(unique(y))
assertthat::assert_that(length_pre == length_post)
return(y)
}
