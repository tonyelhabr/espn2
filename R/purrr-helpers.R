
# # Reference: https://github.com/hrbrmstr/freebase/blob/master/inst/templates/mappers.R.
# # NOTE: Although the `map_dfr()` function actually uses `dplyr::bind_rows()`,
# # this is avoided by using `rbind()` and not including the `.id` argument.
# # Additionally, this function is different because it calls `purrr::map()`
# # instead of `map()` defined within this package
# # UPDATE: This doesn't work!.
# map_df <- function(.x, .f, ...) {
#
#   res <- purrr::map(.x, .f, ...)
#   out <- rbind(res)
#   out
#
# }
#
# map_dfr <- map_df
