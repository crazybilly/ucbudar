#' @title Count_if
#'
#' @description returns a vector of counts for a vector. Uses table() to calculate counts. Primarily for use in dplyr::mutate().
#' @param col a referece to the column to be counted.
#' @return a integer vector of counts for each value in col.
#' @export

count_if  <- function( col  ) {
  table(col)[as.character(col)]  %>% as.integer()
}

