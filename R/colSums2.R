




#' @title Column Sums (safe-ish)
#' @desciption calculate column sums for all columns, returning NA for columns that won't coerce to numeric
#'
#' @param df a data frame with columns to sum
#' @param remove_non_numeric a logical value determing whether or not NA values (ie. columns that cannot be cooreced to numeric and therefore won't sum) should be removed from the returned results
#' @param na.rm a logical value determining if NA values should be ignored when summing
#'
#' @return a one-row data frame with the sum of each column. Columns that cannot be summed return NA unless remove_non_numeric=T
#' @export
#'
colSums2  <- function(df, remove_non_numeric = F, na.rm = T) {


  the_sums  <- map_dfc(df, ~ sum(as.numeric(.x), na.rm = na.rm ) )

  if(remove_non_numeric) {

    the_sums[,as.logical(!is.na(the_sums[1,]))]

  } else {
    return(the_sums)
  }

}
