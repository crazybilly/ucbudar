#' Print All
#'
#' @description a quick helper function to print all the rows of a tibble to the console. Basically a wrapper for print(data, n = 1000)
#'
#' @param data an object to print to the console
#' @param n number of rows to print. Defaults to 1000, unlike print.tibble, which prints 10-15 rows by default
#' @param ... other options passed to print()
#'
#' @return returns the results of print(data), usually data itself
#' @export
#'
print_all  <- function(data, n=1000, ...)  {

  if('tbl' %in% class(data)) {
    print(data, n = n, ...)
  } else {
    print(data, ...)
  }


}
