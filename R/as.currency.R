#' Format as currency
#'
#' Given a numeric vector, format it as currency, implicitly changing it to a string.
#'
#' Think about using formattable::currency() instead, which preserves numbers (but displays them as currency).
#'
#' @param x a numeric vector to be formatted
#' @return a vector of strings formated as currency, with commas and dollar signs.
#' @export



as.currency  <- function(x) {

  if(exists('currency')) {

    formattable::currency(x)

  } else {

  dplyr::case_when(
      # return NAs as is
     is.na(x) ~ NA_character_

    , T ~ paste0("$",
             formatC(x
                     , format   = 'f'
                     , big.mark = ','
                     , digits   =  2
                     )
            )
    )
  }
}
