
#' Get Data Dictionary
#'
#' @param cdw_only a logical determining whether we should just grab the CDW-related columns (and DATA_DESCRIPTION)
#'
#' @return a data_frame of the main big data dictionary
#' @export
#'
get_main_dictionary  <- function( cdw_only = T)  {

  if(cdw_only) {
      googlesheets4::read_sheet(
        'https://docs.google.com/spreadsheets/d/1TeU71m8b4iKCyKNSPJcLgEfNzt8HRrGwV4QJkKT4JGo/edit#gid=1902720326'
        , range = 'Z:AB'
      ) |>
        dplyr::bind_cols(
          googlesheets4::read_sheet(
            'https://docs.google.com/spreadsheets/d/1TeU71m8b4iKCyKNSPJcLgEfNzt8HRrGwV4QJkKT4JGo/edit#gid=1902720326'
            , range = 'G:G'
          )
        )
    } else {
      googlesheets4::read_sheet(
        'https://docs.google.com/spreadsheets/d/1TeU71m8b4iKCyKNSPJcLgEfNzt8HRrGwV4QJkKT4JGo/edit#gid=1902720326'
      )
    }

}


#' Search the Data Dictionary
#'
#' @param search_term  a regular expression to search across for within every column
#' @param cdw_table a regular expression to search for in the CDW table name column
#' @param cdw_col a regular expression to search for in the CDW column name column
#' @param the_dictionary a dictionary object to search within, as created by get_main_dictionary(). By default, pulls the CDW-related columns.
#'
#' @return a filtered data_Frame from the_dictionary matching the specified search terms
#' @export
#'
search_dictionary  <- function(search_term, cdw_table, cdw_col,  the_dictionary = get_main_dictionary()) {


  if(!missing(cdw_table)) {
    the_results  <- the_dictionary |>
      dplyr::filter(stringr::str_detect(CDW_TABLE_NAME, cdw_table))
  } else {
    the_results  <- the_dictionary
  }

  if(!missing(cdw_col)) {
    the_results  <- the_dictionary |>
      dplyr::filter(stringr::str_detect(CDW_COLUMN_NAME, cdw_col))
  }

  if(!missing(search_term)) {
    the_results  <- the_results  |>
      dplyr::filter(
        dplyr::if_any(dplyr::everything(),  ~stringr::str_detect(.x, search_term))
      )
  }


  return(the_results)


}


