#' get SQL from file
#'
#' @param filepath the path of an SQL script.
#' @param returnsql a logical value determining whether the function should return an SQL object (via dplyr::sql()) or just a text string
#' @return either an sql object or a text string to be used in a tbl() connection
#' @export
#'
getSQL <- function(filepath, returnsql = T){

  con = file(filepath, "r")
  sql.string <- ""

  while (TRUE){
    line <- readLines(con, n = 1)

    if ( length(line) == 0 ){
      break
    }

    line <- gsub("\\t", " ", line)

    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }

    sql.string <- paste(sql.string, line)
  }

  close(con)

  if(!returnsql) {
    return(sql.string)
  } else {
    dplyr::sql(sql.string)
  }

}
