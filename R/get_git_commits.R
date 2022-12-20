#' Get Commit Data from Gitlab
#'
#' @param project_id the id of a project. See project/settings/general
#' @param token a private token, stored in .Renviron by default. Create a token in User Settings/Access Tokens
#' @param return how you want the data returned. Choose "tableau" for a string vector you can paste in Tableau, or "string" for an unformatted character vector. Choose "data_frame" for a table of values. Choose "list" for a list with the table as one item and the string as the second item.
#'
#' @return up to 10000 commits from gitlab. How this is returned depends on the value of return
#' @export
get_git_commits  <- function(project_id = 102, token = Sys.getenv('GITLAB_TOKEN'), return = 'tableau', timezone = 'America/Chicago') {


  the_url  <- glue::glue("https://gitlab.udar.berkeley.edu/api/v4/projects/{project_id}/repository/commits?per_page=10000&private_token={token}")

  df <- jsonlite::fromJSON(the_url) |>
    as_tibble() |>
    select(
        commit_date = committed_date
      , desc = title
      , by = author_name
    ) |>
    mutate(
        commit_date =  str_replace(commit_date, "T", " ") |> str_remove("\\..*") |> lubridate::ymd_hms(tz = timezone)
      , the_text = str_c(commit_date, by, desc, sep = '    ')
    ) |>
    arrange(commit_date)


  if(return == 'tableau') {

    df$the_text |>
      str_replace('\\s+\\d\\d:\\d\\d:\\d\\d\\s+Jake Tolbert\\s+', '\t') |>
      str_c(collapse = '\n')

  } else if (stringr::str_detect(return, 'data.frame|table')) {
    return(df)
  } else if (stringr::str_detect(return, "string")) {
    return(df$the_text)
  } else if (stringr::str_detect(return, "list")) {
    return(
      list(
          data = df
        , text = df$the_text
      )
    )
  } else {
    return(df$the_text)
  }


}
