#' Get Solicitation Plan Assignments
#'
#' @param start_date a YYYY-MM-DD date-like object (can be a string or date) for the date range when plan assignment should be active
#' @param end_date a YYYY-MM-DD date-like object (can be a string or date) for the date range when plan assignment should be active
#' @param fundraiser_id an optional vector of fundraiser entity IDs
#' @param unit an optional vector of assignment office codes
#' @param include_post_commit_assignments a logical indicating whether assignments which started after the proposal commit/turndown date should be include. TRUE to include stewardship and planned giving assignments. FALSE for standard gift cultivation-type assignments.
#' @param db an active database connection
#'
#' @return a database tibble with one row per fundraiser/assignment/proposal/prospect/entity. Includes non-primary entities for all prospects.
#' @export
#'
get_do_assignments    <- function(start_date = fyStartDate, end_date = fyEndDate, fundraiser_id, unit, include_post_commit_assignments = F, db = cdw) {

  f_assignment_mv  <- tbl(db, in_schema2("CDW", "f_assignment_mv"))

  qualification_dates  <- get_qualification_date(db = db)

  the_start_dt  <- format(as.Date(start_date), '%Y-%m-%d')
  the_end_dt    <- format(as.Date(end_date)  , '%Y-%m-%d')

  assignments  <- f_assignment_mv %>%
    filter(
        ASSIGNMENT_TYPE == 'DO'
      , !is.na(PROPOSAL_ID)
      , START_DATE <= to_date(local(the_start_dt), 'YYYY-MM-DD')
      , (is.na(STOP_DATE) | STOP_DATE >= to_date(local(the_end_dt), 'YYYY-MM-DD'))
    )

  if(!missing(fundraiser_id)) {
    assignments  <- assignments %>%
      filter(ASSIGNMENT_ENTITY_ID %in% fundraiser_id)
  }

  if(!missing(unit)) {
    assignments  <- assignments %>%
      filter(OFFICE_CODE %in% unit)
  }


  assignment_prep  <- assignments |>
    mutate(FUNDRAISER_NAME = ASSIGNMENT_LAST_NAME || ', ' || ASSIGNMENT_FIRST_NAME) |>
    select(
      PROPOSAL_ID
      , FUNDRAISER_ID = ASSIGNMENT_ENTITY_ID
      , FUNDRAISER_LAST_NAME  = ASSIGNMENT_LAST_NAME
      , FUNDRAISER_FIRST_NAME = ASSIGNMENT_FIRST_NAME
      , ASSIGNMENT_ACTIVE = ACTIVE_IND
      , ASSIGNMENT_START_DT = START_DATE
      , ASSIGNMENT_STOP_DT  = STOP_DATE
    ) |>
    left_join(
      tbl(db, in_schema2("CDW", "f_proposal_mv")) %>%
      filter(
          PRIMARY_IND == 'Y'
        ) |>
        select(
          PROPOSAL_ID
          , PROPOSAL_TITLE
          , PROPOSAL_TYPE
          , PROPOSAL_TYPE_DESC
          , PROPOSAL_STATUS_CODE
          , PROPOSAL_STATUS_DESC
          , STAGE_CODE
          , STAGE_DESC
          , PROPOSAL_START_DT = START_DT
          , TARGET_ASK_AMT        = ORIGINAL_ASK_AMT
          , TARGET_ASK_DT         = EXPECTED_DATE
          , ACTUAL_ASK_AMT        = ASK_AMT
          , ACTUAL_ASK_DT         = INITIAL_CONTRIBUTION_DT
          , COMMIT_TURNDOWN_AMT = GRANTED_AMT
          , COMMIT_TURNDOWN_DT    = STOP_DT
          , PROSPECT_ID
        )
      , by = 'PROPOSAL_ID'
    )


  # only include assignments that started before the commit
  if(!include_post_commit_assignments) {
    assignment_prep  <- assignment_prep %>%
      filter(ASSIGNMENT_START_DT < COMMIT_TURNDOWN_DT)

  }



  assignment_prep |>
    left_join(
      tbl(db, in_schema2("CDW", "d_prospect_mv")) |>
        filter(
          START_DATE <= to_date(local(the_start_dt), 'YYYY-MM-DD')
          , (is.na(STOP_DATE) | STOP_DATE >= to_date(local(the_end_dt), 'YYYY-MM-DD'))
        ) |>
        select(
          PROSPECT_ID
          , ENTITY_ID
          , PROSPECT_NAME
          , PROSPECT_ACTIVE     = ACTIVE_IND
          , PROSPECT_ENTITY_PRIMARY_IND
          , PROSPECT_START_DT = START_DATE
          , PROSPECT_STOP_DT    = STOP_DATE
        )
      , by = 'PROSPECT_ID'
    ) %>%
    left_join(qualification_dates, by = 'PROPOSAL_ID') %>%
    mutate(
        ask_in_range    = case_when(
            ACTUAL_ASK_DT      >= to_date(local(the_start_dt), 'YYYY-MM-DD') & ACTUAL_ASK_DT      <= to_date(local(the_end_dt), 'YYYY-MM-DD') ~ PROPOSAL_ID
          , 0==0 ~ na_dbl
        )
      , commit_in_range = case_when(
            COMMIT_TURNDOWN_DT >= to_date(local(the_start_dt), 'YYYY-MM-DD') & COMMIT_TURNDOWN_DT <= to_date(local(the_end_dt), 'YYYY-MM-DD') ~ PROPOSAL_ID
          , 0==0 ~ na_dbl
        )
      , qualification_in_range = case_when(
            QUALIFICATION_DATE >= to_date(local(the_start_dt), 'YYYY-MM-DD') & QUALIFICATION_DATE <= to_date(local(the_end_dt), 'YYYY-MM-DD') ~ PROPOSAL_ID
          , 0==0 ~ na_dbl
        )
    )

}
