
#' Build Special Handling Codes
#'
#' @param no_contact a logical
#' @param no_solc a logical
#' @param no_mail a logical
#' @param no_email a logical
#' @param no_phone a logical
#' @param no_sms a logical
#' @param other_codes a character vector of any codes to add manually
#'
#' @return a vector of unique special handling codes based on the logical flags specified
build_handling_codes  <- function(no_contact, no_solc, no_mail, no_email, no_phone, no_sms, other_codes) {

  the_codes  <- character()

  if(!missing(no_contact) && no_contact) {
    the_codes  <- c('NC')
  }



  if(!missing(no_solc) && no_solc) {

    the_codes  <- c(the_codes, 'NS')


    # only add the vehicle-specific no solc codes if you have that type specified
    if(!missing(no_mail) && no_mail) {
        the_codes  <- c(the_codes, 'NMS')
    }

    if(!missing(no_email) && no_email) {
        the_codes  <- c(the_codes, 'NES')
    }

    if(!missing(no_phone) && no_phone) {
        the_codes  <- c(the_codes, 'NPS')
    }

    if(!missing(no_sms) && no_sms) {
        the_codes  <- c(the_codes, 'NTS')
    }

  }

  if(!missing(no_mail) && no_mail) {
    the_codes  <- c(the_codes, 'NM', 'NMS')
  }

  if(!missing(no_email) && no_email) {
    the_codes  <- c(the_codes, 'NE', 'NES')
  }

  if(!missing(no_phone) && no_phone) {
    the_codes  <- c(the_codes, 'NP', 'NPS')
  }

  if(!missing(no_sms) && no_sms) {
    the_codes  <- c(the_codes, 'NTX', 'NTS')
  }

  if(!missing(other_codes)) {
    the_codes  <- c(the_codes, other_codes)
  }

  the_codes  <- unique(the_codes)


  if(length(the_codes) == 0) {
    warning('You have not selected any handling codes!')
  }

  the_codes

}


#' Get Special Handling Codes for A Unit
#'
#' @param no_contact a logical
#' @param no_solc a logical
#' @param no_mail a logical
#' @param no_email a logical
#' @param no_phone a logical
#' @param no_sms a logical
#' @param other_codes a character vector of any codes to include. Use this if you don't want to use the logical flags
#' @param unit_codes a character vector of unit codes to include. 'UC' is included by default and added to all queries if you don't pass it in
#' @param group_codes a character vector of group codes to include. If NA, rows with blank group codes are returned
#' @param handling_tbl a tbl_Oracle where the handling data lives
#'
#' @return a tbl_Oracle with one row per unique entity_id and handling type code pair
#' @export
get_handling_for_unit  <- function(

    no_contact
  , no_solc
  , no_mail
  , no_email
  , no_phone
  , no_sms
  , other_codes
  , unit_codes = c('UC')
  , group_codes = NA
  , handling_tbl = tbl(cdw, in_schema2("CDW", "d_bio_handling_mv"))
) {

  # add UC to unit codes if it's not included
  if(!any(str_detect(unit_codes, 'UC'))) {
    unit_codes  <- c('UC', unit_codes)
  }


  the_codes  <- build_handling_codes(
      no_contact  = no_contact
    , no_solc     = no_solc
    , no_mail     = no_mail
    , no_email    = no_email
    , no_phone    = no_phone
    , no_sms      = no_sms
    , other_codes = other_codes
  )



  if(!is.na(group_codes)) {
    handling_tbl  <- handling_tbl |>
      filter(GROUP_CODE %in% group_codes)
  } else {
    handling_tbl  <- handling_tbl |>
      filter(is.na(trim(GROUP_CODE)))
  }


  handling_tbl |>
    filter(
      UNIT_CODE %in% unit_codes
      , HND_STATUS_CODE != 'R'
      , HND_TYPE_CODE %in% the_codes
    ) |>
    distinct(ENTITY_ID, HND_TYPE_DESC)

}


#' Spread Special Handling Codes
#'
#' @description pivots a vertical list of special handling codes out into a wide vector with one column per code and one row per entity ID
#' @param handling_data a distinct list of handling codes with one line per entity id and handling code. Can be generated with `get_handling_for_unit()`
#'
#' @return a dataframe with one row per entity id
#' @export
spread_handling  <- function(handling_data) {


  handling_data |>
    select(ENTITY_ID, HND_TYPE_DESC) |>
    mutate(val = HND_TYPE_DESC) |>
    pivot_wider(names_from = HND_TYPE_DESC, values_from = val)

}

