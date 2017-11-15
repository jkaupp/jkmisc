#' @importFrom magrittr %>%
magrittr::`%>%`

#' grid_draw
#'
#' wrapper around grid.newpage() and grid.draw()
#'
#' @param x a grid object
#' @export

grid_draw <- function(x) {

  grid::grid.newpage()

  grid::grid.draw(x)

}
#' prog_from_plan
#'
#' @param x plan string
#'
#' @return prog
#' @export
prog_from_plan <- function(x) {

  if (grepl("CHEE", x)) {
    "CHEE"
  } else if (grepl("MECH|MEME", x)) {
    "MECH"
  } else if (grepl("ECEN|ELEC|CMPE", x)) {
    "ECE"
  } else if (grepl("MINE", x)) {
    "MINE"
  } else if (grepl("CIVL", x)) {
    "CIVL"
  } else {
    x
  }

}

#' wrapper for Not In.  Finds values that don't match to a provided vector
#'
#' @param x input vector to compare
#' @param table values to compare against
#'
#' @export
`%notin%` <- function(x, table){

  match(x, table, nomatch = 0L) == 0L

}


#' conveinence function to quickly filter all plans/concentrations under engineering
#' currently used only for undergrad
#'
#' @param x academic plan or concetration
#' @param core TRUE/FALSE
#'
#' @export
inFEAS <- function(x, core = FALSE) {

  if (core) {
    table <- c("ENGR", "ECEN", "ELEC", "CMPE", "CIVL", "CHEE", "ENCH", "MINE", "MECH", "MEME")
  } else {
    table <- c("ENGR", "ECEN", "ELEC", "CMPE", "CIVL", "CHEE", "ENCH", "MINE", "MECH", "MEME", "ENPH", "GEOE", "MTHE", "PEPA", "GSGE")
  }

  grepl(paste0(table, collapse = "|"), x)


}


#' Fix peoplesoft frozen data, accounting for degree, program and plan changes
#'
#' @param x input PS dataframe
#'
#' @return x the cleaned dataframe
#' @export
fix_frozen_data <- function(x) {

  conc_change <- stats::setNames(c("ECEN", "PEPA", "MEME", "MTHE", "GSGE",
                            "MINE", "CHEE", "CIVL", "DM-R", "GSGE", "MEME", "UN-R", "ECEN",
                            "PEPA", "CHEE", "CHEE", "GSGE"),
                          c("ECEN", "PEPA", "MEME", "MTHE", "GSGE",
                            "MINE", "CHEE", "CIVL", "DM-R", "GEOE", "MECH", "UN-R", "ELEC",
                            "ENPH", "ENCH", "CHEM", "GENG"))

  x <- dplyr::mutate_(x, conc1 = ~dplyr::if_else(grepl("SGS", acad_group), conc_change[conc1], dplyr::if_else(stringi::stri_sub(acad_plan, 1, 4) == "ECEN" & acad_career == "UGRD", "ENGR", conc1)))

  x <- dplyr::mutate_(x, acad_prog = ~dplyr::case_when(acad_prog == "BSCE" ~ "BASC",
                                       acad_prog == "PHDD" ~ "PHD",
                                       TRUE ~ acad_prog))

  return(x)
}
