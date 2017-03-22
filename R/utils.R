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
#'
#' @export
in_FEAS <- function(x) {

  grepl(c("ENGR|ECEN|ELEC|CMPE|CIVL|CHEE|ENCH|MINE|MECH|MEME|ENPH|GEOE|MTHE|PEPA|GSGE"), x)

}

#' Spaced sort for slopegraphs
#'
#' Calculates the position of each element to ensure a minimum
#' space between adjacent entries within a column, while preserving
#' rank order.  Group lines can cross
#'
#' Credit to James Keirstead for the original function
#' https://github.com/jkeirstead/r-slopegraph
#'
#' @param .data the raw data frame
#' @param min_space fraction of total data range to leave as a
#' minium gap
#' @param group  grouping column to split data on
#' @param target target column to sort
#'
#' @export
#'
#' @return a data frame with the ypos column added
slopegraph_sort <- function(.data, target, group = NULL, min_space = 0.05) {
  ## Define a minimum spacing (5% of full data range)
  min_space <- min_space*diff(range(.data[[target]]))

  if (!is.null(group)) {
    data <- .data %>%
      split(interaction(.data[[group]])) %>%
      purrr::map_df(~ calc_spaced_offset(.x, min_space, target))
  } else {

    data <- slopegraph_offset(.data, min_space, target)
  }


  return(data)
}

#' Calculates the vertical offset between successive data points
#' Credit to James Keirstead for the original function
#' https://github.com/jkeirstead/r-slopegraph
#'
#' @param .data a data frame representing a single year of data
#' @param min_space the minimum spacing between y values
#' @param target target column to sort
#'
#' @export
#' @return a data frame
slopegraph_offset <- function(.data, min_space, target) {

  ## Sort by value
  ord <- order(.data[[target]], decreasing = T)
  ## Calculate the difference between adjacent values
  delta <- -1*diff(.data[[target]][ord])
  # delta <- -1*diff(dplyr::arrange(.data[target], ord))
  ## Adjust to ensure that minimum space requirement is met
  offset <- (min_space - delta)
  offset <- replace(offset, offset < 0, 0)
  ## Add a trailing zero for the lowest value
  offset <- c(offset, 0)
  ## Calculate the offset needed to be added to each point
  ## as a cumulative sum of previous values
  offset <- rev(cumsum(rev(offset)))
  ## Assemble and return the new data frame
  data <- .data %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(.dots = paste0("desc(", target, ")")) %>%
    dplyr::mutate_(.dots = stats::setNames(list(lazyeval::interp(~ offset + var, var = as.name(target))), "spaced"))

  return(data)
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
