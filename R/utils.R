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

#' @title Cleans names of a data.frame.
#'
#' @description
#' Resulting names are unique and consist only of the \code{_} character, lowercase letters, and numbers.
#' From janitor
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with clean names.
#' @export
#' @examples
#' # not run:
#' # clean_names(poorly_named_df)
#'
#' # library(dplyr) ; library(readxl)
#' # not run:
#' # readxl("messy_excel_file.xlsx") %>% clean_names()

clean_names <- function(dat){

  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("'", "", .) %>% # remove quotation marks
    gsub("\"", "", .) %>% # remove quotation marks
    gsub("%", "percent", .) %>%
    make.names(.) %>%
    gsub("[.]+", "_", .) %>% # convert 1+ periods to single _
    gsub("[_]+", "_", .) %>% # fix rare cases of multiple consecutive underscores
    tolower(.) %>%
    gsub("_$", "", .) # remove string-final underscores

  # Handle duplicated names - they mess up dplyr pipelines
  # This appends the column number to repeated instances of duplicate variable names
  dupe_count <- sapply(1:length(new_names), function(i) { sum(new_names[i] == new_names[1:i]) })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1],
                                     sep = "_")
  stats::setNames(dat, new_names)
}



#' @title Removes empty rows from a data.frame.
#'
#' @description
#' Removes all rows from a data.frame that are composed entirely of \code{NA} values.
#' From Janitor
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with no empty rows.
#' @export
#' @examples
#' # called with magrittr pipe %>% :
#' # library(dplyr)
#' # not run:
#' # dat %>% remove_empty_rows

remove_empty_rows <- function(dat){
  dat[rowSums(is.na(dat)) != ncol(dat), ]
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
#' @param target target column to sort
#' @export
#'
#' @return a data frame with the ypos column added
spaced_sort <- function(.data, min_space = 0.05, target) {
  ## Define a minimum spacing (5% of full data range)
  min_space <- min_space*diff(range(.data[target]))

  data <- .data %>%
    split(list(.$cohort)) %>%
    purrr::map_df(~ calc_spaced_offset(.x, min_space, target))

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
calc_spaced_offset <- function(.data, min_space, target) {

  ## Sort by value
  ord <- order(.data[target], decreasing = T)
  ## Calculate the difference between adjacent values
  delta <- -1*diff(dplyr::arrange(.data[target], ord))
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
    dplyr::arrange_(dplyr::desc(~target)) %>%
    dplyr::mutate_(mod_diff = offset + ~target)

  return(data)
}

