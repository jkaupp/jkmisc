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

