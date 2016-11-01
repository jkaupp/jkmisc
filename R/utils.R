#' grid_draw
#'
#' wrapper around grid.newpage() and grid.draw()
#'
#' @param x a grid object
#'
#' @return
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

  dplyr::case_when(grepl("CHEE", x) ~ "CHEE",
                   grepl("MECH|MEME",x) ~ "MECH",
                   grepl("ECEN|ELEC|CMPE", x) ~ "ECE",
                   grepl("MINE", x) ~ "MINE",
                   grepl("CIVL", x) ~ "CIVL")


}
