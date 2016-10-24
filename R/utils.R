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
