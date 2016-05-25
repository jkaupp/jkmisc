#' Install github packages by repository name
#'
#' To Do: - build ability to take a list/vector or single input
#'        - figure out how to wrap errors properly
#'        - figure out how to return errors properly
#'        - figure out how to install only R code packages (html widgets list as html)
#'        - figure out better means of searching github
#'
#'
#' @param package name of the package to install e.g "ggplot2"
#' @param force force re-installation of the package, defults to FALSE
#'
#' @return NULL
#' @export
#'
#' @examples
#' install_github_packages("ggplot2")
install_github_packages <- function(package, force = FALSE) {

  search_string <- sprintf("%s language:R language:HTML -user:cran", package)

  repo <- github::search.repositories(search_string, type="owner")

  repo_content <- repo[["content"]]

  if (repo_content$total_count != 0) {
    repo_items <- repo_content[["items"]][[1]]

    repo_info <- repo_items[["full_name"]]

    try(devtools::install_github(repo_info, force = force))
  } else
  {
    warning("No github repo exists")
  }

  }


