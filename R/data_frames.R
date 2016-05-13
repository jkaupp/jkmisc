#' match_type - a function to match the types of two identically named data frames
#'
#' @param data data frame whose classes are to be changed
#' @param template data frame providing the template for the correct data classes
#'
#' @return data data with its classes changed to match df1
#' @export
match_type <- function(data = NULL, template = NULL) {
  if(is.null(data)|is.null(template)){
    stop("Please provide both data and a template to match to.")
  }

  if(names(template) != names(data)){
    stop("Data and template must have identical columns names.")
  }

  data <- as.data.frame(suppressWarnings(mapply(FUN = as,data,sapply(template,class), SIMPLIFY = FALSE)))
  return(data)
}

#' bind_mrows - a function to match the types of two identically named data frames
#' and bind the results.  The first input is also the template, and the second data frame
#' classes will be changed to match.
#'
#' @param df1 First Input data frame. Also the provides the template classes for the result
#' @param df2 Second input data frame.
#'
#' @return data \code{bind_rows(df1,d2)} with its classes changed to match df1
#' @export
bind_mrows <- function(df1 = NULL, df2 = NULL) {
  if(is.null(df1)|is.null(df2)){
    stop("Please provide both data and a template to match to.")
  }

  if(names(df1) != names(df2)){
    stop("Data and template must have identical columns names.")
  }

  df2 <- as.data.frame(suppressWarnings(mapply(FUN = as,df2,sapply(df1,class), SIMPLIFY = FALSE)))

  data <- dplyr::bind_rows(df1,df2)

  return(data)
}
