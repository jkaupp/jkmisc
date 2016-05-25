#' Function to check a data frame for primary key integrity prior to
#' copying to the database.
#'
#' @param df Data frame to check.  Columns must contain all elements of the key to test
#' @param p_key Character vector of colnames that comprise the primary key

p_key_check <- function(df, p_key)
{

  if(any(!(p_key %in% colnames(df)))){
    stop("Table doesn't contain all elements of the primary key")
  }

  check <- df %>%
    group_by_(.dots = p_key) %>%
    tally %>%
    filter(n>1)


  if (nrow(check) == 0)
  {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
