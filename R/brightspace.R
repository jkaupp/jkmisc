#' import_brightspace
#'
#'  This function takes an input data frame, in D2L export format and exports it
#'  into a tidy data frame in a specific tidy format
#'
#' @param df a data frame in brightspace export wide-format
#'
#' @return df
#' @export

import_brightspace <- function(df, mode = "median")
{


  # transform column names to lowercase
  df  <- magrittr::set_names(df,tolower(names(df)))

  # select needed columns,
  # Imports change between data available

  df <- group_by(df, `org defined id`,`activity name`, `rubric name`,  criterion) %>%
    distinct %>%
    ungroup %>%
    select(`course code`, `org defined id`, `activity name`, `assessor first name`,`assessor last name`, criterion, level) %>%
    unite(assessor,`assessor first name`, `assessor last name`, sep = " ") %>%
    group_by(`org defined id`, `activity name`, criterion, assessor) %>%
    mutate(level = factor(level, c("Not Demonstrated","Marginal","Developing","High Quality","Mastery")),
           score = as.numeric(level))

  if(mode != "median"){
    df <- summarize(df, score = mean(score))
  } else {
    df <- summarize(df, score = median(score))
  }

  return(df)
}



