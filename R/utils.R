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

#' wrapper for Not In.  Finds values that don't match to a provided vector
#'
#' @param x input vector to compare
#' @param table values to compare against
#'
#' @export
`%notin%` <- function(x, table){

  match(x, table, nomatch = 0L) == 0L

}

#' Produce Date from Peoplesoft Term
#'
#' @param term a peoplesoft term code
#'
#' @return a useable date
#' @export
date_from_term <- function(term) {

  year <- as.numeric(sprintf("20%s", stringi::stri_sub(term, 2, 3)))

  month <- as.numeric(stringi::stri_sub(term, 4, 4))

  lubridate::make_date(year, month)

}


#' Produce academic year from Peoplesoft Term
#'
#' @param term a peoplesoft term code
#'
#' @return academic year
#' @export
acadYear_from_term <- function(term) {

  year <- stringi::stri_sub(term, 2, 3)

  if (grepl("^0", year)) {

    start_year <- sprintf("20%s", year)

    if (start_year == "2009") {
      end_year <- "2010"
    } else {

    end_year <- sprintf("200%s", as.numeric(year) + 1)
    }

    paste(start_year, end_year, sep = "-")

  } else  {

    start_year <- sprintf("20%s", year)

    end_year <- sprintf(ifelse(stringi::stri_length(year) == 1, "200%s", "20%s"), as.numeric(year) + 1)

    paste(start_year, end_year, sep = "-")
  }

}


#' str_break
#'
#' @param string string to break
#' @param width  width to break at
#' @param indent left indent
#' @param exdent right indent
#'
#' @return string with line braaks at specified width
#' @export
#'
str_break <- function (string, width = 80, indent = 0, exdent = 0)
{
  if (width <= 0)
    width <- 1
  out <- stringi::stri_wrap(string, width = width, indent = indent,
                   exdent = exdent, simplify = FALSE)
  vapply(out, stringr::str_c, collapse = "\n", character(1))
}

str_break_wrap <- function (html_string, width = 80, indent = 0, exdent = 0) {

  tags <- stringr::str_extract_all(html_string, "<.*?>") %>%
    purrr::flatten_chr()

  index <- sprintf("tag_%s", seq_along(tags))

  string <- stringr::str_replace_all(html_string, set_names(index, tags))

  if (width <= 0)
    width <- 1

  out <- stringi::stri_wrap(string, width = width, indent = indent,
                            exdent = exdent, simplify = FALSE)

  broken <- vapply(out, str_c, collapse = "<br>", character(1))

  stringr::str_replace_all(broken, set_names(tags, index))

}

#' Format text with markdown and span elements for use with ggtext
#' element_markdown or geom_richtext
#'
#' @param text text to highight
#' @param colour hex code of color to highlight
#' @param style \strong{b} for bold, \emph{i} for italics or a two letter combination \strong{\emph{bi}} for both
#'
#' @return formatted string
#' @export
highlight_text <- function(text, colour = "#000000", style = "") {

  out <- switch(style,
                "i" = glue::glue("*{text}*"),
                "b" = glue::glue("**{text}**"),
                "ib" = glue::glue("***{text}***"),
                "bi" = glue::glue("***{text}***"),
                 text)

  if (style != "") {

    tags$span(style = glue::glue("color:{colour}"), out)

    } else {

   out

  }




}
