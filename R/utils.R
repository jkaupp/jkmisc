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


#' str_break
#'
#' @param html_string string of html to break
#' @param width  width to break at
#' @param indent left indent
#' @param exdent right indent
#'
#' @return string with html line breaks <br> at specified width
#' @export
#'
str_break <- function (html_string, width = 80, indent = 0, exdent = 0) {

  tags <- unlist(stringi::stri_extract_all_regex(html_string, "<.*?>"))

  full_tags <- unlist(stringi::stri_extract_all_regex(html_string, "<span[^>]*>[^>]*<\\/span>"))

  plain_string <- stringi::stri_replace_all_fixed(html_string, c(tags, style), "", vectorize_all = FALSE)

  highlight <- stringi::stri_replace_all_fixed(full_tags, c(tags, style), "", vectorize_all = FALSE)

  if (width <= 0)
    width <- 1

  out <- stringi::stri_wrap(plain_string, width = width, indent = indent,
                            exdent = exdent, simplify = FALSE)

  broken <- vapply(out, stringi::stri_c, collapse = "<br>", character(1))

  stringi::stri_replace_all_fixed(broken, highlight, full_tags, vectorize_all = FALSE)

}

#' Format text with markdown and span elements for use with ggtext
#' element_markdown or geom_richtext
#'
#' @param text text to highight
#' @param colour hex code of color to highlight
#' @param style \strong{b} for bold, \emph{i} for italics or a two letter combination \strong{\emph{bi}} for both
#' @param size text size in px, defaults to 16
#'
#' @return formatted string
#' @export
highlight_text <- function(text, colour = "#000000", style = "", size = 16) {

  out <- switch(style,
                "i" = glue::glue("*{text}*"),
                "b" = glue::glue("**{text}**"),
                "ib" = glue::glue("***{text}***"),
                "bi" = glue::glue("***{text}***"),
                 text)

  as.character(glue::glue("<span style = 'color:{colour}; font-size:{size}px;'>{out}</span>"))

}


#' Make word into W O R D
#'
#' @param text word to space and capitalize
#'
#' @return T E X T
#' @export
spaced_title <- function(text) {

  gsub("(?<=.)(?!$)", " ", toupper(text), perl = TRUE)


}
