#' My ggplot2 theme heavy credits for influencing the theme function
#' go to @@hrbrmstr (Bob Rudis)
#'
#' It requires installing Bebas Neue fonts unless you change the font parameters
#'
#' \url{https://www.google.com/fonts}
#'
#' @param base_family base font family
#' @param base_size base font size
#' @param strip_text_family facet label font family
#' @param strip_text_size facet label text size
#' @param plot_title_family plot tilte family
#' @param plot_title_size plot title font size
#' @param plot_title_margin plot title margin
#' @param subtitle_family plot subtitle family
#' @param subtitle_size plot subtitle size
#' @param subtitle_margin plot subtitle margin
#' @param caption_family plot caption family
#' @param caption_size plot caption size
#' @param caption_margin plot caption margin
#' @param axis_title_family axis title font family
#' @param axis_title_size axis title font size
#' @param axis_title_just axis title font justification \code{blmcrt}
#' @param grid panel grid (\code{TRUE}, \code{FALSE}, or a combination of
#'        \code{X}, \code{x}, \code{Y}, \code{y})
#' @param axis axis \code{TRUE}, \code{FALSE}, [\code{xy}]
#' @param ticks ticks \code{TRUE}, \code{FALSE}
#' @param dark dark mode \code{TRUE}, \code{FALSE}
#' @param markdown enabled ggtext markdown styling  \code{TRUE}, \code{FALSE}
#' @param axis_text axis labels \code{TRUE}, \code{FALSE}, [\code{xy}]
#'
#' @export

theme_jk <- function(base_family = "Proxima Nova",
                           base_size = 11,
                           strip_text_family = base_family,
                           strip_text_size = 12,
                           plot_title_family = "Futura",
                           plot_title_size = 18,
                           plot_title_margin = 10,
                           subtitle_family = base_family,
                           subtitle_size = 12,
                           subtitle_margin = 15,
                           caption_family = base_family,
                           caption_size = 9,
                           caption_margin = 10,
                           axis_title_family = base_family,
                           axis_title_size = 9,
                           axis_title_just = "mm",
                           axis_text = TRUE,
                           dark = FALSE,
                           grid = TRUE,
                           axis = FALSE,
                           ticks = FALSE,
                           markdown = FALSE) {

  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())


  if (dark == TRUE) {

    ret <- ret + ggplot2::theme(plot.background = ggplot2::element_rect(fill ="#2E3440", color = "#2E3440"),
                                text = ggplot2::element_text(color = "white"),
                                axis.text = ggplot2::element_text(color = "white"),
                                strip.text = ggplot2::element_text(color = "white"))

    grid_color <- "#E5E9F0"
    tick_color <- "#E5E9F0"
    text_color <- "#FFFFFF"

  } else {

    ret <- ret + ggplot2::theme(plot.background = ggplot2::element_rect(fill ="white", color = "white"),
                                text = ggplot2::element_text(color = "black"),
                                axis.text = ggplot2::element_text(color = "black"),
                                strip.text = ggplot2::element_text(color = "black"))

    grid_color <- "#cccccc"
    tick_color <- "black"
    text_color <- "black"
  }

   if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = grid_color, size = 0.05))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }

  } else {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis  ==  TRUE) {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(color = grid_color, size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks  =  ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x  =  ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y  =  ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length  =  grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

  ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size/2)))
  ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size/2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))

  if(!markdown) {

    ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size/2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size/2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))

    ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))

  } else {

    ret <- ret + ggplot2::theme(axis.text.x = ggtext::element_markdown(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size/2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggtext::element_markdown(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size/2))) + ggplot2::theme(axis.title = ggtext::element_markdown(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggtext::element_markdown(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggtext::element_markdown(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggtext::element_markdown(hjust = 0, size = strip_text_size, family = strip_text_family))

    ret <- ret + ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggtext::element_markdown(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggtext::element_markdown(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))

  }

  if (inherits(axis_text, "character") | axis_text == TRUE) {

    if (!markdown) {

      ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = text_color))
      ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = text_color))

      if (inherits(axis_text, "character")) {
        if (regexpr("x", axis_text)[1] < 0) ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_blank())
        if (regexpr("y", axis_text)[1] < 0) ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_blank())
      }

    } else {

      ret <- ret + ggplot2::theme(axis.text.x = ggtext::element_markdown(color = text_color))
      ret <- ret + ggplot2::theme(axis.text.y = ggtext::element_markdown(color = text_color))

      if (inherits(axis_text, "character")) {
        if (regexpr("x", axis_text)[1] < 0) ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_blank())
        if (regexpr("y", axis_text)[1] < 0) ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_blank())
      }

    }
  } else {
    ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

  ret <- ret + ggplot2::theme(plot.margin = ggplot2::margin(base_size/2, base_size/2, base_size/2, base_size/2))

  ret

}

#' gtable_remove_grob
#'
#' Helper function to remove grobs by name, from gtables
#'
#' @param g, gtable with the grob removed
#' @param pattern grob name or pattern to match
#'
#' @return g, with pattern removed.
gtable_remove_grob <- function(g, pattern = "guide-box") {
  matches <- c(grepl(pattern = pattern, g$layout$name))

  g$layout <- g$layout[!matches, , drop = FALSE]

  g$grobs <- g$grobs[!matches]
  return(g)
}

#' gtable_extract_grob
#'
#' Helper function to extract a grob from gtables by name.
#'
#' @param g, the gtabel to extract the grob
#' @param pattern, grob name or pattern to match
#'
#' @return g, a grob matching the specified pattern
gtable_extract_grob <- function(g, pattern = "guide-box") {
  matches <- grepl(pattern = pattern, g$layout$name)

  g$layout <- g$layout[matches, , drop = FALSE]

  g$grobs <- g$grobs[matches]
  return(g)
}





#' Five thirty-eight style formatter for Ratios
#'
#' @param labels vector of labels
#'
#' @return formatted ratio labels
#' @export
scale_ratio_labels <- function(labels) {

  labels_out <- as.character(labels)

  labels_out[length(labels)] <- sprintf("%s:1",as.character(labels[length(labels)]))

  return(labels_out)

}

#' Five thirty-eight style formatter for percentages
#'
#' @param labels vector of labels
#'
#' @return formatted percent labels
#' @export
scale_percent_labels <- function(labels){

  labels <- labels*100

  labels[length(labels)] <- paste0(labels[length(labels)], "%")

  return(labels)

}

#' Five thirty-eight style formatter for currency
#'
#' @param labels vector of labels
#'
#' @return formatted percent labels
#' @export
scale_dollar_labels <- function(labels){

  labels <- labels

  labels[length(labels)] <- paste0(labels[length(labels)], "$")

  return(labels)

}



#' label_wrap_md_gen
#'
#' @param width line with
#' @param multi_line is it multiline
#'
#' @export
label_wrap_md_gen <- function (width = 25, multi_line = TRUE)
{
  fun <- function(labels) {
    labels <- ggplot2::label_value(labels, multi_line = multi_line)
    lapply(labels, function(x) {
      x <- strwrap(x, width = width, simplify = FALSE)
      vapply(x, paste, character(1), collapse = "<br>")
    })
  }
  structure(fun, class = "labeller")
}
