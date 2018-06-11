#' My ggplot2 theme heavy credits for influencing the theme function
#' go to @@hrbrmstr (Bob Rudis)
#'
#' It requires installing Oswald fonts unless you change the font parameters
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
#' @param ticks ticks
#' @export

theme_jk <- function(base_family="Oswald",
                           base_size = 11,
                           strip_text_family = base_family,
                           strip_text_size = 12,
                           plot_title_family = "Oswald",
                           plot_title_size = 18,
                           plot_title_margin = 10,
                           subtitle_family = "Scope One",
                           subtitle_size = 12,
                           subtitle_margin = 15,
                           caption_family = "Scope One",
                           caption_size = 9,
                           caption_margin = 10,
                           axis_title_family = "Oswald",
                           axis_title_size = 9,
                           axis_title_just = "mm",
                           grid = TRUE,
                           axis = FALSE,
                           ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = "#cccccc", size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "#cccccc", size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = "#cccccc", size = 0.05))

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
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(color = "#cccccc", size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = "#cccccc", size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = "#cccccc", size = 0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = "#cccccc", size = 0.15))
      ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = "#cccccc", size = 0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
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

  ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * base_size/2)))
  ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * base_size/2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))
  ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
  ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
  ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
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

#' Custom formatter for peoplesoft terms
#'
#' @param labels vector of labels
#'
#' @return formatted year labels
#' @export
scale_ps_year <- function(labels) {

  labels_out <- sprintf("20%s", stringi::stri_sub(labels, 2, 3))

  labels_out <- c(labels_out[1], stringi::stri_sub(labels_out[-1], 3, 4))

  return(labels_out)


}



#' Queen's FEAS theme heavy credits for influencing the theme function
#' go to @@hrbrmstr (Bob Rudis)
#'
#' It requires installing Oswald fonts unless you change the font parameters
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
#' @param ticks ticks
#' @export

theme_feas <- function(base_family="Roboto Condensed Light",
                       base_size = 16,
                       strip_text_family = base_family,
                       strip_text_size = 12,
                       plot_title_family = "Roboto Condensed Light",
                       plot_title_size = 18,
                       plot_title_margin = 10,
                       subtitle_family = "Raleway ExtraLight",
                       subtitle_size = 12,
                       subtitle_margin = 15,
                       caption_family = "Raleway ExtraLight",
                       caption_size = 9,
                       caption_margin = 10,
                       axis_title_family = "Roboto Condensed Light",
                       axis_title_size = 9,
                       axis_title_just = "mm",
                       grid = TRUE,
                       axis = FALSE,
                       ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = "#cccccc", size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "#cccccc", size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = "#cccccc", size = 0.05))

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
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(color = "#cccccc", size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = "#cccccc", size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = "#cccccc", size = 0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = "#cccccc", size = 0.15))
      ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = "#cccccc", size = 0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  if (inherits(ticks, "character") | ticks == TRUE) {
    ret <- ret + ggplot2::theme(axis.ticks  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length  =  grid::unit(5, "pt"))

    if (inherits(ticks, "character")) {
      ticks <- tolower(ticks)
      if (regexpr("x", ticks)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.ticks.x  =  ggplot2::element_blank())
      }

      if (regexpr("y", ticks)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.ticks.y  =  ggplot2::element_blank())
      }
    }
  } else {
    ret <- ret + ggplot2::theme(axis.ticks  =  ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x  =  ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y  =  ggplot2::element_blank())

  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

  ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * base_size/2)))
  ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * base_size/2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))
  ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
  ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
  ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
  ret <- ret + ggplot2::theme(plot.margin = ggplot2::margin(base_size/2, base_size/2, base_size/2, base_size/2))

  ret

}

