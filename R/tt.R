
#' Create a new TidyTuesday directory
#'
#' @param year 4 digit year
#' @param week numeric week
#'
#' @export
tt_dir <- function(year = lubridate::year(Sys.Date()), week = lubridate::isoweek(Sys.Date()) + 1) {

  base_tt <- "/Users/jake/Projects/R/tidytuesdays"

  week <- sprintf("week%s", week)

  if (!dir.exists(fs::path(base_tt, year, week))) {

    # Create the Base Directory
    invisible(dir.create(fs::path(base_tt, year, week)))

    # Create the R folder
    invisible(dir.create(fs::path(base_tt, year, week, "R")))

    invisible(file.create(fs::path(base_tt, year, week, "R", "analysis.R")))

    cat(crayon::cyan(sprintf("Created TidyTuesday Directory and analysis.R for %s, %s", year, week)))

  } else {
    cat(crayon::yellow("Directory Already Exists"))
  }
}

#' Create a new 30DCC directory
#'
#' @param day 2 digit day
#'
#' @export
tdcc_dir <- function(day = lubridate::day(Sys.Date())) {

  base <- "/Users/jake/Projects/R/30dayChartChallenge"

  day <- sprintf("day%s", day)

  if (!dir.exists(fs::path(base, day))) {

    # Create the Base Directory
    invisible(dir.create(fs::path(base, day)))

    # Create the R folder
    invisible(dir.create(fs::path(base, day, "R")))

    invisible(file.create(fs::path(base, day, "R", paste0(day, ".R"))))

    cat(crayon::cyan(sprintf("Created 30DCC Directory and file for %s", day)))

  } else {
    cat(crayon::yellow("Directory Already Exists"))
  }
}
