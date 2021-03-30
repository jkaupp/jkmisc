
tt_dir <- function(year = lubridate::year(Sys.Date()), week = lubridate::isoweek(Sys.Date()) + 1) {

  base_tt <- "/Users/jake/Projects/R/tidytuesdays"

  week <- sprintf("week%s", week)

  if (!dir.exists(fs::path(base_tt, year, week))) {

    # Create the Base Directory
    invisible(dir.create(fs::path(base_tt, year, week)))

    # Create the R folder
    invisible(dir.create(fs::path(base_tt, year, week, "R")))

    invisible(file.create(fs::path(base_tt, year, week, "R", "analysis.R")))

    cat(crayon::cyan("Created TidyTuesday Directory and analysis.R for %s, %s", year, week))

  } else {
    cat(crayon::yellow("Directory Already Exists"))
  }

}
