rename_file <- function(src, cat_name, output_dir = "conv/") {

  library(magrittr)
  library(stringi)

  # get file extension
  ext <- 
    stri_split(src, fixed = ".", simplify = TRUE) %>%
    .[length(.)]

  if (ext == "csv") {
    # the file is csv, just convert the Date column to a date object

    dat   <- read.csv(src)
    date  <- as.Date(dat$Date)

  } else if (ext == "gpx") {
    # the file is gpx, so I need to group dates from all tracks into one single
    # vector before getting the min and max dates

    library(plotKML)

    dat <- readGPX(src)

    # empty date vector to store dates
    date <- as.Date(character())

    for (track in dat$tracks) {
      # temporary vector with dates
      date_g <-
        track[[1]][["time"]] %>%
        stri_split(., fixed = "T", simplify = TRUE) %>%
        .[, 1] %>%
        stri_replace_all(., fixed = "-", replacement = "/")

      # put it in big vector
      date <- c(date, as.Date(date_g))
    }
  } else {
    # the file is neither gpx or csv, aborting
    stop("File format is not any of 'gpx' or 'csv', that's not cool bro!")
  }

  # create the name of the file
  file_name <-
    paste(output_dir,
          cat_name, "_",
          min(date), "_",
          max(date),
          ".", ext,
          sep = "")

  # open new file
  con <- file(file_name, open = "w")

  # copy the content of the file in it
  readLines(src) %>%
    writeLines(., con)

  # close the new file
  close(con)

}
