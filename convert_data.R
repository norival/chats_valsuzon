source("gpx_to_csv.R")
source("csv_to_gpx.R")
source("rename_file.R")

library(magrittr)
library(stringi)


# ------------------------------------------------------------------------------
for (src in list.files("data", full.names = TRUE)) {
  # get file extension
  ext <-
    stri_split(src, fixed = ".", simplify = TRUE) %>%
    .[length(.)]

  # get cat name by reading the file name
  # all_names <- c("baghera", "teddy", "jah", "mistigris", "berlioz", "fury", "symba", "kitkat", "zoe")
  if (grepl(pattern = "baghera", x = src, ignore.case = TRUE)) {
    cat_name <- "baghera"
  } else if (grepl(pattern = "teddy", x = src, ignore.case = TRUE)) {
    cat_name <- "teddy"
  } else if (grepl(pattern = "jah", x = src, ignore.case = TRUE)) {
    cat_name <- "jah"
  } else if (grepl(pattern = "mistigris", x = src, ignore.case = TRUE)) {
    cat_name <- "mistigris"
  } else if (grepl(pattern = "berlioz", x = src, ignore.case = TRUE)) {
    cat_name <- "berlioz"
  } else if (grepl(pattern = "fury", x = src, ignore.case = TRUE)) {
    cat_name <- "fury"
  } else if (grepl(pattern = "symba", x = src, ignore.case = TRUE)) {
    cat_name <- "symba"
  } else if (grepl(pattern = "kitkat", x = src, ignore.case = TRUE)) {
    cat_name <- "kitkat"
  } else if (grepl(pattern = "zo[eé]", x = src, ignore.case = TRUE)) {
    cat_name <- "zoe"
  } else if (grepl(pattern = "duchesse", x = src, ignore.case = TRUE)) {
    cat_name <- "duchesse"
  } else {
    cat_name <- "unknown"
  }
  if (cat_name == "unknown") 
    stop(cat(src))

  if (ext == "gpx") {
    # file is gpx: convert it to csv and rename it
    gpx_to_csv(src, cat_name)
    rename_file(src, cat_name)
  } else {
    rename_file(src, cat_name)
  }
}
