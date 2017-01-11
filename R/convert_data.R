source("gpx_to_csv.R")
source("csv_to_gpx.R")
source("rename_file.R")

library(magrittr)
library(stringi)


# ------------------------------------------------------------------------------
# convert and rename all data files

# define patterns to search associated with cat names
patterns <- c("baghera", "teddy", "jah", "mistigris", "berlioz", "fury", "symba", "kitkat", "zo[eé]")
cats     <- c("baghera", "teddy", "jah", "mistigris", "berlioz", "fury", "symba", "kitkat", "zoe")

for (src in list.files("data", full.names = TRUE)) {
  # get file extension
  ext <-
    stri_split(src, fixed = ".", simplify = TRUE) %>%
    .[length(.)]

  # get cat name
  for (i in all_names) {
    if (grepl(pattern = i, x = src, ignore.case = TRUE)) {
      cat_name <- cats[patterns == i]
      break
    }
    if (i == patterns[length(patterns)]) {
      print(src)
      stop("Cat name cannot be found in that file")
  }

  if (ext == "gpx") {
    # file is gpx: convert it to csv and rename it
    gpx_to_csv(src, cat_name)
    rename_file(src, cat_name)
  } else {
    rename_file(src, cat_name)
  }
}
