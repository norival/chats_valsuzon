# ------------------------------------------------------------------------------
# convert and rename all data files

source("functions/gpx_to_csv.R")
source("functions/rename_file.R")

library(magrittr)
library(stringi)

data_old_dir <- "data_old"

# define patterns to search associated with cat names
patterns <- c("baghera", "duchesse", "teddy", "jah", "mistigris", "berlioz", "fury", "symba", "kitkat", "zo[eé]")
cats     <- c("baghera", "duchesse", "teddy", "jah", "mistigris", "berlioz", "fury", "symba", "kitkat", "zoe")

for (src in list.files(data_old_dir, full.names = TRUE)) {
  # print the file name, it's cool
  print(src)

  # get file extension
  ext <-
    stri_split(src, fixed = ".", simplify = TRUE) %>%
    .[length(.)]

  # quickly check if file type is consistent with its extension
  a <- readLines(src, n = 1)

  if (grepl(a, pattern = "Date")) {
    file_type <- "csv"
  } else {
    file_type <- "gpx"
  }

  # if not, print the file name and stop the script
  if (file_type != ext) {
    paste("The file", src, "is marked as", ext, "but is not",
          "Please check that file!") %>%
      stop()
  }

  # get cat name
  for (i in patterns) {
    if (grepl(pattern = i, x = src, ignore.case = TRUE)) {
      cat_name <- cats[patterns == i]
      break
    }
    if (i == patterns[length(patterns)]) {
      print(src)
      stop("Cat name cannot be found in that file")
    }
  }

  if (ext == "gpx") {
    # file is gpx: convert it to csv and rename it
    gpx_to_csv(src, cat_name, output_dir = "data/")
  } else {
    rename_file(src, cat_name, "data/")
  }
}
