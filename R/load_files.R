# ------------------------------------------------------------------------------
# Read all data files that are in data/ and load the data.
#
# It loads the data into data.frames named after the cat's name.
# If a file containing data from a cat that has already been loaded, the new
# data are coerced with the old ones, so there is only one data.frame per cat.
# ------------------------------------------------------------------------------

library(stringi)
library(magrittr)

# First, clean the global environment so we can check if data about a cat has
# already been read
rm(list = ls())

for (src in list.files(path = "data", pattern = "csv$", full.names = TRUE)) {
  # get cat's name from file name
  cat_name <-
    stri_split_fixed(src, pattern = "/", simplify = TRUE)[2] %>%
    stri_split_fixed(., pattern = "_", simplify = TRUE) %>%
    .[1, 1]

  # check if this cat has already been read
  if (!(exists(cat_name))) {
    # this is the first file with this cat
    # we initialize a new data frame with the first data
    assign(cat_name, read.csv(src))
  } else {
    # this is not the first file that is read for this cat: the cat is back!
    # we coerce the new data with the old
    assign(cat_name, rbind.data.frame(get(cat_name), read.csv(src)))
  }
}
