# ------------------------------------------------------------------------------
# Read all data files that are in data/ and load the data.
#
# It loads the data into data.frames named after the cat's name.
# Each data.frame is an element in the list 'cats'
# If a file containing data from a cat that has already been loaded, the new
# data are coerced with the old ones, so there is only one data.frame per cat.
# ------------------------------------------------------------------------------

library(stringi)
library(magrittr)

# First, clean the global environment so we can check if data about a cat has
# already been read
rm(list = ls())

# we create an empty list to store all cats data
cats <- data.frame()

for (src in list.files(path = "data", pattern = "[[:lower:]]+_[[:digit:]_]", full.names = TRUE)) {
  # get cat's name from file name
  cat_name <-
    stri_split_fixed(src, pattern = "/", simplify = TRUE)[2] %>%
    stri_split_fixed(., pattern = "_", simplify = TRUE) %>%
    .[1, 1]

  # adds the data to the dataframe
  cats <-
    read.csv(src) %>%
    cbind(., cat_name = cat_name) %>%
    rbind.data.frame(cats, .)

}

cats$Date <- as.character(cats$Date)
cats$Time <- as.character(cats$Time)
cats$cat_name <- as.character(cats$cat_name)
