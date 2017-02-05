# convert the data files and rename them
source("convert_data.R")

# load the data files
source("load_files.R")
addr <- read.csv("data/addr.csv", stringsAsFactors = FALSE)


library(adehabitatHR)
library(ggmap)
library(dplyr)


# ------------------------------------------------------------------------------
# plot cats data
#
# devtools::install_github("dkahle/ggmap")

# try to get the tiles from stamen mirrors
# map <- stri_replace_all(str = map, replacement = "http://c.b.", fixed = "http://")
# url(map[1])
# z <- tempfile()
# download.file(map[1], z, mode = "wb")
# pic <- readPNG(source = z)

# get the map from google maps
# better looking maps can be obtained from stamen but it does not work all the
# time, might have some problems
map <- get_map(location = c(lon = 4.902, lat = 47.410),
               source = "google",
               zoom = 15,
               maptype = "satellite",
               filename = "maps/suzon")


# ------------------------------------------------------------------------------
# map each territory on one map
#
# empty dataframe to store mcp results
cats_mc <- data.frame()

for (i in levels(as.factor(cats$cat_name))) {
  # get the data for the cat 'i'
  the_cat <- cats[cats$cat_name == i, ]

  # get the coordinates into a 'SpatialPoints' objects
  the_cat_xy <-
    cbind(the_cat$Latitude, the_cat$Longitude) %>%
    SpatialPoints()

  # compute the home range estimation
  the_cat_mc <-
    mcp(the_cat_xy) %>%
    fortify()
  # fill the id field with the name of the cat
  the_cat_mc$id <- i
  the_cat_mc$addr_lat   <- addr$lat[addr$cat_name == i]
  the_cat_mc$addr_long  <- addr$long[addr$cat_name == i]

  # add the dataframe to cats_mc dataframe
  cats_mc <- rbind.data.frame(cats_mc, fortify(the_cat_mc))

}

# plot the map
# map 1 with half of the cats
cats_mc1 <-
  cats_mc %>%
  filter(id %in% c("baghera", "berlioz", "duchesse", "fury"))
p <-
ggmap(map,
      extent = "device",
      base_layer = ggplot(data = cats_mc1, aes(x = lat, y = long))) +
  geom_polygon(colour = "red", alpha = 0.0) +
  geom_point(aes(x = addr_long, y = addr_lat, size = 3, color = "red")) +
  facet_wrap(~ id, nrow = 2) #+
p +
  guides(colour = FALSE,
         size = FALSE)

# map 2 with other half of the cats
cats_mc2 <-
  cats_mc %>%
  filter(!(id %in% c("baghera", "berlioz", "duchesse", "fury")))
ggmap(map,
      extent = "device",
      base_layer = ggplot(data = cats_mc2, aes(x = lat, y = long))) +
  geom_polygon(colour = "red", alpha = 0.0) +
  geom_point(aes(x = addr_long, y = addr_lat, size = 3, color = "red")) +
  facet_wrap(~ id, nrow = 2) #+

# ------------------------------------------------------------------------------
# group all territories into one big territory
# 
# cats_mc_global <-
#   cbind(cats$Latitude, cats$Longitude) %>%
#   SpatialPoints() %>%
#   mcp()
# 
# ggmap(map,
#       extent = "device",
#       base_layer = ggplot(data = cats_mc_global, aes(x = lat, y = long))) +
#   geom_polygon(alpha = 0.6)
#
# this makes strange results
