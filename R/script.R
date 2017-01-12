# convert the data files and rename them
source("convert_data.R")

# load the data files
source("load_files.R")


library(adehabitatHR)
library(ggmap)


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

# get coordinates from cat dataframe
jah_xy <- cbind(jah$Latitude, jah$Longitude)

# convert the coordinates into spatial object
jah_xysp <- SpatialPoints(jah_xy)

# compute the habitat range with mcu method
jah_mc <- mcp(jah_xysp)

# get the map from google maps
# better looking maps can be obtained from stamen but it does not work all the
# time, might have some problems
map <- get_map(location = c(lon = 4.902, lat = 47.410),
               # source = "stamen",
               source = "google",
               zoom = 15,
               # urlonly = TRUE,
               maptype = "terrain",
               # maptype = "watercolor",
               filename = "maps/suzon")

# plot the map with the data of jah
# ggplot2 uses fortify() to get the coordinates from the spatial object
ggmap(map,
      extent = "device",
      base_layer = ggplot(data = jah_mc, aes(x = lat, y = long))) +
  geom_polygon(alpha = 0.7)
