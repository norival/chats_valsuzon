# convert the data files and rename them
source("convert_data.R")

# load the data files
source("load_files.R")


library("adehabitatHR")
library("adehabitatLT")


# -- tests ---------------------------------------------------------------------

# simulate data
xy <- matrix(runif(60), ncol = 2)

# convert into sp object
xysp <- SpatialPoints(xy)

# convert it into a SpatialPolygonsDataFrame
clu <- clusthr(xysp)
plot(clu)

# second animal
xy2 <- matrix(runif(60), ncol = 2)

# bind the two together
xyt <- rbind(xy, xy2)

# generate id for the animals
id <- gl(2, 30)
# convert it to SpatialPointsDataFrame
idsp <- data.frame(id)
coordinates(idsp) <- xyt
class(idsp)

clu2 <- clusthr(idsp)
class(clu2)
length(clu2)
plot(clu2)

# ------------------------------------------------------------------------------
# exemple
data(puechabonsp)
names(puechabonsp)
head(as.data.frame(puechabonsp$relocs))

image(puechabonsp$map, col = grey(c(1:10/10)))
plot(puechabonsp$relocs, add = TRUE,
     col = as.data.frame(puechabonsp$relocs)[,1])

a

# ------------------------------------------------------------------------------

# jah
xy <- cbind(jah$Latitude, jah$Longitude)
head(xy)
xysp <- SpatialPoints(xy)
clu <- clusthr(xysp)
plot(clu)

# ------------------------------------------------------------------------------
devtools::install_github("dkahle/ggmap")
library(ggmap)

# plot(hadley)
map <- get_map(location = c(lon = 4.8936, lat = 47.4151),
               source = "google",
               zoo = 12,
               maptype = "terrain-background",
               scale = "auto")
p <- 
  ggmap(map)
p +
  geom_polygon(clu, aes()
