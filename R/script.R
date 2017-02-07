# convert the data files and rename them
source("convert_data.R")

# load the data files
source("load_files.R")
addr <- read.csv("data/addr.csv", stringsAsFactors = FALSE)


library(adehabitatHR)
library(ggmap)
library(dplyr)

# couleurs
bop     <- rgb(184, 65, 31, max = 255)
my_gray <- rgb(76, 76, 76, max = 255)

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
cats_area <-
  data.frame(cat_name = levels(as.factor(cats$cat_name)),
             surface = numeric(length(levels(as.factor(cats$cat_name)))),
             stringsAsFactors = FALSE)

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
  # mcp(the_cat_xy, unin = "km", unout = "m2") %>%
  #   as.data.frame() %>%
  #   print()
  # fill the id field with the name of the cat
  the_cat_mc$id <- i
  the_cat_mc$addr_lat   <- addr$lat[addr$cat_name == i]
  the_cat_mc$addr_long  <- addr$long[addr$cat_name == i]

  # add the dataframe to cats_mc dataframe
  cats_mc <- rbind.data.frame(cats_mc, fortify(the_cat_mc))

  # compute the area
  proj4string(the_cat_xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(the_cat_xy, CRS("+proj=utm +zone=31 ellps=WGS84"))
  the_cat_area <- mcp.area(res, unin = "m", unout = "km2", percent = 95)
  cats_area$surface[cats_area$cat_name == i] <- the_cat_area$a

}

# xy <- data.frame(ID = 1:2, X = c(118, 119), Y = c(10, 50))
# coordinates(xy) <- c("X", "Y")
# proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example

# res <- spTransform(xy, CRS("+proj=utm +zone=31 ellps=WGS84"))
# res

# xy <- the_cat_xy
# proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
# res <- spTransform(xy, CRS("+proj=utm +zone=31 ellps=WGS84"))
# mcp.area(res, unin = "m", unout = "m2")

# proj4string <- CRS("+proj=utm +zone=17N +ellps=WGS84")
# the_cat_xy <-
#   cbind(the_cat$Latitude, the_cat$Longitude) %>%
#   SpatialPoints(., proj4string = proj4string)

# plot the map
# map 1 with half of the cats
cats_mc1 <-
  cats_mc %>%
  filter(id %in% c("baghera", "duchesse", "fury", "jah", "zoe")) %>%
  mutate(id = as.factor(id))

levels(cats_mc1$id)[levels(cats_mc1$id) == "baghera"]   <- "Baghera"
levels(cats_mc1$id)[levels(cats_mc1$id) == "duchesse"]  <- "Duchesse"
levels(cats_mc1$id)[levels(cats_mc1$id) == "fury"]      <- "Fury"
levels(cats_mc1$id)[levels(cats_mc1$id) == "jah"]       <- "Jah"
levels(cats_mc1$id)[levels(cats_mc1$id) == "zoe"]       <- "Zoé"

p <-
ggmap(map,
      extent = "device",
      base_layer = ggplot(data = cats_mc1, aes(x = lat, y = long, group = group))) +
  geom_polygon(colour = "red", fill = "red", alpha = 0.4) +
  geom_point(aes(x = addr_long, y = addr_lat, size = 2.5), color = "green", fill = "green", shape = 25) +
  facet_wrap(~ id, nrow = 2) +
  guides(colour = FALSE,
         size = FALSE,
         fill = FALSE) +
  theme(strip.text = element_text(size = rel(2)))
ggsave(filename = "../../report/img/map_females.jpg", plot = p)

# map 2 with other half of the cats
cats_mc2 <-
  cats_mc %>%
  filter(!(id %in% c("baghera", "duchesse", "fury", "jah", "zoe"))) %>%
  mutate(id = as.factor(id))

levels(cats_mc2$id)[levels(cats_mc2$id) == "berlioz"]   <- "Berlioz"
levels(cats_mc2$id)[levels(cats_mc2$id) == "kitkat"]    <- "Kitkat"
levels(cats_mc2$id)[levels(cats_mc2$id) == "mistigris"] <- "Mistigris"
levels(cats_mc2$id)[levels(cats_mc2$id) == "symba"]     <- "Symba"
levels(cats_mc2$id)[levels(cats_mc2$id) == "teddy"]     <- "Teddy"

p <-
  ggmap(map,
        extent = "device",
        base_layer = ggplot(data = cats_mc2, aes(x = lat, y = long))) +
  geom_polygon(colour = "red", fill = "red", alpha = 0.4) +
  geom_point(aes(x = addr_long, y = addr_lat, size = 2.5), color = "green", fill = "green", shape = 25) +
  facet_wrap(~ id, nrow = 2) +
  guides(colour = FALSE,
         size = FALSE,
         fill = FALSE) +
  theme(strip.text = element_text(size = rel(2)))
ggsave(filename = "../../report/img/map_males.jpg", plot = p)

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


# Taillles d'habitat
cats_area$sex <- c("female", "male", "female", "female", "female", "male", "male", "male", "male", "female")
cats_area$castrated <- c("N", "N", "O", "N", "O", "O", "N", "O", "O", "O")

cats_area %>%
  ggplot(aes(x = sex, y = surface, fill = castrated)) +
  geom_boxplot()


# -- bootstrap -----------------------------------------------------------------

dcohen <- function(v1, v2) {

  m1  <- mean(v1)
  sd1 <- sd(v1)
  n1  <- length(v1)

  m2  <- mean(v2)
  sd2 <- sd(v2)
  n2  <- length(v2)

  sp <- sqrt(((n2 - 1) * sd2^2 + (n1 - 1) * sd1^2) / (n1 + n2 - 2))

  d <- (m2 - m1) / sp

  return(d)
}

dcohen_ic95 <- function(v1, v2, nb_boot = 10000) {

  # bootstrap
  boot <- numeric(nb_boot)

  for (i in 1:nb_boot) {
    v1bis <- sample(v1, replace = TRUE)
    v2bis <- sample(v2, replace = TRUE)

    boot[i] <- dcohen(v1bis, v2bis)
  }

  # get the true dcohen and the ic95
  return(c(dcohen(v1, v2), quantile(boot, c(0.025, 0.975))))
}


# grouper par sexe
males <-
  cats_area %>%
  filter(sex == "male") %>%
  .$surface

females <-
  cats_area %>%
  filter(sex == "female") %>%
  .$surface

castrated <-
  cats_area %>%
  filter(castrated == "O") %>%
  .$surface

non_castrated <-
  cats_area %>%
  filter(castrated == "N") %>%
  .$surface

sex <- dcohen_ic95(males, females)
cas <- dcohen_ic95(castrated, non_castrated)
res_d <- data.frame(comp = c("Sexe", "Statut"),
                    d = c(sex[1], cas[1]),
                    inf = c(sex[2], cas[2]),
                    sup = c(sex[3], cas[3]))

p <-
  res_d %>%
  ggplot(aes(x = d, y = comp)) +
  geom_point(size = 4, colour = bop) +
  geom_errorbarh(aes(xmin = inf, xmax = sup), height = 0.2) +
  geom_vline(xintercept = 0, lty = "dotted") +
  xlab("\\textit{d} de Cohen") +
  ylab("") +
  theme_bw() +
  theme(panel.grid    = element_blank(),
        axis.line.x   = element_line(size = 0.5, colour = "black"),
        axis.text   = element_text(size = 9),
        axis.ticks.y  = element_blank(),
        axis.title    = element_text(size = 10),
        panel.border  = element_blank())

tikz("../../report/img/cohen.tex", width = 4, height = 2)
plot(p)
dev.off()

# ------------------------------------------------------------------------------
# moyennes bootstrapées
nbboot <- 10000

males <-
  cats_area %>%
  filter(sex == "male") %>%
  .$surface
moyboot_males <- numeric(nbboot)

for (i in 1:nbboot) {
  x2          <- sample(males, replace = TRUE)
  moyboot_males[i]  <- mean(x2)
}

females <-
  cats_area %>%
  filter(sex == "female") %>%
  .$surface
moyboot_females <- numeric(nbboot)

for (i in 1:nbboot) {
  x2          <- sample(females, replace = TRUE)
  moyboot_females[i]  <- mean(x2)
}

castr <-
  cats_area %>%
  filter(castrated == "O") %>%
  .$surface
moyboot_castr <- numeric(nbboot)

for (i in 1:nbboot) {
  x2          <- sample(castr, replace = TRUE)
  moyboot_castr[i]  <- mean(x2)
}

non_castr <-
  cats_area %>%
  filter(castrated == "N") %>%
  .$surface
moyboot_non_castr <- numeric(nbboot)

for (i in 1:nbboot) {
  x2          <- sample(non_castr, replace = TRUE)
  moyboot_non_castr[i]  <- mean(x2)
}

res_boot <- data.frame(bootstrap = c("Mâles", "Femelles", "Castrés", "Non castrés"),
                       mean_boot = c(mean(moyboot_males),
                                     mean(moyboot_females),
                                     mean(moyboot_castr),
                                     mean(moyboot_non_castr)),
                       inf = c(quantile(moyboot_males, c(0.025, 0.975))[1],
                               quantile(moyboot_females, c(0.025, 0.975))[1],
                               quantile(moyboot_castr, c(0.025, 0.975))[1],
                               quantile(moyboot_non_castr, c(0.025, 0.975))[1]),
                       sup = c(quantile(moyboot_males, c(0.025, 0.975))[2],
                               quantile(moyboot_females, c(0.025, 0.975))[2],
                               quantile(moyboot_castr, c(0.025, 0.975))[2],
                               quantile(moyboot_non_castr, c(0.025, 0.975))[2]))

res_boot$bootstrap <- factor(res_boot$bootstrap, levels = c("Mâles", "Femelles", "Castrés", "Non castrés"))

p <-
  res_boot %>%
  ggplot(aes(x = mean_boot, y = bootstrap)) +
  geom_point(size = 4, colour = bop) +
  geom_errorbarh(aes(xmin = inf, xmax = sup), height = 0.2) +
  xlab("Moyenne bootstrapée (km\\textsuperscript{2})") +
  ylab("") +
  xlim(-0.05, 0.7) +
  geom_vline(xintercept = 0, lty = "dotted") +
  theme_bw() +
  theme(panel.grid    = element_blank(),
        axis.line.x   = element_line(size = 0.5, colour = "black"),
        axis.text   = element_text(size = 9),
        axis.ticks.y  = element_blank(),
        axis.title    = element_text(size = 10),
        panel.border  = element_blank())

library(tikzDevice)
tikz("../../report/img/bootstrap.tex", width = 4, height = 2)
plot(p)
dev.off()
