# -- chargement des données ----------------------------------------------------
# conversion des données réçues vers le format csv
# ne lancer que si besoin
# source("convert_data.R")

# chargement des données
source("load_files.R")
addr <- read.csv("data/addr.csv", stringsAsFactors = FALSE)

# -- chargement des packages ---------------------------------------------------
library(adehabitatHR)
library(ggmap)
library(dplyr)
library(tikzDevice)

# -- couleurs personnalisées ---------------------------------------------------
bop     <- rgb(184, 65, 31, max = 255)
my_gray <- rgb(76, 76, 76, max = 255)

# graines pour la générations desnombres aléatoires des bootstraps
set.seed(42)


# -- cartes des territoires ----------------------------------------------------
# récupération du fond de carte sur google maps
map <- get_map(location = c(lon = 4.902, lat = 47.410),
               source = "google",
               zoom = 15,
               maptype = "satellite",
               filename = "maps/suzon")

# dataframe pour stocker les valeurs des MCP
cats_mc <- data.frame()

# dataframe pour stocker les valeurs de surfaces d'habitats
cats_area <-
  data.frame(cat_name = levels(as.factor(cats$cat_name)),
             surface  = numeric(length(levels(as.factor(cats$cat_name)))),
             stringsAsFactors = FALSE)

for (i in levels(as.factor(cats$cat_name))) {
  # cette boucle calcule les tailles d'habitat et renvoie les polygones de
  # chaque habitat

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
  # fill the address
  the_cat_mc$addr_lat   <- addr$lat[addr$cat_name == i]
  the_cat_mc$addr_long  <- addr$long[addr$cat_name == i]

  # add the dataframe to cats_mc dataframe
  cats_mc <- rbind.data.frame(cats_mc, fortify(the_cat_mc))

  # compute the area
  proj4string(the_cat_xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(the_cat_xy, CRS("+proj=utm +zone=31 ellps=WGS84"))

  the_cat_area <- mcp.area(res, unin = "m", unout = "km2", percent = 95, plotit = FALSE)
  cats_area$surface[cats_area$cat_name == i] <- the_cat_area$a
}

# plot the map
# map 1 with males
cats_mc1 <-
  cats_mc %>%
  filter(id %in% c("baghera", "duchesse", "fury", "jah", "zoe")) %>%
  mutate(id = as.factor(id))

# get names for better looking output
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
# ggsave(filename = "../../report/img/map_females.jpg", plot = p)
ggsave(filename = "img/map_females.jpg", plot = p)

# map 2 with females
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

# Taillles d'habitat
cats_area$sex <- c("female", "male", "female", "female", "female", "male", "male", "male", "male", "female")
cats_area$castrated <- c("N", "N", "O", "N", "O", "O", "N", "O", "O", "O")


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
  geom_errorbarh(aes(xmin = inf, xmax = sup), height = 0.2) +
  geom_point(size = 4, colour = bop) +
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
  geom_errorbarh(aes(xmin = inf, xmax = sup), height = 0.2) +
  geom_point(size = 4, colour = bop) +
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

tikz("../../report/img/bootstrap_homerange.tex", width = 4, height = 2)
plot(p)
dev.off()


# temps en forêt
source("functions/forest.R")
# rectangle <- data.frame(x = c(1, 2, 2, 1),
#                         y = c(1, 1, 2, 2))
# point <- c(0.5, 0.5)
# point <- c(1.5, 1.5)
# point <- c(1.5, 2.1)
# p_in_rectangle(p = point, rectangle = rectangle)

# x <- sample(x = 0:5, replace = TRUE, size = 100)
# y <- sample(x = 0:5, replace = TRUE, size = 100)

# cc <- cbind(x, y)
# percent_in_forest(cc, rectangle)

forest1 <- data.frame(x = c(47.406915, 47.417228, 47.421944, 47.413083),
                      y = c(4.889973, 4.909132, 4.903603, 4.884238))

forest2 <- data.frame(x = c(47.404284, 47.406217, 47.402600, 47.396131),
                      y = c(4.887653, 4.893342, 4.896097, 4.883259))

forest3 <- data.frame(x = c(47.406855, 47.416759, 47.412726, 47.403206),
                      y = c(4.897497, 4.915226, 4.918545, 4.901372))

forests <- list(forest1, forest2, forest3)


cats$sex <- character(nrow(cats))
cats$sex[cats$cat_name == "baghera"]    <- "female"
cats$sex[cats$cat_name == "berlioz"]    <- "male"
cats$sex[cats$cat_name == "duchesse"]   <- "female"
cats$sex[cats$cat_name == "fury"]       <- "female"
cats$sex[cats$cat_name == "jah"]        <- "female"
cats$sex[cats$cat_name == "kitkat"]     <- "male"
cats$sex[cats$cat_name == "mistigris"]  <- "male"
cats$sex[cats$cat_name == "symba"]      <- "male"
cats$sex[cats$cat_name == "teddy"]      <- "male"
cats$sex[cats$cat_name == "zoe"]        <- "female"

cats$statut <- character(nrow(cats))
cats$statut[cats$cat_name == "baghera"]    <- "n_castr"
cats$statut[cats$cat_name == "berlioz"]    <- "n_castr"
cats$statut[cats$cat_name == "duchesse"]   <- "castr"
cats$statut[cats$cat_name == "fury"]       <- "n_castr"
cats$statut[cats$cat_name == "jah"]        <- "castr"
cats$statut[cats$cat_name == "kitkat"]     <- "castr"
cats$statut[cats$cat_name == "mistigris"]  <- "n_castr"
cats$statut[cats$cat_name == "symba"]      <- "castr"
cats$statut[cats$cat_name == "teddy"]      <- "castr"
cats$statut[cats$cat_name == "zoe"]        <- "castr"

# temps moyen en forêt par sexe
cats_female <-
  cats %>%
  filter(sex == "female") %>%
  select(x = Latitude, y = Longitude)

perc_forest_females <- 0
for (i in 1:length(forests)) {
  perc_forest_females <- perc_forest_females + percent_in_forest(cats_female, forests[[i]])
}

cats_male <-
  cats %>%
  filter(sex == "male") %>%
  select(x = Latitude, y = Longitude)

perc_forest_males <- 0
for (i in 1:length(forests)) {
  perc_forest_males <- perc_forest_males + percent_in_forest(cats_male, forests[[i]])
}

# temps moyen en forêt par statut
cats_castr <-
  cats %>%
  filter(statut == "castr") %>%
  select(x = Latitude, y = Longitude)

perc_forest_castr <- 0
for (i in 1:length(forests)) {
  perc_forest_castr <- perc_forest_castr + percent_in_forest(cats_castr, forests[[i]])
}

cats_n_castr <-
  cats %>%
  filter(statut == "n_castr") %>%
  select(x = Latitude, y = Longitude)

perc_forest_n_castr <- 0
for (i in 1:length(forests)) {
  perc_forest_n_castr <- perc_forest_n_castr + percent_in_forest(cats_n_castr, forests[[i]])
}

percent_forest <-
  data.frame(cat_name = levels(as.factor(cats$cat_name)),
             perc_forest = numeric(length(levels(as.factor(cats$cat_name)))))

for (name in levels(as.factor(cats$cat_name))) {

  perc_forest <- 0
  the_cat <-
    cats %>%
    filter(cat_name == name) %>%
    select(x = Latitude, y = Longitude)

  for (i in 1:length(forests)) {
    perc_forest <- perc_forest + percent_in_forest(the_cat, forests[[i]])
  }

  percent_forest$perc_forest[percent_forest$cat_name == name] <- perc_forest

}

percent_forest$sex <- character(nrow(percent_forest))
percent_forest$sex[percent_forest$cat_name == "baghera"]    <- "female"
percent_forest$sex[percent_forest$cat_name == "berlioz"]    <- "male"
percent_forest$sex[percent_forest$cat_name == "duchesse"]   <- "female"
percent_forest$sex[percent_forest$cat_name == "fury"]       <- "female"
percent_forest$sex[percent_forest$cat_name == "jah"]        <- "female"
percent_forest$sex[percent_forest$cat_name == "kitkat"]     <- "male"
percent_forest$sex[percent_forest$cat_name == "mistigris"]  <- "male"
percent_forest$sex[percent_forest$cat_name == "symba"]      <- "male"
percent_forest$sex[percent_forest$cat_name == "teddy"]      <- "male"
percent_forest$sex[percent_forest$cat_name == "zoe"]        <- "female"

percent_forest$statut <- character(nrow(percent_forest))
percent_forest$statut[percent_forest$cat_name == "baghera"]    <- "n_castr"
percent_forest$statut[percent_forest$cat_name == "berlioz"]    <- "n_castr"
percent_forest$statut[percent_forest$cat_name == "duchesse"]   <- "castr"
percent_forest$statut[percent_forest$cat_name == "fury"]       <- "n_castr"
percent_forest$statut[percent_forest$cat_name == "jah"]        <- "castr"
percent_forest$statut[percent_forest$cat_name == "kitkat"]     <- "castr"
percent_forest$statut[percent_forest$cat_name == "mistigris"]  <- "n_castr"
percent_forest$statut[percent_forest$cat_name == "symba"]      <- "castr"
percent_forest$statut[percent_forest$cat_name == "teddy"]      <- "castr"
percent_forest$statut[percent_forest$cat_name == "zoe"]        <- "castr"

# ------------------------------------------------------------------------------
# moyennes bootstrapées pour le temps passé en forêt
nbboot <- 10000

males <-
  percent_forest %>%
  filter(sex == "male") %>%
  .$perc_forest
moyboot_males <- numeric(nbboot)

for (i in 1:nbboot) {
  x2          <- sample(males, replace = TRUE)
  moyboot_males[i]  <- mean(x2)
}

females <-
  percent_forest %>%
  filter(sex == "female") %>%
  .$perc_forest
moyboot_females <- numeric(nbboot)

for (i in 1:nbboot) {
  x2          <- sample(females, replace = TRUE)
  moyboot_females[i]  <- mean(x2)
}

castr <-
  percent_forest %>%
  filter(statut == "castr") %>%
  .$perc_forest
moyboot_castr <- numeric(nbboot)

for (i in 1:nbboot) {
  x2          <- sample(castr, replace = TRUE)
  moyboot_castr[i]  <- mean(x2)
}

non_castr <-
  percent_forest %>%
  filter(statut == "n_castr") %>%
  .$perc_forest
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
  geom_errorbarh(aes(xmin = inf, xmax = sup), height = 0.2) +
  geom_point(size = 4, colour = bop) +
  xlab("Moyenne bootstrapée (\\%)") +
  ylab("") +
  # xlim(-0.05, 0.7) +
  geom_vline(xintercept = 0, lty = "dotted") +
  theme_bw() +
  theme(panel.grid    = element_blank(),
        axis.line.x   = element_line(size = 0.5, colour = "black"),
        axis.text   = element_text(size = 9),
        axis.ticks.y  = element_blank(),
        axis.title    = element_text(size = 10),
        panel.border  = element_blank())

tikz("../../report/img/bootstrap_forest.tex", width = 4, height = 2)
plot(p)
dev.off()


# ------------------------------------------------------------------------------
# description du jeu de données
source("functions/tab2latex.R")

cats$Date <- as.Date(cats$Date)
cats_summary <-
  cats %>%
  group_by(cat_name) %>%
  summarise(date_min  = min(Date),
            date_max  = max(Date),
            duration  = date_max - date_min,
            n_obs     = length(Date)) %>%
  arrange(desc(duration))

# tab2latex(x = cats_summary,
#           fileName = "../../report/tables/summary_cats.tex",
#           col.names = c("Chat", "Première date", "Dernière date", "Durée", "n"),
#           align = "lRRrr")
