# -----------------------------------------------------------------------------
# fichier     : csv_to_gpx.R
# description : convertir fichier csv en fichier gpx pour l'application
#               CatTrack
#               La fonction crée un fichier gpx à partir des données contenues
#               dans le fichier csv
# utilisation : source("csv_to_gpx.R")
#               csv_to_gpx("fichier_source.csv", "nom_du_chat")
# packages    : il faut le package "magrittr"
# -----------------------------------------------------------------------------

csv_to_gpx <- function(src, cat_name) {

  # packages nécessaires
  library(magrittr)

  add_to_file <- function(...) {
    cat(..., sep = "", file = con, fill = TRUE)
  }

  # charger le fichier csv
  dat <- read.csv(src)

  # générer le nom pour le fichier
  file_name <-
    paste(cat_name, "_",
          min(as.Date(dat$Date)), "_",
          max(as.Date(dat$Date)),
          ".gpx",
          sep = "")

  # générer le tag 'description' pour le fichier gpx
  description <-
    paste(cat_name, "_",
          min(as.Date(dat$Date)), "_",
          max(as.Date(dat$Date)),
          sep = "")

  # création et ouverture
  con <- file(description = file_name, open = "w")

  # ajout l'en-tête xml dans le fichier
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>\n<gpx creator=\"Mobile Action http://www.mobileaction.com/1.1\" version=\"1.0\" xmlns=\"http://www.topografix.com/GPX/1/0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">" %>%
    add_to_file()

  # ajout des différents tags génériques
  "<trk>" %>% 
    add_to_file()

  paste("<name>", description, "</name>", sep = "") %>%
    add_to_file()

  "<desc>Color:004000ff</desc>" %>%
    add_to_file()

  "<trkseg>" %>%
    add_to_file()

  # ajout des données du csv ligne par ligne
  for (i in 1:nrow(dat)) {

    d <- dat[i, ]

    add_to_file("<trkpt lat=\"", d[["Latitude"]], "\" lon=\"", d[["Longitude"]], "\">")

    add_to_file("<ele>", d[["Altitude"]], "</ele>")

    substr(as.character(d[["Time"]]), 2, 9) %>%
      paste("<time>", as.Date(d[["Date"]]), "T", ., "Z", "</time>", sep = "") %>%
      add_to_file()

    paste("<speed>", d[["Speed"]], "</speed>", sep = "") %>%
      add_to_file()

    "</trkpt>" %>%
      add_to_file()
  }

  # fermeture des tags génériques
  "</trkseg>" %>%
    add_to_file()
  "</trk>" %>%
    add_to_file()
  "</gpx>" %>%
    add_to_file()

  # fermeture du fichier
  close(con)
}
