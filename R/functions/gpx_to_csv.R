# -----------------------------------------------------------------------------
# fichier     : gpx_to_csv.R
# description : convertir les fichiers gpx en fichiers csv, en accord avec les
#               autres fichiers csv générés par le CatTrack
#               La fonction crée un fichier csv contenant tous les relevés
#               contenu dans le fichier gpx. Il manque juste les valeurs
#               "Course", "Type", "Distance" et "Essential", qui ne sont pas
#               présentes dans le fichier gpx.
# utilisation : source("gpx_to_csv")
#               gpx_to_csv("fichier_source.gpx", "nom_du_chat")
# packages    : il faut les packages "stringi", "plotKML", "magrittr"
# -----------------------------------------------------------------------------

gpx_to_csv <- function(src, cat_name, output_dir = "conv/") {

  # packages nécessaires
  library(stringi)
  library(plotKML)
  library(magrittr)

  # charger le fichier gpx
  gpx <- readGPX(src)

  # créer un tableau vide pour accueillir les données
  dat <- data.frame(Date       = numeric(0),
                    Time       = numeric(0),
                    Latitude   = numeric(0),
                    Longitude  = numeric(0),
                    Altitude   = numeric(0),
                    Speed      = numeric(0),
                    Course     = numeric(0),
                    Type       = numeric(0),
                    Distance   = numeric(0),
                    Essential  = numeric(0))


  # récupérer les parcours issus du fichier gpx, formater les chaînes de
  # caractères pour qu'elles soient en accord avec les autres fichiers csv et
  # remplir le tableau
  for (track in gpx$tracks) {
    date <-
      track[[1]][["time"]] %>%
      stri_split(., fixed = "T", simplify = TRUE) %>%
      .[, 1] %>%
      stri_replace_all(., fixed = "-", replacement = "/")

    time <-
      track[[1]][["time"]] %>%
      stri_split(., fixed = "T", simplify = TRUE) %>%
      .[, 2] %>%
      stri_split(., fixed = "Z", simplify = TRUE) %>%
      .[, 1]

    dat_tmp <- data.frame(Date       = date,
                          Time       = time,
                          Latitude   = track[[1]][["lat"]],
                          Longitude  = track[[1]][["lon"]],
                          Altitude   = track[[1]][["ele"]],
                          Speed      = track[[1]][["speed"]],
                          Course     = NA,
                          Type       = NA,
                          Distance   = NA,
                          Essential  = NA)

    dat <- rbind.data.frame(dat, dat_tmp)

  }

  # écrire le tableau dans le fichier csv
  file_name <-
    paste(output_dir,
          cat_name, "_",
          min(as.Date(dat$Date)), "_",
          max(as.Date(dat$Date)),
          ".csv",
          sep = "")

  write.csv(dat, file_name, quote = FALSE, row.names = FALSE)
}
