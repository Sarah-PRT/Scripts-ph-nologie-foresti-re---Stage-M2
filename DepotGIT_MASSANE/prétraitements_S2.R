setwd("E:/MASSANE/Sentinel-2")
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)

# Lire chaque fichier
Planetary <- read_delim("ROI_Hetre_RNN_Planetary.csv", delim = ";", locale = locale(decimal_mark = ","))

THEIA <- read_delim("ROI_HVC_RI.csv", delim = ";", locale = locale(decimal_mark = ","))

Dates <- read_delim("Dates_Sentinel2_2017.csv", delim = ";", locale = locale(decimal_mark = ","))

Planetary <- Planetary[ , !names(THEIA) %in% "...2"]
THEIA <- THEIA[ , !names(THEIA) %in% "...2"]
Dates <- Dates[ , !names(Dates) %in% "...2"]


Planetary <- Planetary %>% mutate_at(vars(2:7), as.numeric)
Planetary <- na.omit(Planetary)

THEIA <- THEIA %>% mutate_at(vars(2:7), as.numeric)
THEIA <- na.omit(THEIA)


Planetary$time <- as.Date(Planetary$time, format="%d/%m/%Y")
THEIA$time <- as.Date(THEIA$time, format="%d/%m/%Y")


nettoyer_mois <- function(data) {
  # Calculer les seuils minimaux, spécifiques à chaque mois
  seuils <- case_when(
    lubridate::month(data$time[1]) == 1 ~ c(0.02, 0.1), # Seuils pour janvier
    lubridate::month(data$time[1]) == 2 ~ c(0.1, 1),
    lubridate::month(data$time[1]) == 3 ~ c(0.1, 1),   
    lubridate::month(data$time[1]) == 4 ~ c(0.2, 1), 
    lubridate::month(data$time[1]) == 5 ~ c(0.3, 1),   
    lubridate::month(data$time[1]) == 6 ~ c(0.4, 1),
    lubridate::month(data$time[1]) == 7 ~ c(0.55, 1),
    lubridate::month(data$time[1]) == 8 ~ c(0.5, 1),
    lubridate::month(data$time[1]) == 9 ~ c(0.4, 1),
    lubridate::month(data$time[1]) == 10 ~ c(0.3, 1),
    lubridate::month(data$time[1]) == 11 ~ c(0.1, 1),
    lubridate::month(data$time[1]) == 12 ~ c(0.1, 0.3),
    TRUE ~ c(0, 1)  # Par défaut, pas de nettoyage
  )
  
  # Nettoyage et application aux autres colonnes 
  data_cleaned <- mutate_at(data, vars(mean_ndvi),
                            list(~ if_else(. < seuils[1] | . > seuils[2], NA_real_, .)))
  
  return(data_cleaned)
}

# Nettoyer les données mois par mois
THEIA <- THEIA %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(month = lubridate::month(time)) %>%
  do(nettoyer_mois(.)) %>%
  ungroup()

# Convertir la colonne "time" en format de date
THEIA <- THEIA %>%
  mutate(time = ymd(time))  # Utilisez dmy() pour le format jour/mois/année
# Trier les données par ordre chronologique
THEIA <- THEIA %>%
  arrange(time)
head(THEIA)

# Remplacer les lignes 2 à 6 par NA si la colonne "time" est NA
THEIA[is.na(THEIA$mean_ndvi), c("median_ndvi", "std_ndvi", "min_ndvi", "max_ndvi", "cv_ndvi")] <- NA

# Remplacement des valeurs spécifiées par NA
dates_a_remplacer <- as.Date(c("2017-09-10", "2016-03-19", "2019-11-04", "2018-01-13", "2019-10-20", "2018-11-29", "2018-09-25", "2018-01-03", "2020-01-18", "2021-01-02", "2017-09-10", "2017-09-25", "2018-01-08", "2018-09-25", "2019-08-26", "2019-09-15", "2019-10-10", "2020-01-13", "2020-01-18", "2020-11-13", "2021-01-02", "2021-01-07", "2022-01-07", "2019-10-20", "2019-11-04"))
THEIA <- THEIA %>%
  mutate_at(vars(-time), ~if_else(time %in% dates_a_remplacer, NA, .))


# Convertir la colonne "time" en format de date
Dates <- Dates %>%
  mutate(time = dmy(time))  # Utilisez dmy() pour le format jour/mois/année
#jointure by time
T <- left_join(Dates, THEIA, by = "time")


# Ajouter la colonne has_NA
T <- T %>%
  mutate(has_NA = is.na(mean_ndvi))

library(imputeTS)
T <- na_interpolation(T)
#Compter le nb de NA dans chaque colonne
sapply(T, function(x) sum(is.na(x)))



### Calcul des différence normalisées de la série de NDVI

# Définir la fenêtre de lissage gaussien
window_size <- 5 # Taille de la fenêtre
sigma <- 1  # Écart-type pour la fonction gaussienne

# Calculer les poids gaussiens
gaussian_weights <- dnorm(seq(-window_size, window_size), sd = sigma)

# Normaliser les poids pour qu'ils somment à 1
gaussian_weights <- gaussian_weights / sum(gaussian_weights)

# Appliquer le lissage gaussien à la colonne mean_ndvi
T$smoothed_mean_ndvi <- stats::filter(T$mean_ndvi, gaussian_weights, sides = 2)

# Calculer la moyenne et l'écart-type de mean_ndvi
mean_mean_ndvi <- mean(T$mean_ndvi, na.rm = TRUE)
sd_mean_ndvi <- sd(T$mean_ndvi, na.rm = TRUE)

# Calcul des z-scores
T <- T %>%
  mutate(z_score_mean_ndvi = (mean_ndvi - mean_mean_ndvi) / sd_mean_ndvi) %>%
  mutate(mean_ndvi = ifelse(z_score_mean_ndvi < -0.2, NA, mean_ndvi))

# Remplacer les valeurs manquantes de mean_ndvi par les valeurs correspondantes de smoothed_mean_ndvi
T$mean_ndvi <- ifelse(is.na(T$mean_ndvi), T$smoothed_mean_ndvi, T$mean_ndvi)


# Remplacer les premiers NA par la valeurs du dessous
T <- T %>%
  mutate(mean_ndvi = ifelse(is.na(mean_ndvi), first(mean_ndvi[!is.na(mean_ndvi)]), mean_ndvi))

# Remplacer les derniers NA par la valeurs du dessus
T <- T %>%
  fill(mean_ndvi, .direction = "downup")
#Compter le nb de NA dans chaque colonne
sapply(T, function(x) sum(is.na(x)))



library(openxlsx)

# Définir le chemin du fichier
#output_file <- "E:/MASSANE/Sentinel-2/CORR_Rapport_INTERP/HVC.xlsx"

# Enregistrer le dataframe THEIA dans un fichier Excel
#write.xlsx(T, file = output_file, sheetName = "Sheet1", overwrite = TRUE)

# Confirmation de l'enregistrement
#cat("Le fichier a été enregistré avec succès à", output_file)














