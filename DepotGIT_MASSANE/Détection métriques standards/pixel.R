# Charger les bibliothèques nécessaires
library(readxl)
library(readr)
library(tidyverse)
library(xts)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(imputeTS)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(readxl)

# Spécifier le chemin vers le répertoire contenant les fichiers CSV
path <- "E:/MASSANE/PIXEL"

# Liste des fichiers CSV dans le répertoire
files <- list.files(path, pattern = "*.csv", full.names = TRUE)

# Vérifier s'il y a au moins 5 fichiers
if(length(files) >= 5) {
  # Lire les fichiers CSV avec un point-virgule comme séparateur
  df1 <- read_delim(files[1], delim = ";")
  df2 <- read_delim(files[2], delim = ";")
  df3 <- read_delim(files[3], delim = ";")
  df4 <- read_delim(files[4], delim = ";")
  df5 <- read_delim(files[5], delim = ";")
} else {
  stop("Le nombre de fichiers CSV est inférieur à 5.")
}

# Afficher les premières lignes de chaque dataframe pour vérification
print(head(df1))
print(head(df2))
print(head(df3))
print(head(df4))
print(head(df5))


# Combine all DataFrames
df <- rbind(df1, df2, df3, df4, df5)
rm("df1", "df2", "df3", "df4", "df5")

# Convertir la colonne time en type date dans le dataframe VH_RI
df$time <- as.Date(df$time, format = "%d/%m/%Y")
# Extraire les années
annees <- unique(lubridate::year(df$time))


# Supposons que votre colonne time est de type date ou datetime, vous pouvez extraire l'année
df <- df %>%
  mutate(year = year(time))

# Supprimer les données pour l'année 2024
df <- df %>%
  dplyr::filter(year != 2024)

# Renommer la colonne
df <- df %>%
  rename(ndvi = spatial_ref)

# Créer une nouvelle colonne combinant longitude et latitude
df <- df %>%
  mutate(lon_lat = paste(x, y, sep = ", "))

# Créer un indice unique pour chaque combinaison lon_lat
df <- df %>%
  mutate(lonlat = as.numeric(factor(lon_lat)))

# Compter les NA dans 'ndvi' pour chaque modalité de 'lonlat'
na_count_per_modalite <- df %>%
  group_by(lonlat) %>%
  summarise(na_count = sum(is.na(ndvi)))
print(na_count_per_modalite)


# Appliquer l'interpolation linéaire par modalité de lonlat et suivant l'ordre de time
df <- df %>%
  group_by(lonlat) %>%
  arrange(time) %>%
  mutate(ndvi = na.approx(ndvi, na.rm = FALSE)) %>%
  ungroup()
print(df)

library(dplyr)
library(zoo)




library(dplyr)
library(signal)

# Supprimer les lignes avec des valeurs NA
df <- df[complete.cases(df$ndvi), ]
head(df)
df <- subset(df, !is.na(ndvi))

# colonne time de type date ou datetime, vous pouvez extraire l'année
df <- df %>%
  mutate(year = lubridate::year(time))

# Appliquer le filtre Savitzky-Golay par lonlat et année
df <- df %>%
  group_by(lonlat, year) %>%
  arrange(time) %>%
  mutate(SG_year = sgolayfilt(ndvi, p = 3, n = 9)) %>%
  ungroup()
print(df)



head(df)


library(dplyr)
library(ggplot2)

# Filtrer les données pour la modalité de lonlat 560
df_filtered <- df %>%
  dplyr::filter(lonlat == 560)

# Créer le graphique
ggplot(df_filtered, aes(x = time)) +
  geom_line(aes(y = ndvi, color = "NDVI")) +
  geom_line(aes(y = SG_year, color = "NDVI Filtré (SG)")) +
  labs(title = "NDVI et NDVI Filtré par Savitzky-Golay pour lonlat 560",
       x = "Temps",
       y = "NDVI") +
  scale_color_manual(name = "Légende", values = c("NDVI" = "blue", "NDVI Filtré (SG)" = "red")) +
  theme_minimal()



library(dplyr)
library(ggplot2)

# Filtrer les données pour la modalité de lonlat 560
df_filtered <- df %>%
  dplyr::filter(lonlat == 478)

# Créer le graphique avec facettes par année
ggplot(df_filtered, aes(x = time)) +
  geom_line(aes(y = ndvi, color = "NDVI")) +
  geom_line(aes(y = SG_year, color = "NDVI Filtré (SG)")) +
  labs(title = "NDVI et NDVI Filtré par Savitzky-Golay pour lonlat 560",
       x = "Temps",
       y = "NDVI") +
  scale_color_manual(name = "Légende", values = c("NDVI" = "blue", "NDVI Filtré (SG)" = "red")) +
  theme_minimal() +
  facet_wrap(~ year, scales = "free_x")


# Appliquer le lissage LOESS par lonlat et année
#df <- df %>%
#  group_by(lonlat, year) %>%
 # arrange(time) %>%
 # mutate(
#    LOESS_year = {
#      if(n() > 1) {
 #       # Conversion de time en numérique et suppression des NAs
  #      loess_fit <- loess(ndvi ~ as.numeric(time), data = ., span = 0.75, na.action = na.exclude)
  #      predict(loess_fit, as.numeric(time))
 #     } else {
 #       NA
 #     }
#   }
#  ) %>%
#  ungroup()






library(dplyr)

# Supposons que vh_ri est le dataframe contenant vos données initiales

# Calcul de l'amplitude de la variation annuelle pour chaque colonne par lonlat
annual_variation <- df %>%
  group_by(lonlat, year) %>%
  summarize(
    range_mean_ndvi = diff(range(ndvi, na.rm = TRUE)),
    range_SG = diff(range(SG_year, na.rm = TRUE))
  )

# Affichons les premières lignes de annual_variation
head(annual_variation)



library(dplyr)

# Calcul de l'amplitude de la variation annuelle pour chaque colonne par lonlat
annual_variation <- df %>%
  group_by(lonlat, year) %>%
  summarize(
    range_mean_ndvi = {
      valid_values <- ndvi[!is.na(ndvi)]
      if(length(valid_values) > 1) diff(range(valid_values, na.rm = TRUE)) else NA
    },
    range_SG = {
      valid_values <- SG_year[!is.na(SG_year)]
      if(length(valid_values) > 1) diff(range(valid_values, na.rm = TRUE)) else NA
    },
    .groups = "drop"
  )

# Afficher les premières lignes de annual_variation
head(annual_variation)


# Ajout de la colonne DOY si elle n'existe pas
if (!"DOY" %in% names(df)) {
  df <- df %>%
    mutate(DOY = yday(time))  # 'time' est supposé être une colonne de type Date ou POSIXct
}



library(dplyr)

# Jointure avec annual_variation par lonlat et year
df <- df %>%
  left_join(annual_variation, by = c("lonlat", "year")) %>%
  select(-matches("\\.x|\\.y"))

# Calcul des dérivées par lonlat et year
df <- df %>%
  group_by(lonlat, year) %>%
  arrange(DOY) %>%
  mutate(
    diff_ndvi = c(NA, diff(ndvi)),
    diff_SG_year = c(NA, diff(SG_year))
  ) %>%
  ungroup()

# Calcul des événements saisonniers pour chaque lonlat et année (points extrêmes)
season_extreme_50 <- df %>%
  group_by(lonlat, year) %>%
  summarize(
    SOS_ndvi = ifelse(any(diff_ndvi > 0 & DOY >= 80 & DOY <= 105), min(DOY[diff_ndvi > 0 & DOY >= 70 & DOY <= 100], na.rm = TRUE), NA),
    SOS_SG = ifelse(any(diff_SG_year > 0 & DOY >= 80 & DOY <= 105), min(DOY[diff_SG_year > 0 & DOY >= 70 & DOY <= 100], na.rm = TRUE), NA),
    POS_ndvi = ifelse(any(DOY >= 121 & DOY <= 171), max(DOY[DOY >= 121 & DOY <= 171 & ndvi == max(ndvi[DOY >= 121 & DOY <= 171], na.rm = TRUE)], na.rm = TRUE), NA),
    POS_SG = ifelse(any(DOY >= 121 & DOY <= 171), max(DOY[DOY >= 121 & DOY <= 171 & SG_year == max(SG_year[DOY >= 121 & DOY <= 171], na.rm = TRUE)], na.rm = TRUE), NA),
    EOS_ndvi = ifelse(any(diff_ndvi < 0 & DOY >= 288 & DOY <= 330), min(DOY[diff_ndvi < 0 & DOY >= 288 & DOY <= 330], na.rm = TRUE), NA),
    EOS_SG = ifelse(any(diff_SG_year < 0 & DOY >= 288 & DOY <= 330), min(DOY[diff_SG_year < 0 & DOY >= 288 & DOY <= 330], na.rm = TRUE), NA)
  ) %>%
  ungroup()


# Supposons que df contient les colonnes x, y, et lonlat
# season_extreme_50 contient également la colonne lonlat

# Joindre x et y de df à season_extreme_50
season_extreme_50 <- season_extreme_50 %>%
  left_join(df %>% select(lonlat, x, y), by = "lonlat")

# Afficher les premières lignes pour vérifier la jointure
head(season_extreme_50)


season_extreme_50 <- season_extreme_50[, !(names(season_extreme_50) %in% c("SOS_ndvi", "POS_ndvi", "EOS_ndvi"))]





# Supposons que season_extreme_50 contient la colonne 'year'

# Diviser season_extreme_50 par année
season_extreme_50_by_year <- split(season_extreme_50, season_extreme_50$year)

# Exemple pour accéder à un dataframe pour une année spécifique (par exemple, 2017)
HRI_2017 <- season_extreme_50_by_year[['2017']]
HRI_2018 <- season_extreme_50_by_year[['2018']]
HRI_2019 <- season_extreme_50_by_year[['2019']]
HRI_2020 <- season_extreme_50_by_year[['2020']]
HRI_2021 <- season_extreme_50_by_year[['2021']]
HRI_2022 <- season_extreme_50_by_year[['2022']]
HRI_2023 <- season_extreme_50_by_year[['2023']]





save_path <- "D:/MASSANE/DATA/PHENOLOGIE"

# Liste des dataframes à sauvegarder avec leurs noms de fichiers
dataframes_to_save <- list(
  HRI_2017 = HRI_2017,
  HRI_2018 = HRI_2018,
  HRI_2019 = HRI_2019,
  HRI_2020 = HRI_2020,
  HRI_2021 = HRI_2021,
  HRI_2022 = HRI_2022,
  HRI_2023 = HRI_2023
)

# Sauvegarder chaque dataframe dans un fichier CSV
for (name in names(dataframes_to_save)) {
  df <- dataframes_to_save[[name]]
  file_name <- paste0(name, ".csv")
  full_path <- file.path(save_path, file_name)
  write.csv(df, full_path, row.names = FALSE)
}

# Vérification de l'un des chemins de sauvegarde
print(full_path)
