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
path <- "D:/MASSANE/PIXEL/Floraison"

# Liste des fichiers CSV dans le répertoire
files <- list.files(path, pattern = "*.csv", full.names = TRUE)

# Vérifier s'il y a au moins 5 fichiers
if(length(files) >= 4) {
  # Lire les fichiers CSV avec un point-virgule comme séparateur
  df1 <- read_delim(files[1], delim = ",")
  df2 <- read_delim(files[2], delim = ",")
  df3 <- read_delim(files[3], delim = ",")
  df4 <- read_delim(files[4], delim = ",")
} else {
  stop("Le nombre de fichiers CSV est inférieur à 5.")
}

# Afficher les premières lignes de chaque dataframe pour vérification
print(head(df1))
print(head(df2))
print(head(df3))
print(head(df4))


# Combine all DataFrames
df <- rbind(df1, df2, df3, df4)
rm("df1", "df2", "df3", "df4")


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
  rename(ndvi = value)

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

# Supposons que vous avez un DataFrame nommé 'df' et que vous voulez conserver les colonnes 'colonne1', 'colonne2', et 'colonne3'
df <- df[, c("time", "x", "y", "ndvi", "lonlat")]


# Appliquer l'interpolation linéaire par modalité de lonlat et suivant l'ordre de time
df <- df %>%
  group_by(lonlat) %>%
  arrange(time) %>%
  mutate(ndvi = na.approx(ndvi, na.rm = FALSE)) %>%
  ungroup()
print(df)


library(ggplot2)
library(sf)
library(terra)
library(ggspatial)
library(paletteer)
# Charger le shapefile
shapefile_path <- "D:/MASSANE/PIXEL/Floraison/buffer_collecteur.shp"
shape_data <- st_read(shapefile_path)
#les données sont au format spatial si nécessaire
# Si df est un DataFrame simple, le transformer en sf si colonnes x et y
metrics <- st_as_sf(shape_data, coords = c("x", "y"), crs = 32631) # CRS si nécessaire
# Filtrer les lignes où la colonne ID commence par "JH" ou "VH"
metrics <- metrics[grepl("^JH|^VH", metrics$ID), ]



library(sf)

# Conversion de df en un objet sf
df_sf <- st_as_sf(df, coords = c("x", "y"), crs = st_crs(metrics))
# Jointure spatiale pour associer chaque point avec le polygone auquel il appartient
df_with_polygons <- st_join(df_sf, metrics)
library(dplyr)

# Calculer la moyenne du NDVI pour chaque polygone et chaque date
mean_buff <- df_with_polygons %>%
  group_by(ID, time) %>%  # Grouper par l'ID du polygone et la date
  summarise(ndvi = mean(ndvi, na.rm = TRUE))  # Calculer la moyenne de NDVI

# Compter le nombre de modalités dans ID
nombre_modalites <- length(unique(means_buff$ID))
nombre_modalites

# Supprimer les lignes où ID est NA
mean_buff <- mean_buff[!is.na(mean_buff$ID), ]

# Créer le graphique de séries temporelles NDVI pour chaque ID
ggplot(mean_buff, aes(x = time, y = ndvi, group = ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ ID, scales = "free_y") +  # Un graphique pour chaque ID
  labs(title = "Séries temporelles de NDVI par ID",
       x = "Date",
       y = "NDVI") +
  theme_minimal()

library(dplyr)

# Filtrer les données pour ne garder que celles de l'année 2020
mean_buff_2020 <- mean_buff %>%
  filter(format(time, "%Y") == "2018")


library(ggplot2)

# Créer le graphique de séries temporelles NDVI pour chaque ID pour 2020
ggplot(mean_buff_2020, aes(x = time, y = ndvi, group = ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ ID, scales = "free_y") +  # Un graphique pour chaque ID
  labs(title = "Séries temporelles de NDVI par ID en 2020",
       x = "Date",
       y = "NDVI") +
  theme_minimal()






