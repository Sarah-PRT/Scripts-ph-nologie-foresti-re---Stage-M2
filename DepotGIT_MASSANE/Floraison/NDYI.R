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
path <- "D:/MASSANE/PIXEL/Floraison/EBI"

# Liste des fichiers CSV dans le répertoire
files <- list.files(path, pattern = "*.csv", full.names = TRUE)

# Vérifier s'il y a au moins 5 fichiers
if(length(files) >= 7) {
  # Lire les fichiers CSV avec un point-virgule comme séparateur
  df1 <- read_delim(files[1], delim = ",")
  df2 <- read_delim(files[2], delim = ",")
  df3 <- read_delim(files[3], delim = ",")
  df4 <- read_delim(files[4], delim = ",")
  df5 <- read_delim(files[5], delim = ",")
  df6 <- read_delim(files[6], delim = ",")
  df7 <- read_delim(files[7], delim = ",")
} else {
  stop("Le nombre de fichiers CSV est inférieur à 5.")
}

# Afficher les premières lignes de chaque dataframe pour vérification
print(head(df1))
print(head(df2))
print(head(df3))
print(head(df4))


# Combine all DataFrames
df <- rbind(df1, df2, df3, df4, df5, df6, df7)
rm("df1", "df2", "df3", "df4","df5", "df6", "df7")



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


# Sélectionner les colonnes désirées dans chaque DataFrame
multiband <- df[, c("time", "x", "y", "ndvi", "lonlat")]
# Sélectionner les colonnes désirées dans chaque DataFrame
RE <- df[, c("time", "x", "y", "ndvi", "lonlat")]


# Supposons que votre dataframe s'appelle df
# Assurez-vous que la colonne 'time' est de type Date
RE$time <- as.Date(RE$time)

# Remplacer les valeurs de NDVI inférieures à 0.1 par NA
RE$ndvi[RE$ndvi < 0.01] <- NA

# Afficher le dataframe filtré
head(RE)


# Appliquer l'interpolation linéaire par modalité de lonlat et suivant l'ordre de time
RE <- RE %>%
  group_by(lonlat) %>%
  arrange(time) %>%
  mutate(ndvi = na.approx(ndvi, na.rm = FALSE)) %>%
  ungroup()
print(RE)



library(ggplot2)
library(sf)
library(terra)
library(ggspatial)
library(paletteer)
# Charger le shapefile
shapefile_path <- "D:/MASSANE/PIXEL/Floraison/buffer_collecteur.shp"
shape_data <- st_read(shapefile_path)
# Assurez-vous que vos données sont au format spatial si nécessaire
# Si df est un DataFrame simple, vous pouvez le transformer en sf si vous avez des colonnes x et y
metrics <- st_as_sf(shape_data, coords = c("x", "y"), crs = 32631) # Adaptez le CRS si nécessaire
library(dplyr)
library(sf)

# Filtrer les lignes où la colonne ID commence par "JH" ou "VH"
metrics <- metrics[grepl("^JH|^VH", metrics$ID), ]

# Conversion de df en un objet sf
df_sf <- st_as_sf(RE, coords = c("x", "y"), crs = st_crs(metrics))

# Jointure spatiale pour associer chaque point avec le polygone auquel il appartient
df_with_polygons <- st_join(df_sf, metrics)

# Assurez-vous que la colonne RE est bien numérique
df_with_polygons$RE <- as.numeric(df_with_polygons$ndvi)

# Filtrer les NA de la colonne RE avant de calculer la moyenne
df_with_polygons <- df_with_polygons %>%
  dplyr::filter(!is.na(ndvi))

# Calculer la moyenne du NDVI pour chaque polygone et chaque date
mean_buff <- df_with_polygons %>%
  group_by(ID, time) %>%  # Grouper par l'ID du polygone et la date
  summarise(ndvi = mean(ndvi, na.rm = TRUE), .groups = "drop")  # Calculer la moyenne de NDVI


# Créer le graphique de séries temporelles NDVI pour chaque ID
ggplot(mean_buff, aes(x = time, y = ndvi, group = ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ ID, scales = "free_y") +  # Un graphique pour chaque ID
  labs(title = "Séries temporelles de NDVI par ID",
       x = "Date",
       y = "NDVI") +
  theme_minimal()


mean_buff_FLO <- mean_buff %>%
  dplyr::filter(format(time, "%Y") %in% c("2019", "2020", "2022"))


mean_buff_FLO <- mean_buff_FLO %>%
  mutate(DOY = yday(time))

library(dplyr)


#mean_buff_FLO_filtered <- mean_buff_FLO %>%
#  mutate(year = year(time)) %>%
#  dplyr::filter((year == 2019 & DOY >= 71 & DOY <= 160) |
#           (year == 2020 & DOY >= 76 & DOY <= 166) |
#           (year == 2022 & DOY >= 89 & DOY <= 154))


mean_buff_FLO_filtered <- mean_buff_FLO %>%
  mutate(year = year(time)) %>%
  dplyr::filter((year == 2019 & DOY >= 160 & DOY <= 200) |
                  (year == 2020 & DOY >= 166 & DOY <= 200) |
                  (year == 2022 & DOY >= 154 & DOY <= 200))


mean_buff_FLO_normalized <- mean_buff_FLO_filtered %>%
  group_by(ID, year) %>%
  mutate(ndvi_min = min(ndvi),
         ndvi_max = max(ndvi),
         ndvi_normalized = (ndvi - ndvi_min) / (ndvi_max - ndvi_min)) %>%
  ungroup() %>%
  dplyr::select(-ndvi_min, -ndvi_max)  # Optionnel: pour ne pas garder les colonnes temporaires

# Afficher les premières lignes du DataFrame normalisé pour vérifier
head(mean_buff_FLO_normalized)

mean_buff_FLO <- mean_buff_FLO %>%
  mutate(DOY = yday(time))

library(dplyr)

# Normaliser DOY de 0 à 1 pour chaque ID et année
mean_buff_FLO_time_normalized <- mean_buff_FLO_normalized %>%
  group_by(ID, year) %>%
  mutate(time_normalized = (DOY - min(DOY)) / (max(DOY) - min(DOY))) %>%
  ungroup()

# Afficher les premières lignes pour vérifier
head(mean_buff_FLO_time_normalized)

library(ggplot2)
library(signal)
library(dplyr)

# Appliquer le filtre Savitzky-Golay à chaque combinaison d'ID et d'année
smoothed_data <- mean_buff_FLO_time_normalized %>%
  dplyr::filter(year %in% c(2019, 2020, 2022)) %>%
  group_by(ID, year) %>%
  mutate(ndvi_sg = sgolayfilt(ndvi, p = 2, n = 3))  # Appliquer le filtre avec un polynôme d'ordre 3 et une fenêtre de 9 points

# Liste unique des IDs
unique_IDs <- unique(smoothed_data$ID)

# Boucle pour créer un graphique pour chaque ID
for (id in unique_IDs) {
  # Filtrer les données pour l'ID courant en utilisant dplyr::filter
  subset_data <- smoothed_data %>% dplyr::filter(ID == id)
  
  # Créer le graphique
  p <- ggplot(subset_data, 
              aes(x = time_normalized, y = ndvi_sg, color = as.factor(year))) +
    geom_line() +
    labs(title = paste("Comparaison du NDVI Normalisé pour ID:", id),
         x = "Temps Normalisé (0-1)",
         y = "NDVI Filtré (Savitzky-Golay)",
         color = "Année") +
    ylim(0, 1) +  # Fixer l'échelle de l'axe des Y entre 5 et 15
    theme_minimal()
  
  # Afficher le graphique
  print(p)
  
  # Pause pour visualiser chaque graphique séparément
  #readline(prompt="Appuyez sur [Entrée] pour continuer au graphique suivant...")
}



# Appliquer le filtre Savitzky-Golay à chaque combinaison d'ID et d'année
smoothed_data <- mean_buff_FLO_time_normalized %>%
  dplyr::filter(year %in% c(2019, 2020, 2022)) %>%
  group_by(ID, year) %>%
  mutate(ndvi_sg = sgolayfilt(ndvi, p = 2, n = 3))  # Appliquer le filtre avec un polynôme d'ordre 3 et une fenêtre de 9 points

# Liste unique des IDs
unique_IDs <- unique(smoothed_data$ID)

# Boucle pour créer un graphique pour chaque ID
for (id in unique_IDs) {
  # Filtrer les données pour l'ID courant en utilisant dplyr::filter
  subset_data <- smoothed_data %>% dplyr::filter(ID == id)
  
  # Créer le graphique
  p <- ggplot(subset_data, 
              aes(x = time_normalized, y = ndvi_sg, color = as.factor(year))) +
    geom_line(size = 1.2, alpha = 0.8) +  # Épaisseur des courbes et transparence
    scale_color_manual(values = c("2019" = "black", "2020" = "#5D478B", "2022" = "#4F94CD")) +  # Changer les couleurs des courbes
    labs(title = paste("NDYI", id),
         x = "Temps",
         y = "NDYI") +
    ylim(0, 0.8) +  # Fixer l'échelle de l'axe des Y entre 5 et 15
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),  # Ajouter un fond blanc à la zone de tracé
      plot.background = element_rect(fill = "white", color = NA),   # Ajouter un fond blanc à l'ensemble du graphique
      panel.border = element_rect(color = "#CDC653", fill = NA, size = 1),  # Ajouter un cadre rouge autour du graphique
      axis.ticks = element_line(color = "#CDC653"),  # Ajouter des ticks rouges
      axis.title.x = element_text(size = 22, face = "bold", color = "#CDC653"),  # Augmenter la taille et changer la couleur des titres des axes
      axis.title.y = element_text(size = 22, face = "bold", color = "#CDC653"),
      axis.text.x = element_text(size = 20, color = "#CDC653"),  # Augmenter la taille et changer la couleur des étiquettes des axes
      axis.text.y = element_text(size = 24, color = "#CDC653"),
      plot.title = element_text(size = 24, face = "bold", color = "#CDC653", hjust = 0.5),  # Augmenter la taille du titre du graphique
      legend.position = "none"  # Supprimer la légende
    )
  
  # Chemin complet pour enregistrer le fichier
  file_path <- paste0("D:/MASSANE/Sorties Stats/PROPRE/Floraison/NDYI_JUILLET_", id, ".png")
  
  # Enregistrer le graphique en tant que fichier PNG
  ggsave(file_path, plot = p, width = 4, height = 5, dpi = 300)
}






























































# Supposons que votre dataframe s'appelle df
# Assurez-vous que la colonne 'time' est de type Date
df$time <- as.Date(df$time)

# Filtrer les valeurs de NDVI >= 0.1 et les dates entre mai et juillet
df <- df[df$ndvi >= 0.1 & format(df$time, "%m") %in% c("05", "06", "07"), ]

# Afficher le dataframe filtré
head(df_filtered)


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
# Assurez-vous que vos données sont au format spatial si nécessaire
# Si df est un DataFrame simple, vous pouvez le transformer en sf si vous avez des colonnes x et y
metrics <- st_as_sf(shape_data, coords = c("x", "y"), crs = 32631) # Adaptez le CRS si nécessaire
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
#nombre_modalites <- length(unique(means_buff$ID))
#nombre_modalites

# Supprimer les lignes où ID est NA
#mean_buff <- mean_buff[!is.na(mean_buff$ID), ]

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

library(dplyr)

# Filtrer les données pour ne garder que celles de l'année 2019
mean_buff_2019 <- mean_buff %>%
  dplyr::filter(format(time, "%Y") == "2019")


library(ggplot2)

# Créer le graphique de séries temporelles NDVI pour chaque ID pour 2020
ggplot(mean_buff_2019, aes(x = time, y = ndvi, group = ID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ ID, scales = "free_y") +  # Un graphique pour chaque ID
  labs(title = "Séries temporelles de NDVI par ID en 2020",
       x = "Date",
       y = "NDVI") +
  theme_minimal()






