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

# Définir le répertoire de travail
setwd("E:/MASSANE/Sentinel-2/NDVI/Séries Temporelles")

# Lire chaque fichier
VH_RI <- read_delim("VH_RI_2016_23.csv", delim = ";", locale = locale(decimal_mark = ","))
JH_RI <- read_delim("JH_RI_2016_23.csv", delim = ";", locale = locale(decimal_mark = ","))
HVC_RI <- read_delim("HVC_RI_2016_23.csv", delim = ";", locale = locale(decimal_mark = ","))
CH_RI <- read_delim("CH_RI_2016_23.csv", delim = ";", locale = locale(decimal_mark = ","))
PC_RI <- read_delim("PC_RI_2016_23.csv", delim = ";", locale = locale(decimal_mark = ","))
Dates <- read_delim("Dates_Sentinel2.csv", delim = ";", locale = locale(decimal_mark = ","))

# Convertir les colonnes 2 à 6 en numérique
VH_RI <- VH_RI %>% mutate_at(vars(2:6), as.numeric)
JH_RI <- JH_RI %>% mutate_at(vars(2:6), as.numeric)
HVC_RI <- HVC_RI %>% mutate_at(vars(2:6), as.numeric)
CH_RI <- CH_RI %>% mutate_at(vars(2:6), as.numeric)
PC_RI <- PC_RI %>% mutate_at(vars(2:6), as.numeric)

library(dplyr)

VH_RI <- left_join(Dates, VH_RI, by = "time")
CH_RI <- left_join(Dates, CH_RI, by = "time")
HVC_RI <- left_join(Dates, HVC_RI, by = "time")
JH_RI <- left_join(Dates, JH_RI, by = "time")
PC_RI <- left_join(Dates, PC_RI, by = "time")

# Vérifier le nombre de valeurs non-NA dans la colonne "mean_ndvi"
#VH_RI <- sum(!is.na(VH_RI$mean_ndvi))
#print(VH_RI)


library(dplyr)

# Modifier les valeurs de toutes les colonnes sauf la première (time)
VH_RI <- VH_RI %>% 
  mutate(across(-time, ~ if_else(. == 0, NA_real_, .)))

# Afficher le résultat
print(VH_RI)


JH_RI <- JH_RI %>% mutate(
  across(2:6, ~ if_else(JH_RI[[2]] == 0, NA_real_, .))
)

HVC_RI <- HVC_RI %>% mutate(
  across(2:6, ~ if_else(HVC_RI[[2]] == 0, NA_real_, .))
)

CH_RI <- CH_RI %>% mutate(
  across(2:6, ~ if_else(CH_RI[[2]] == 0, NA_real_, .))
)

PC_RI <- PC_RI %>% mutate(
  across(2:6, ~ if_else(PC_RI[[2]] == 0, NA_real_, .))
)

# temporal linear interpolation
CH_RI <- na_interpolation(CH_RI)
HVC_RI <- na_interpolation(HVC_RI)
JH_RI <- na_interpolation(JH_RI)
PC_RI <- na_interpolation(PC_RI)
VH_RI <- na_interpolation(VH_RI)

# combiner uniquement la colonne 2 de chaque dataframe
ndvi_mean <- cbind(VH_RI[[1]], VH_RI[[2]], JH_RI[[2]], HVC_RI[[2]], CH_RI[[2]], PC_RI[[2]])

# Renommer les colonnes si nécessaire
colnames(ndvi_mean) <- c("date", "VH_RI", "JH_RI", "HVC_RI", "CH_RI", "PC_RI")  # Remplacez "colonne1", "colonne2", ... par les noms appropriés

library(tidyr)
library(ggplot2)

# Assurez-vous que ndvi_mean est un dataframe
ndvi_mean <- as.data.frame(ndvi_mean)

# Charger la bibliothèque tidyr
library(tidyr)

# Convertir le dataframe en format long
ndvi_mean_long <- ndvi_mean %>%
  gather(key = "variable", value = "value", -date)

# Assurez-vous que ndvi_mean est un dataframe
ndvi_mean <- as.data.frame(ndvi_mean)

# Charger les bibliothèques nécessaires
library(tidyr)
library(ggplot2)

# Convertir le dataframe en format long
ndvi_mean_long <- ndvi_mean %>%
  gather(key = "variable", value = "value", -date)

library(ggplot2)

# Convertir la colonne 'date' en classe 'Date'
ndvi_mean_long$date <- as.Date(ndvi_mean_long$date, format = "%d/%m/%Y")
library(dplyr)

# Convertir la colonne "value" en numérique
ndvi_mean_long <- ndvi_mean_long %>%
  mutate(value = as.numeric(value))

# Vérifier la structure de ndvi_mean_long après la conversion
str(ndvi_mean_long)

# Tracer un plot par variable avec la date en axe x
ggplot(ndvi_mean_long, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  labs(title = "Chroniques du NDVI", x = "Date", y = "Valeur") +
  theme_minimal()




library(ggplot2)

# Convertir la colonne 'date' en classe 'Date'
ndvi_mean_long$date <- as.Date(ndvi_mean_long$date, format = "%d/%m/%Y")

library(ggplot2)

# Convertir la colonne 'date' en classe 'Date'
ndvi_mean_long$date <- as.Date(ndvi_mean_long$date, format = "%d/%m/%Y")

library(ggplot2)

# Convertir la colonne 'date' en classe 'Date'
ndvi_mean_long$date <- as.Date(ndvi_mean_long$date, format = "%d/%m/%Y")

library(ggplot2)

# Convertir la colonne 'date' en classe 'Date'
ndvi_mean_long$date <- as.Date(ndvi_mean_long$date, format = "%d/%m/%Y")

library(ggplot2)

# Convertir la colonne 'date' en classe 'Date'
ndvi_mean_long$date <- as.Date(ndvi_mean_long$date, format = "%d/%m/%Y")

ggplot(ndvi_mean_long, aes(x = date, y = value)) +
  geom_line(color = "darkgreen", size = 1.2) +  # Épaissir les lignes
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  labs(x = "Date", y = "NDVI") +
  theme_minimal() +
  scale_x_date(date_labels = "%m", date_breaks = "3 months", date_minor_breaks = "1 year") +
  geom_vline(xintercept = as.numeric(seq(as.Date("2016-01-01"), as.Date("2024-12-31"), by = "years")), color = "red", linetype = "dotted", size=1) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(axis.text.x = element_text(size = 12),  # Ajuster la taille des étiquettes sur l'axe x
        axis.text.y = element_text(size = 12))  # Ajuster la taille des étiquettes sur l'axe y


########################## Cycle annuel moyen ##################################
# Convertir la colonne "time" en format de date
VH_RI <- VH_RI %>%
  mutate(time = as.Date(time, format = "%d/%m/%Y")) %>%
  mutate(month = lubridate::month(time),
         day = lubridate::day(time)) 
# Calculer la moyenne pour chaque mois sur toutes les années
cycle_annuel_moyen <- VH_RI %>%
  group_by(month) %>%
  summarize(across(2:6, mean, na.rm = TRUE))

library(dplyr)

traiter_dataframe <- function(df) {
  df <- df %>%
    mutate(time = as.Date(time, format = "%d/%m/%Y")) %>%
    mutate(month = lubridate::month(time),
           day = lubridate::day(time))
  
  return(df)
}

# Liste de dataframes
list_dataframes <- list(VH_RI, CH_RI, HVC_RI, JH_RI, PC_RI)
# Appliquer la fonction à chaque dataframe dans la liste
list_dataframes <- lapply(list_dataframes, traiter_dataframe)
