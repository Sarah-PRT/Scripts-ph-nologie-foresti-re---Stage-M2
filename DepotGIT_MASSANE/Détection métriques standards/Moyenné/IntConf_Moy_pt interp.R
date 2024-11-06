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
library(openxlsx)

# Définir le répertoire de travail (seulement le chemin du répertoire)
setwd("E:/MASSANE/Sentinel-2/Interp_Gaussien")
# Spécifier le chemin complet du fichier Excel
# Spécifier le chemin complet vers votre fichier Excel
file_path <- "E:/MASSANE/Sentinel-2/Interp_Gaussien/ROI_vh_ri.xlsx"
VH_RI <- read.xlsx(file_path, detectDates = TRUE)


library(dplyr)
library(slipper)
library(boot)
# Convertir en format de date si nécessaire
VH_RI$time <- as.Date(VH_RI$time)


################## Intervalle de confiance ########################################
# Calcul de l'intervalle de confiance à 95%
n <- nrow(VH_RI)  # Nombre d'observations
alpha <- 0.05  # Niveau de confiance
z_value <- qnorm(1 - alpha/2)  # Quantile de la distribution normale standard

# Calcul de l'intervalle de confiance
VH_RI$lower_bound <- VH_RI$mean_ndvi - z_value * VH_RI$std_ndvi / sqrt(n)
VH_RI$upper_bound <- VH_RI$mean_ndvi + z_value * VH_RI$std_ndvi / sqrt(n)

# Affichage des résultats
print(VH_RI[, c("time", "mean_ndvi", "lower_bound", "upper_bound")])


# Calculer la déviation standard sur la colonne mean_ndvi pour chaque année
# Calculer la déviation standard sur la colonne mean_ndvi pour chaque année
VH_RI <- VH_RI %>%
  group_by(year = lubridate::year(time)) %>%
  mutate(std_ndvi = sd(mean_ndvi, na.rm = TRUE)) %>%
  ungroup()
# Filtrer les données pour inclure uniquement les mois d'avril à novembre
T_filtered <- VH_RI %>%
  filter(lubridate::month(time) %in% 4:10)
# Calculer la moyenne globale de mean_ndvi sur les mois filtrés
normale <- mean(T_filtered$mean_ndvi, na.rm = TRUE)
print(normale)
# Calculer la moyenne annuelle de mean_ndvi pour les mois de mai à octobre
mean_annual <- T_filtered %>%
  group_by(year) %>%
  summarize(mean_annual = mean(mean_ndvi, na.rm = TRUE))

###
###PLOT
###
# Supposons que VH_RI contient vos données avec les colonnes nécessaires comme mean_ndvi, lower_bound, upper_bound et time
# Plotting avec ggplot2
ggplot(VH_RI, aes(x = time, y = mean_ndvi)) +
  geom_line(color = "grey") +  # Trace la ligne de la moyenne
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "black", alpha = 0.2) +  # Trace l'intervalle de confiance
  labs(x = "Time", y = "Mean NDVI", title = "Mean NDVI with 95% Confidence Interval") +  # Ajoute les étiquettes des axes et le titre
  theme_minimal()  # Choix du thème du graphique


# Calculer la déviation standard sur la colonne mean_ndvi pour chaque année
# Calculer la déviation standard sur la colonne mean_ndvi pour chaque année
VH_RI <- VH_RI %>%
  group_by(year = lubridate::year(time)) %>%
  mutate(std_ndvi = sd(mean_ndvi, na.rm = TRUE)) %>%
  ungroup()
# Filtrer les données pour inclure uniquement les mois d'avril à novembre
T_filtered <- VH_RI %>%
  filter(lubridate::month(time) %in% 4:10)
# Calculer la moyenne globale de mean_ndvi sur les mois filtrés
normale <- mean(T_filtered$mean_ndvi, na.rm = TRUE)
print(normale)
# Calculer la moyenne annuelle de mean_ndvi pour les mois de mai à octobre
mean_annual <- T_filtered %>%
  group_by(year) %>%
  summarize(mean_annual = mean(mean_ndvi, na.rm = TRUE))

################## Séparation par année ########################################

# Filtrer les données pour inclure seulement l'année 2017
VH_RI_2017 <- VH_RI[format(VH_RI$time, "%Y") == "2017", ]
# Filtrer les données pour inclure seulement l'année 2017
VH_RI_2018 <- VH_RI[format(VH_RI$time, "%Y") == "2018", ]
# Filtrer les données pour inclure seulement l'année 2017
VH_RI_2019 <- VH_RI[format(VH_RI$time, "%Y") == "2019", ]
# Filtrer les données pour inclure seulement l'année 2018
VH_RI_2020 <- VH_RI[format(VH_RI$time, "%Y") == "2020", ]
# Filtrer les données pour inclure seulement l'année 2017
VH_RI_2021 <- VH_RI[format(VH_RI$time, "%Y") == "2021", ]
# Filtrer les données pour inclure seulement l'année 2017
VH_RI_2022 <- VH_RI[format(VH_RI$time, "%Y") == "2022", ]
# Filtrer les données pour inclure seulement l'année 2017
VH_RI_2023 <- VH_RI[format(VH_RI$time, "%Y") == "2023", ]

################################################################################
######################### Plot Annuels #########################################

# Filtrer les données pour l'année 2017
VH_RI_2017 <- VH_RI %>% filter(year(time) == 2017)

# Créer le graphique pour l'année 2017 avec une ligne noire pour mean_ndvi et points colorés en fonction de has_NA
plot_2017 <- ggplot(VH_RI_2017, aes(x = time, y = mean_ndvi)) +
  geom_line(color = "#2F766B", size = 1, linetype = "solid") +  # Ajoute une ligne noire pour mean_ndvi
  geom_point(aes(color = has_NA), size = 5, shape = 20) +  # Ajoute des points colorés par has_NA
  scale_color_manual(values = c("FALSE" = "#2F766B", "TRUE" = "red"), name = "has_NA") +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#2F766B", alpha = 0.3, color = NA) +  # Ajoute la bande de couleur
  geom_segment(aes(x = as.Date("2017-04-01"), xend = as.Date("2017-10-31"), y = normale, yend = normale), color = "#4A708B", linetype = "dashed", size = 0.9) +  # Ajoute la barre horizontale pour normale
  geom_segment(data = mean_annual %>% filter(year == 2017), aes(x = as.Date("2017-04-01"), xend = as.Date("2017-10-31"), y = mean_annual, yend = mean_annual), color = "#4A708B", size = 0.9) +  # Ajoute la barre horizontale pour mean_annual
  geom_text(data = mean_annual %>% filter(year == 2017), aes(x = as.Date("2017-10-15"), y = mean_annual, label = paste("Moy annuelle:", round(mean_annual, 2))), vjust = -1, size = 5, color = "#4A708B") +  # Ajoute l'annotation pour mean_annual
  geom_text(aes(x = as.Date("2017-10-15"), y = normale, label = paste("Normale:", round(normale, 2))), vjust = -1, size = 5, color = "#4A708B") +  # Ajoute l'annotation pour normale
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +  # Définit des ticks mensuels et les mois en chiffres
  theme_minimal() +
  labs(x = "Date", y = "NDVI moy", title = "2017") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Augmente la taille des textes de l'axe x
    axis.text.y = element_text(size = 12),  # Augmente la taille des textes de l'axe y
    axis.title.x = element_text(size = 14),  # Augmente la taille de l'étiquette de l'axe x
    axis.title.y = element_text(size = 14),  # Augmente la taille de l'étiquette de l'axe y
    axis.ticks = element_line(size = 0.5),  # Ajoute des ticks aux axes
    panel.grid.major = element_blank(),  # Retire la grille principale
    panel.grid.minor = element_blank(),  # Retire la grille secondaire
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Ajoute un cadre
  )

print(plot_2017)

