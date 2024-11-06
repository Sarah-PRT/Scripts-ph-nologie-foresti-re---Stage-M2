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

# répertoire de travail
setwd("E:/MASSANE/Sentinel-2/NDVI/Séries Temporelles")
# Lire chaque fichier
VH_RI <- read_delim("JH_RI_interp.csv", delim = ";", locale = locale(decimal_mark = ","))
#JH_RI <- read_delim("JH_RI_interp.csv", delim = ";", locale = locale(decimal_mark = ","))
#HVC_RI <- read_delim("HVC_RI_2016_23.csv", delim = ";", locale = locale(decimal_mark = ","))
#CH_RI <- read_delim("CH_RI_2016_23.csv", delim = ";", locale = locale(decimal_mark = ","))
#PC_RI <- read_delim("PC_RI_2016_23.csv", delim = ";", locale = locale(decimal_mark = ","))

# Convertir les colonnes 2 à 6 en numérique et suppression NA
VH_RI <- VH_RI %>% mutate_at(vars(2:10), as.numeric)
#JH_RI <- JH_RI %>% mutate_at(vars(2:6), as.numeric)
#HVC_RI <- HVC_RI %>% mutate_at(vars(2:6), as.numeric)
#CH_RI <- CH_RI %>% mutate_at(vars(2:6), as.numeric)
#PC_RI <- PC_RI %>% mutate_at(vars(2:6), as.numeric)


# Convertir la colonne time en type date dans le dataframe VH_RI
VH_RI$time <- as.Date(VH_RI$time, format = "%d/%m/%Y")

#Compter le nb de NA dans chaque colonne
sapply(VH_RI, function(x) sum(is.na(x)))



### Calcul des différence normalisées de la série de NDVI

# Définir la fenêtre de lissage gaussien
window_size <- 6 # Taille de la fenêtre
sigma <- 1  # Écart-type pour la fonction gaussienne

# Calculer les poids gaussiens
gaussian_weights <- dnorm(seq(-window_size, window_size), sd = sigma)

# Normaliser les poids pour qu'ils somment à 1
gaussian_weights <- gaussian_weights / sum(gaussian_weights)

# Appliquer le lissage gaussien à la colonne mean_ndvi
VH_RI$smoothed_mean_ndvi <- stats::filter(VH_RI$mean_ndvi, gaussian_weights, sides = 2)

# Calculer la moyenne et l'écart-type de mean_ndvi
mean_mean_ndvi <- mean(VH_RI$mean_ndvi, na.rm = TRUE)
sd_mean_ndvi <- sd(VH_RI$mean_ndvi, na.rm = TRUE)

# Calcul des z-scores
VH_RI <- VH_RI %>%
  mutate(z_score_mean_ndvi = (mean_ndvi - mean_mean_ndvi) / sd_mean_ndvi) %>%
  mutate(mean_ndvi = ifelse(z_score_mean_ndvi < -0.2, NA, mean_ndvi))


# Remplacer les valeurs manquantes de mean_ndvi par les valeurs correspondantes de smoothed_mean_ndvi
VH_RI$mean_ndvi <- ifelse(is.na(VH_RI$mean_ndvi), VH_RI$smoothed_mean_ndvi, VH_RI$mean_ndvi)


#Plot toute la série 
# Convertir la colonne 'time' en format de date
VH_RI$time <- as.Date(VH_RI$time, format = "%d/%m/%Y")
# Extraire les années distinctes
annees <- unique(lubridate::year(VH_RI$time))
# Appliquer le lissage gaussien à la colonne mean_ndvi
VH_RI$smoothed_mean_ndvi <- stats::filter(VH_RI$mean_ndvi, gaussian_weights, sides = 2)

# Remplacer les NA par la valeur du dessous dans la colonne mean_ndvi
VH_RI$mean_ndvi <- na_locf(VH_RI$mean_ndvi)


# Calculer la moyenne de mean_ndvi
mean_ndvi_mean <- mean(VH_RI$mean_ndvi, na.rm = TRUE)

# Tracer les deux séries temporelles avec un cadre de contour, des étiquettes de ticks plus grandes, un tick par année et une barre pour la moyenne
ggplot(VH_RI, aes(x = time)) +
  geom_line(aes(y = mean_ndvi), color = "black", linetype = "solid", size = 1) +
  # geom_hline(yintercept = mean_ndvi_mean, color = "grey", linetype = "solid", size = 0.3) +  # Ajouter une barre pour la moyenne
  geom_line(aes(y = smoothed_mean_ndvi), color = "green", linetype = "dashed", size = 1.5) +
  labs(x = "Date", y = "NDVI", title = "NDVI moyen avec lissage gaussien") +
  theme_light() +
  theme(panel.border = element_rect(color = "black", fill = NA),  # Ajouter un cadre de contour
        axis.text = element_text(size = 12),  # Taille des étiquettes des axes
        panel.grid.major = element_blank(),  # Supprimer le quadrillage de fond
        panel.grid.minor = element_blank()) +  # Supprimer le quadrillage de fond
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  # Afficher un tick par année et formater les étiquettes de date


# Convertir la colonne 'time' en format de date
VH_RI$time <- as.Date(VH_RI$time, format = "%d/%m/%Y")
# Extraire les années
annees <- unique(lubridate::year(VH_RI$time))
# plot avec spération des années
# Calculer la moyenne glissante sur la colonne mean_ndvi
VH_RI$smoothed_mean_ndvi <- ave(VH_RI$mean_ndvi, lubridate::year(VH_RI$time), FUN = function(x) rollmean(x, k = 10, fill = NA))
# Calculer la déviation standard
VH_RI <- VH_RI %>%
  mutate(upper_bound = mean_ndvi + std_ndvi,
         lower_bound = mean_ndvi - std_ndvi)

# Tracer les deux séries temporelles avec la déviation standard
ggplot(VH_RI, aes(x = time)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "lightcyan3", alpha = 0.6) + # Ajouter la déviation standard en transparence
  geom_line(aes(y = mean_ndvi), color = "lightcyan3", size = 1, alpha = 0.9) +
  geom_line(aes(y = smoothed_mean_ndvi), color = "#53868B", size = 1.5) + # Ajouter la moyenne glissante
  labs(x = "", y = "NDVI") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA), 
        axis.ticks.x = element_line(size = 0.5), 
        axis.ticks.y = element_line(size = 0.5), 
        axis.text = element_text(size = 12)) + # Augmenter la taille des étiquettes des axes
  facet_wrap(~ lubridate::year(time), scales = "free_x", nrow = 4) # facetgrid by year



################# On ajuste 2 modèles LOESS et SG pour la détection de phénophases
VH_RI <- select(VH_RI, time, mean_ndvi)

# Convertir la colonne 'time' en un objet de classe Date
VH_RI$time <- as.Date(VH_RI$time)
# Extraire l'année de la date
VH_RI$year <- lubridate::year(VH_RI$time)
# Ajouter une colonne DOYVH_RI pour le jour de l'année
VH_RI$DOY<- as.numeric(format(VH_RI$time, "%j"))
head(VH_RI)


# Ajuster le modèle Loess par année
models <- VH_RI %>%
  group_by(year) %>%
  do(model = loess(mean_ndvi ~ DOY, data = .))

# Visualiser les modèles
ggplot(VH_RI, aes(x = time, y = mean_ndvi)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  facet_wrap(~ year, scales = "free") +
  labs(title = "Modèles Loess par année")


# Charger la librairie dplyr si ce n'est pas déjà fait
library(dplyr)

# Ajuster le modèle LOESS par année et ajouter les prédictions à filtered_data
loess <- VH_RI %>%
  group_by(year) %>%
  mutate(loess = predict(loess(mean_ndvi ~ DOY))) %>%
  ungroup()  # Retirer la group_by pour revenir à un dataframe non groupé
head(loess)



library(signal)

# Définir les paramètres du filtre SG
window_size <- 9 # Taille de la fenêtre du filtre SG (nombre impair)
degree <- 2       # Degré du polynôme du filtre SG

# Ajuster le filtre SG par année et ajouter les données filtrées à un nouveau dataframe
filtered_data <- VH_RI %>%
  group_by(year) %>%
  mutate(SG = sgolayfilt(mean_ndvi, p = degree, n = window_size)) %>%
  ungroup() # Retirer la group_by pour revenir à un dataframe non groupé
head(filtered_data)


# Tracer un graphique par an
for (i in unique(VH_RI$year)) {
  # Sous-ensemble des données pour l'année spécifique
  data_year <- VH_RI[VH_RI$year == i, ]
  
  # Ajuster le filtre SG pour l'année spécifique
  filtered_data <- sgolayfilt(data_year$mean_ndvi, p = degree, n = window_size)
  
  # Tracer les données originales
  plot(data_year$time, data_year$mean_ndvi, type = "p", col = "blue", pch = 16, 
       xlab = "Date", ylab = "Mean NDVI", main = paste("Filtre Savitzky-Golay pour l'année", i))
  
  # Tracer les données filtrées
  lines(data_year$time, filtered_data, col = "red")
}

# Création d'un nouveau dataframe en combinant les colonnes SG et loess
combined_data <- cbind(filtered_data[c("DOY", "time", "mean_ndvi")], loess = loess$loess, SG = filtered_data$SG)
head(combined_data)

library(ggplot2)

# Ajuster le filtre SG par année et ajouter les données filtrées à un nouveau dataframe
filtered_data <- VH_RI %>%
  group_by(year) %>%
  mutate(SG = sgolayfilt(mean_ndvi, p = degree, n = window_size)) %>%
  ungroup() # Retirer la group_by pour revenir à un dataframe non groupé
head(filtered_data)


######## Cbind
combined_data <- cbind(filtered_data[, c("time", "mean_ndvi", "year", "DOY", "SG")], loess[, "loess"])
head(combined_data)



# Convertir la colonne "time" en année
combined_data$year <- lubridate::year(combined_data$time)

library(ggplot2)
library(cowplot)

# Créer un graphique distinct pour chaque année
combined_plot <- ggplot(combined_data, aes(x = DOY)) +
  geom_line(aes(y = mean_ndvi, color = "Mean NDVI")) +
  geom_line(aes(y = loess, color = "LOESS")) +
  geom_line(aes(y = SG, color = "SG")) +
  labs(x = "DOY", y = "Value", color = "Variable") +
  theme_minimal() +
  facet_wrap(~ year)

# Afficher le graphique
print(combined_plot)


################## DETECTION PHENO MIN_MAX #################################### 
# Calculer les événements saisonniers pour chaque année (points extrêmes)
season_extreme <- combined_data %>%
  group_by(year = lubridate::year(time)) %>%
  summarize(SOS_brut = ifelse(any(DOY >= 60 & DOY <= 100), min(DOY[DOY >= 60 & DOY <= 100 & mean_ndvi > 0.5 * diff(range(mean_ndvi))]), NA),
            SOS_loess = ifelse(any(DOY >= 60 & DOY <= 100), min(DOY[DOY >= 60 & DOY <= 100 & loess > 0.5 * diff(range(loess))]), NA),
            SOS_SG = ifelse(any(DOY >= 60 & DOY <= 100), min(DOY[DOY >= 60 & DOY <= 100 & SG > 0.5 * diff(range(SG))]), NA),
            POS_brut = ifelse(any(DOY >= 100 & DOY <= 1500), max(DOY[DOY >= 100 & DOY <= 150 & mean_ndvi > 0.5 * diff(range(mean_ndvi))]), NA),
            POS_loess = ifelse(any(DOY >= 100 & DOY <= 150), max(DOY[DOY >= 100 & DOY <= 150 & loess > 0.5 * diff(range(loess))]), NA),
            POS_SG = ifelse(any(DOY >= 100 & DOY <= 150), max(DOY[DOY >= 100 & DOY <= 150 & SG > 0.5 * diff(range(SG))]), NA),
            EOS_brut = ifelse(any(DOY >= 280 & DOY <= 330), min(DOY[DOY >= 280 & DOY <= 300 & mean_ndvi > 0.5 * diff(range(mean_ndvi))]), NA),
            EOS_loess = ifelse(any(DOY >= 280 & DOY <= 330), min(DOY[DOY >= 280 & DOY <= 300 & loess > 0.5 * diff(range(loess))]), NA),
            EOS_SG = ifelse(any(DOY >= 280 & DOY <= 330), min(DOY[DOY >= 280 & DOY <= 300 & SG > 0.5 * diff(range(SG))]), NA))



# Calculer l'écart type pour chaque année
std_dev <- combined_data %>%
  group_by(year) %>%
  summarize(std_ndvi = sd(mean_ndvi))


# Plot avec séparation des années et segments verticaux pour SOS, POS et EOS
ggplot(combined_data, aes(x = DOY)) +
  geom_line(aes(y = mean_ndvi, color = "Mean NDVI"), size = 2) +  # Courbe pour Mean NDVI
  geom_line(aes(y = SG, color = "SG"), size = 2, linetype = "dotted") +  # Courbe pour SG
  geom_line(aes(y = loess, color = "Loess"), size = 2, linetype = "dashed") +  # Courbe pour Loess
  geom_vline(data = season_extreme, aes(xintercept = SOS_brut, color = "SOS (brut)"), linetype = "dashed", size = 1) +  # Épaissir les traits verticaux pour les événements extrêmes
  geom_vline(data = season_extreme, aes(xintercept = POS_brut, color = "POS (brut)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme, aes(xintercept = EOS_brut, color = "EOS (brut)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme, aes(xintercept = SOS_loess, color = "SOS (loess)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme, aes(xintercept = POS_loess, color = "POS (loess)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme, aes(xintercept = EOS_loess, color = "EOS (loess)"), linetype = "dashed", size = 1) +
  labs(x = "DOY", y = "NDVI", color = "") +  # Titre de la légende
  scale_color_manual(values = c("purple", "lightgreen", "lightblue", "darkred", "pink", "green", "blue", "red", "yellow", "skyblue","grey")) +  # Couleurs pour les événements et les courbes
  scale_x_continuous(breaks = seq(0, 365, by = 50)) +  # Ajouter des graduations tous les 50 jours, commençant à 0
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +  # Ajouter des graduations tous les 0.1 sur l'axe y
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 1),  # Épaissir et ajuster la taille des traits de graduation sur l'axe x
        axis.ticks.y = element_line(color = "black", size = 1),  # Épaissir et ajuster la taille des traits de graduation sur l'axe y
        axis.text.x = element_text(size = 12),  # Ajuster la taille du texte sur l'axe x
        axis.text.y = element_text(size = 12)) +  # Ajuster la taille du texte sur l'axe y
  facet_wrap(~ year, scales = "free_x", nrow = 4, ncol = 2) +
  theme(legend.position = "bottom", panel.grid = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA)) +  # Retirer le quadrillage et ajouter un cadre autour de chaque facet
  ylim(0, 1)  # Fixer l'axe y maximum à 1


######################## DETECTION PHENO 50% ###################################
# Calcul de l'amplitude de la variation annuelle pour chaque colonne
annual_variation <- combined_data %>%
  group_by(year) %>%
  summarize(
    range_mean_ndvi = diff(range(mean_ndvi)),
    range_loess = diff(range(loess)),
    range_SG = diff(range(SG))
  )

# Affichons les premières lignes de annual_variation
head(annual_variation)

# Jointure avec combined_data
combined_data <- left_join(combined_data, annual_variation, by = "year") 
combined_data <- combined_data %>%
  select(-matches("\\.x|\\.y"))


# Calcul des événements saisonniers pour chaque année (points extrêmes)
season_extreme_50 <- combined_data %>%
  group_by(year = lubridate::year(time)) %>%
  summarize(
    SOS_brut = ifelse(any(DOY >= 60 & DOY <= 100), (DOY[DOY >= 50 & DOY <= 100 & mean_ndvi > 0.5 * range_mean_ndvi]), NA),
    SOS_loess = ifelse(any(DOY >= 60 & DOY <= 100), (DOY[DOY >= 50 & DOY <= 100 & loess > 0.5 * range_loess]), NA),
    SOS_SG = ifelse(any(DOY >= 60 & DOY <= 100), (DOY[DOY >= 50 & DOY <= 100 & SG > 0.5 * range_SG]), NA),
    POS_brut = ifelse(any(DOY >= 100 & DOY <= 150), max(DOY[DOY >= 100 & DOY <= 150 & mean_ndvi > 0.8 * range_mean_ndvi]), NA),
    POS_loess = ifelse(any(DOY >= 100 & DOY <= 150), max(DOY[DOY >= 100 & DOY <= 150 & loess > 0.8 * range_loess]), NA),
    POS_SG = ifelse(any(DOY >= 100 & DOY <= 150), max(DOY[DOY >= 100 & DOY <= 150 & SG > 0.8 * range_SG]), NA),
    EOS_brut = ifelse(any(DOY >= 300 & DOY <= 330), (DOY[DOY >= 300 & DOY <= 330 & mean_ndvi < 0.7 * range_mean_ndvi]), NA),
    EOS_loess = ifelse(any(DOY >= 300 & DOY <= 330), (DOY[DOY >= 300 & DOY <= 330 & loess < 0.7 * range_loess]), NA),
    EOS_SG = ifelse(any(DOY >= 300 & DOY <= 330), (DOY[DOY >= 300 & DOY <= 330 & SG < 0.7 * range_SG]), NA)
  )


# Plot avec séparation des années et segments verticaux pour SOS, POS et EOS
ggplot(combined_data, aes(x = DOY)) +
  geom_line(aes(y = mean_ndvi, color = "Mean NDVI"), size = 1) +  # Courbe pour Mean NDVI
  geom_line(aes(y = SG, color = "SG"), size = 2, linetype = "dotted") +  # Courbe pour SG
  geom_line(aes(y = loess, color = "Loess"), size = 2, linetype = "dashed") +  # Courbe pour Loess
  geom_vline(data = season_extreme_50, aes(xintercept = SOS_brut, color = "SOS (brut)"), linetype = "dashed", size = 1) +  # Épaissir les traits verticaux pour les événements extrêmes
  geom_vline(data = season_extreme_50, aes(xintercept = POS_brut, color = "POS (brut)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme_50, aes(xintercept = EOS_brut, color = "EOS (brut)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme_50, aes(xintercept = SOS_loess, color = "SOS (loess)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme_50, aes(xintercept = POS_loess, color = "POS (loess)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme_50, aes(xintercept = EOS_loess, color = "EOS (loess)"), linetype = "dashed", size = 1) +
  labs(x = "DOY", y = "NDVI", color = "") +  # Titre de la légende
  scale_color_manual(values = c("purple", "lightgreen", "black", "darkred", "pink", "green", "blue", "red", "yellow", "skyblue","grey")) +  # Couleurs pour les événements et les courbes
  scale_x_continuous(breaks = seq(0, 365, by = 50)) +  # Ajouter des graduations tous les 50 jours, commençant à 0
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +  # Ajouter des graduations tous les 0.1 sur l'axe y
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 1),  # Épaissir et ajuster la taille des traits de graduation sur l'axe x
        axis.ticks.y = element_line(color = "black", size = 1),  # Épaissir et ajuster la taille des traits de graduation sur l'axe y
        axis.text.x = element_text(size = 12),  # Ajuster la taille du texte sur l'axe x
        axis.text.y = element_text(size = 12)) +  # Ajuster la taille du texte sur l'axe y
  facet_wrap(~ year, scales = "free_x", nrow = 4, ncol = 2) +
  theme(legend.position = "bottom", panel.grid = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA)) +  # Retirer le quadrillage et ajouter un cadre autour de chaque facet
  ylim(0, 1)  # Fixer l'axe y maximum à 1





