library(zoo)
library(dplyr)
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
library(readxl)
library(stats)
library(Rbeast)

pass1 <- "E:/MASSANE/Sentinel-2/BEAST/BEAST_vh_ri.xlsx"
pass2 <- "F:/MASSANE/Sentinel-2/BEAST/BEAST_vh.xlsx"
pass3 <- "F:/MASSANE/Sentinel-2/BEAST/BEAST_jh_ri.xlsx"
pass4 <- "F:/MASSANE/Sentinel-2/BEAST/BEAST_jh.xlsx"
pass5 <- "F:/MASSANE/Sentinel-2/BEAST/BEAST_jv.xlsx"
pass6 <- "F:/MASSANE/Sentinel-2/BEAST/BEAST_hvc_ri.xlsx"
pass7 <- "F:/MASSANE/Sentinel-2/BEAST/BEAST_pc_ri.xlsx"

# Lire les fichiers Excel dans des dataframes
vh_ri <- read_excel(pass1)

# Appliquer les transformations sur chaque dataframe
vh_ri$time <- as.Date(vh_ri$time, format = "%d/%m/%Y")
# Extraire l'année de la colonne 'time'
vh_ri$year <- format(vh_ri$time, "%Y")
# Définir les couleurs pour chaque groupe
colors <- c("VH_RI" = "#668B8B", "VH" = "#2F766B", 
            "JH_RI" = "#CDC9A5", "JH" = "#84AC9D", "JV" = "#903B42")



# Remplacer tous les chiffres non NA dans breakpoint par "A" ou "B" selon les dates spécifiques
vh_ri <- vh_ri %>%
  mutate(
    breakpoint = ifelse(
      !is.na(breakpoint) & 
        ((month(time) == 4 & day(time) >= 1) | 
           (month(time) == 5) | 
           (month(time) == 6 & day(time) <= 15) |
           (month(time) == 9 & day(time) >= 15) |
           (month(time) == 10) | 
           (month(time) == 11 & day(time) <= 25)),
      "A", 
      ifelse(!is.na(breakpoint), "B", NA)
    )
  )

# Identifier la première date "A" pour chaque année
first_A_dates <- vh_ri %>%
  filter(breakpoint == "A") %>%
  group_by(year) %>%
  summarise(first_A_date = min(time))

# Identifier les années où le premier "A" est après le 1er mai
years_to_check <- first_A_dates %>%
  filter(first_A_date > as.Date(paste0(year, "-04-20"))) %>%
  pull(year)

# Identifier la première rupture "C" entre mars et avril pour ces années
first_C_breaks <- vh_ri %>%
  filter(year %in% years_to_check & !is.na(breakpoint) & breakpoint == "B") %>%
  group_by(year) %>%
  filter((month(time) == 3) | (month(time) == 4 & day(time) <= 30)) %>%
  slice_min(time, with_ties = FALSE) %>%
  mutate(breakpoint = "C")

# Mettre à jour la colonne breakpoint avec "C" pour les premières ruptures identifiées
vh_ri <- vh_ri %>%
  left_join(first_C_breaks %>% select(year, time, breakpoint), by = c("year", "time")) %>%
  mutate(breakpoint = coalesce(breakpoint.y, breakpoint.x)) %>%
  select(-breakpoint.x, -breakpoint.y)

# Exemple avec vos données
# Supposons que vh_ri est déjà chargé avec les données
# Je vais utiliser les données simulées précédentes pour l'exemple

# Convertir 'time' en format Date si ce n'est pas déjà fait
vh_ri$time <- as.Date(vh_ri$time)

# Modifier le thème pour spécifier explicitement un fond blanc
theme_custom <- theme_minimal() +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white"),  # Fond des panneaux en blanc
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.line = element_line(color = "black"),
    strip.text = element_text(size = 14),
    strip.background = element_rect(fill = "white")
  )

# Créer le plot avec ggplot pour vh_ri
plot_vh_ri <- ggplot(vh_ri, aes(x = time)) +
  geom_line(aes(y = mean_ndvi, color = "Mean NDVI"), linetype = "dotted", size = 2.2) +  # Ligne pour mean_ndvi
  geom_line(aes(y = mod, color = "MOD"), size = 2.1) +  # Ligne en pointillés pour mod
  labs(x = "Mois", y = "NDVI", color = "Variable") +  # Labels des axes et de la légende
  scale_color_manual(values = c("Mean NDVI" = "black", "MOD" = "#D87118")) +  # Couleurs spécifiques pour les lignes
  theme_bw() +  # Thème minimal avec fond blanc
  theme(
    legend.position = "top",  # Position de la légende
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Cadre avec des ticks
    panel.grid.major = element_blank(),  # Supprimer la grille principale
    panel.grid.minor = element_blank(),  # Supprimer la grille secondaire
    axis.text = element_text(size = 25),  # Taille des étiquettes des axes
    axis.title = element_text(size = 20),  # Taille des titres des axes
    axis.line = element_line(color = "black"),  # Lignes des axes noires
    strip.text = element_text(size = 25),  # Taille du texte des titres des facettes
    strip.background = element_blank(),  # Supprimer le fond des titres des facettes
    plot.background = element_rect(fill = "white", color = NA)  # Fond blanc pour le graphique
  ) +
  facet_wrap(~ year, scales = "free_x", ncol = 1, 
             shrink = TRUE,  # Réduire l'espace autour des facettes
             strip.position = "top") +  # Position des titres des facettes en haut
  
  scale_x_date(date_breaks = "1 month", date_labels = "%m")  # Afficher les étiquettes des mois en chiffres

# Afficher le plot pour vh_ri
print(plot_vh_ri)

# Définir le chemin où vous souhaitez sauvegarder l'image
chemin <- "F:/MASSANE/Sorties Stats/PROPRE/Ruptures_BEAST/"
nom_fichier <- "ROI_vh_ri.png"  # Nom de fichier souhaité

# Exporter le graphique en spécifiant les dimensions
ggsave(file = paste0(chemin, nom_fichier), plot = plot_vh_ri, width = 10, height = 18, units = "in")

################################################################################
# Création du graphique avec ggplot pour vh_ri
plot_vh_ri <- ggplot(vh_ri, aes(x = time)) +
  geom_line(aes(y = mean_ndvi, color = "Mean NDVI"), linetype = "dotted", size = 2) +
  geom_line(aes(y = mod, color = "MOD"), size = 1.5) +
  labs(x = "Date", y = "Valeur", color = "Variable") +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 18),
    axis.line = element_line(color = "black"),
    strip.text = element_text(size = 20),
    strip.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_color_manual(values = c("Mean NDVI" = "black", "MOD" = "#D87118")) + # Couleurs des courbes
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  facet_wrap(~ year, scales = "free_x", ncol = 1)

# Ajout des barres verticales pour les breakpoints "A"
plot_vh_ri <- plot_vh_ri +
  geom_vline(data = vh_ri[vh_ri$breakpoint == "A", ], aes(xintercept = as.numeric(time)), color = "red", linetype = "dashed", size = 1.2)

# Ajout des barres verticales pour les breakpoints "B"
plot_vh_ri <- plot_vh_ri +
  geom_vline(data = vh_ri[vh_ri$breakpoint == "B", ], aes(xintercept = as.numeric(time)), color = "blue", linetype = "dashed", size = 1.2)

# Affichage du graphique
print(plot_vh_ri)


# Définir le chemin où vous souhaitez sauvegarder l'image
chemin <- "F:/MASSANE/Sorties Stats/PROPRE/Ruptures_BEAST/"
nom_fichier <- "pc_ruptures.png"  # Nom de fichier souhaité

# Exporter le graphique en spécifiant les dimensions
ggsave(file = paste0(chemin, nom_fichier), plot = plot_vh_ri, width = 10, height = 20, units = "in")



# Création du graphique avec ggplot pour vh_ri
plot_vh_ri <- ggplot(vh_ri, aes(x = time)) +
  geom_line(aes(y = mean_ndvi, color = "Mean NDVI"), linetype = "dotted", size = 3) +
  geom_line(aes(y = mod, color = "MOD"), size = 1.5) +
  geom_line(aes(y = poba, color = "Proba"), size = 1, linetype = "solid") + # Ajouter la courbe de proba
  labs(x = "Date", y = "NDVI", color = "Variable") +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 22),
    axis.title = element_text(size = 23),
    axis.line = element_line(color = "black"),
    strip.text = element_text(size = 22),
    strip.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_color_manual(values = c("Mean NDVI" = "#D87118", "MOD" = "#D87118", "Proba" = "#CD2990")) + # Couleurs des courbes
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  facet_wrap(~ year, scales = "free_x", ncol = 1) +
  scale_y_continuous(
    name = "NDVI",
    sec.axis = sec_axis(~ ., name = "Probabilité de rupture")
  )

# Ajout des barres verticales pour les breakpoints "A"
plot_vh_ri <- plot_vh_ri +
  geom_vline(data = vh_ri[vh_ri$breakpoint == "A", ], aes(xintercept = as.numeric(time)), color = "#CD2990", linetype = "dashed", size = 1.2)

# Ajout des barres verticales pour les breakpoints "B"
plot_vh_ri <- plot_vh_ri +
  geom_vline(data = vh_ri[vh_ri$breakpoint == "B", ], aes(xintercept = as.numeric(time)), color = "#7EC0EE", linetype = "dashed", size = 1.2)

# Ajout des barres verticales pour les breakpoints "B"
plot_vh_ri <- plot_vh_ri +
  geom_vline(data = vh_ri[vh_ri$breakpoint == "C", ], aes(xintercept = as.numeric(time)), color = "#0000FF", linetype = "dashed", size = 1.2)

# Affichage du graphique
print(plot_vh_ri)

# Définir le chemin où vous souhaitez sauvegarder l'image
chemin <- "F:/MASSANE/Sorties Stats/PROPRE/Ruptures_BEAST/"
nom_fichier <- "pc_probas.png"  # Nom de fichier souhaité
# Exporter le graphique en spécifiant les dimensions
ggsave(file = paste0(chemin, nom_fichier), plot = plot_vh_ri, width = 10, height = 22, units = "in")

################################################################################
################################################################################
################### Calculer la pente ##########################################

# Ajouter une colonne où les "C" deviennent des "A"
vh_ri <- vh_ri %>%
  mutate(breakpoint_adjusted = ifelse(breakpoint == "C", "A", breakpoint))

# Identifier et extraire les segments entre les deux premiers "A" ou entre "C" et "A"
segments <- vh_ri %>%
  group_by(year) %>%
  mutate(
    segment = case_when(
      sum(breakpoint_adjusted == "A") >= 2 ~ {
        first_A <- time[which(breakpoint_adjusted == "A")[1]]
        second_A <- time[which(breakpoint_adjusted == "A")[2]]
        case_when(
          time >= first_A & time <= second_A ~ "Segment",
          TRUE ~ NA_character_
        )
      },
      sum(breakpoint_adjusted == "A") == 1 & sum(breakpoint == "C") == 1 ~ {
        first_C <- time[which(breakpoint == "C")[1]]
        first_A <- time[which(breakpoint_adjusted == "A")[1]]
        case_when(
          time >= first_C & time <= first_A ~ "Segment",
          TRUE ~ NA_character_
        )
      },
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(segment)) %>%
  ungroup()
# Affichage du dataframe des segments
print(segments)



###########Feuillaison##########""

