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

#pass1 <- "E:/MASSANE/Sentinel-2/BEAST/BEAST_vh_ri.xlsx"
pass1 <- "E:/MASSANE/Sentinel-2/BEAST/BEAST_df_560_CORR.xlsx"
vh_ri <- read_excel(pass1)
pass2 <- "E:/MASSANE/Sentinel-2/Interp_Gaussien/ROI_vh_ri.xlsx"
Na <- read_excel(pass2)
# Sélectionner uniquement les colonnes 1 et 8
Na <- Na[, c(1, 8)]
# Joindre les DataFrames sur la colonne 'time'
vh_ri <- merge(vh_ri, Na, by = "time")

# Appliquer les transformations sur chaque dataframe
vh_ri$time <- as.Date(vh_ri$time, format = "%d/%m/%Y")
# Extraire l'année de la colonne 'time'
vh_ri$year <- format(vh_ri$time, "%Y")
# Définir les couleurs pour chaque groupe
colors <- c("VH_RI" = "#668B8B", "VH" = "#2F766B", 
            "JH_RI" = "#CDC9A5", "JH" = "#84AC9D", "JV" = "#903B42")

##################### Récupérer les probabilités ##############################


# Convertir 'time' en format Date si ce n'est pas déjà fait
vh_ri$time <- as.Date(vh_ri$time)

# Création du graphique avec ggplot pour vh_ri
plot_vh_ri <- ggplot(vh_ri, aes(x = time)) +
  #geom_line(aes(y = mean_ndvi, color = "Mean NDVI"), linetype = "solid", size = 1.3) +
  geom_line(aes(y = mod, color = "MOD"), size = 1.5) +
  geom_line(aes(y = poba, color = "Proba"), size = 1.5, linetype = "solid") +
  geom_point(data = subset(vh_ri, has_NA == TRUE), aes(y = mean_ndvi), color = "red", size = 3) +
  geom_point(data = subset(vh_ri, has_NA == FALSE), aes(y = mean_ndvi), color = "#2F766B", size = 3) +
  labs(x = "Mois", y = "NDVI", color = "") +
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
  scale_color_manual(values = c("Mean NDVI" = "#2F766B", "MOD" = "#2F766B", "Proba" = "#CD2990")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  facet_wrap(~ year, scales = "free_x", ncol = 1) +
  scale_y_continuous(
    name = "NDVI",
    sec.axis = sec_axis(~ ., name = "Probabilité de rupture")
  )
# Affichage du graphique
print(plot_vh_ri)

# Définir le chemin où vous souhaitez sauvegarder l'image
chemin <- "E:/MASSANE/Sorties Stats/PROPRE/Ruptures_BEAST/"
nom_fichier <- "560_probas.png"  # Nom de fichier souhaité
# Exporter le graphique en spécifiant les dimensions
ggsave(file = paste0(chemin, nom_fichier), plot = plot_vh_ri, width = 10, height = 22, units = "in")

# Convertir 'time' en format Date si ce n'est pas déjà fait
vh_ri$time <- as.Date(vh_ri$time)

# Filtrer les données pour l'année 2019
vh_ri_2019 <- subset(vh_ri, format(time, "%Y") == "2019")

# Création du graphique avec un cadre autour et des ticks
plot_vh_ri_2019 <- ggplot(vh_ri_2019, aes(x = time)) +
  #geom_line(aes(y = mean_ndvi, color = "Mean NDVI"), linetype = "solid", size = 1.3) +
  geom_line(aes(y = mod, color = "MOD"), size = 1.8) +
  geom_line(aes(y = poba, color = "Proba"), size = 1.5, linetype = "solid") +
  geom_point(data = subset(vh_ri_2019, has_NA == TRUE), aes(y = mean_ndvi), color = "red", size = 3) +
  geom_point(data = subset(vh_ri_2019, has_NA == FALSE), aes(y = mean_ndvi), color = "#2F766B", size = 3) +
  labs(x = "Mois", y = "NDVI", color = "") +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 18),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black", size = 0.5),  # Ajout des ticks
    axis.ticks.length = unit(0.15, "cm"),  # Longueur des ticks
    strip.text = element_text(size = 20),
    strip.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # Ajout du cadre autour du graphique
  ) +
  scale_color_manual(values = c("Mean NDVI" = "#2F766B", "MOD" = "#2F766B", "Proba" = "#CD2990")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  scale_y_continuous(
    name = "NDVI",
    sec.axis = sec_axis(~ ., name = "Probabilité de rupture")
  )
print(plot_vh_ri_2019)

####################Détection des phénophases###########################
########################### Sur la proba max ##################################

# Convertir 'time' en format Date si ce n'est pas déjà fait
vh_ri$time <- as.Date(vh_ri$time)

# Filtrer les données entre le 1er mars et le 15 avril
filtered_SOS <- vh_ri %>%
  dplyr::filter(format(time, "%m-%d") >= "03-20" & format(time, "%m-%d") <= "04-15")

# Trouver la probabilité maximale ('poba') pour chaque année
SOS <- filtered_SOS %>%
  group_by(year) %>%
  dplyr::filter(poba == max(poba, na.rm = TRUE)) %>%
  summarise(max_poba = max(poba),
            date_max_poba = time[poba == max(poba)])  # Obtenir la date correspondant à la probabilité maximale
SOS <- SOS %>% 
  mutate(indice = "SOS")
SOS

# Filtrer les données entre le 1er mars et le 15 avril
filtered_POS <- vh_ri %>%
  dplyr::filter(format(time, "%m-%d") >= "05-01" & format(time, "%m-%d") <= "06-20")

# Trouver la probabilité maximale ('poba') pour chaque année
POS <- filtered_POS %>%
  group_by(year) %>%
  dplyr::filter(poba == max(poba, na.rm = TRUE)) %>%
  summarise(max_poba = max(poba),
            date_max_poba = time[poba == max(poba)])  # Obtenir la date correspondant à la probabilité maximale
POS <- POS %>% 
  mutate(indice = "POS")
POS

# Filtrer les données entre le 1er mars et le 15 avril
filtered_FRU <- vh_ri %>%
  dplyr::filter(format(time, "%m-%d") >= "09-15" & format(time, "%m-%d") <= "10-15")

# Trouver la probabilité maximale ('poba') pour chaque année
FRU <- filtered_FRU %>%
  group_by(year) %>%
  dplyr::filter(poba == max(poba, na.rm = TRUE)) %>%
  summarise(max_poba = max(poba),
            date_max_poba = time[poba == max(poba)])  # Obtenir la date correspondant à la probabilité maximale
FRU <- FRU %>% 
  mutate(indice = "FRU")
FRU

# Filtrer les données entre le 1er mars et le 15 avril
filtered_SEN <- vh_ri %>%
  dplyr::filter(format(time, "%m-%d") >= "10-16" & format(time, "%m-%d") <= "11-20")

# Trouver la probabilité maximale ('poba') pour chaque année
EOS <- filtered_SEN %>%
  group_by(year) %>%
  dplyr::filter(poba == max(poba, na.rm = TRUE)) %>%
  summarise(max_poba = max(poba),
            date_max_poba = time[poba == max(poba)])  # Obtenir la date correspondant à la probabilité maximale

# Ajouter une colonne 'indice' avec la valeur "EOS"
EOS <- EOS %>% 
  mutate(indice = "EOS")
EOS

# Ajout de la colonne 'indice' et fusion des tibbles
pheno <- bind_rows(
  mutate(SOS, indice = "SOS"),
  mutate(POS, indice = "POS"),
  mutate(FRU, indice = "FRU"),
  mutate(EOS, indice = "EOS")
)
print(pheno)

# Renommer la colonne 'date_max_poba' en 'date_max_proba'
pheno <- pheno %>% 
  rename(time = date_max_poba)
# Ajouter un indice de rupture à pheno "1"
pheno <- pheno %>% 
  mutate(rupture = "TRUE")

# Jointure de vh_ri avec pheno par la colonne 'year'
vh_ri <- left_join(vh_ri, pheno, by = "time")


# Séparer le DataFrame par la colonne year.x
dataframes_by_year <- split(vh_ri, vh_ri$year.x)

# Par exemple, accéder au DataFrame pour l'année 2017
df_2017 <- dataframes_by_year[["2017"]]
df_2018 <- dataframes_by_year[["2018"]]
df_2019 <- dataframes_by_year[["2019"]]
df_2020 <- dataframes_by_year[["2020"]]
df_2021 <- dataframes_by_year[["2021"]]
df_2022 <- dataframes_by_year[["2022"]]
df_2023 <- dataframes_by_year[["2023"]]


# Définir les dates pour SOS, POS et EOS dans la colonne 'indice'
sos_dates <- df_2017$time[df_2017$indice == "SOS"]
pos_dates <- df_2017$time[df_2017$indice == "POS"]
eos_dates <- df_2017$time[df_2017$indice == "EOS"]

# Définir les dates uniques pour SOS, POS et EOS dans la colonne 'indice'
sos_dates <- unique(df_2017$time[df_2017$indice == "SOS"])
pos_dates <- unique(df_2017$time[df_2017$indice == "POS"])
eos_dates <- unique(df_2017$time[df_2017$indice == "EOS"])

# Tracer trend en fonction de time pour df_2017 avec les barres verticales
ggplot(df_2017, aes(x = as.Date(time), y = trend)) +
  geom_line() +
  labs(title = "Trend en fonction de Time pour 2017",
       x = "Time",
       y = "Trend") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_vline(xintercept = as.Date(sos_dates), color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = as.Date(pos_dates), color = "green", linetype = "dashed", size = 1) +
  geom_vline(xintercept = as.Date(eos_dates), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = as.Date(sos_dates), y = max(df_2017$trend, na.rm = TRUE), label = "SOS", color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date(pos_dates), y = max(df_2017$trend, na.rm = TRUE), label = "POS", color = "green", angle = 90, vjust = -0.5) +
  annotate("text", x = as.Date(eos_dates), y = max(df_2017$trend, na.rm = TRUE), label = "EOS", color = "red", angle = 90, vjust = -0.5)

####################Analyse de probabilité de rupture###########################
########################### Fixer des seuils ##################################

colors <- c("VH_RI" = "#668B8B", "VH" = "#2F766B", 
            "JH_RI" = "#CDC9A5", "JH" = "#84AC9D", "JV" = "#903B42")

# Convertir la colonne 'time' en type Date
vh_ri$time <- as.Date(vh_ri$time)

# Extraire le jour de l'année (DOY) et l'année
vh_ri <- vh_ri %>%
  mutate(DOY = yday(time), year = year(time))

# Créer une nouvelle colonne avec le format "jour/mois"
vh_ri <- vh_ri %>%
  mutate(day_month = format(as.Date(time), "%m-%d"))


# Calculer les quantiles par groupe d'indice
quantiles_by_indice <- pheno %>%
  group_by(indice) %>%
  summarize(
    Q0 = quantile(max_poba, probs = 0, na.rm = TRUE),
    Q25 = quantile(max_poba, probs = 0.25, na.rm = TRUE),
    Q50 = quantile(max_poba, probs = 0.50, na.rm = TRUE),
    Q75 = quantile(max_poba, probs = 0.75, na.rm = TRUE),
    Q100 = quantile(max_poba, probs = 1, na.rm = TRUE)
  )
print(quantiles_by_indice)

# Fusionner les tibbles 'pheno' et 'vh_ri' sur la colonne 'time'
pheno <- pheno %>%
  left_join(vh_ri %>% select(time, mean_ndvi), by = "time")
print(head(pheno))

library(writexl)
# Définir le chemin pour enregistrer le fichier
file_path <- "E:/MASSANE/phenologie/pheno_JH_RI.xlsx"
# Enregistrer le dataframe en fichier Excel
write_xlsx(pheno, path = file_path)
# Affichage du chemin pour confirmation
cat("Le fichier a été enregistré à l'emplacement :", file_path, "\n")
