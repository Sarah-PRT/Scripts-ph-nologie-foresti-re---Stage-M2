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
library(ggplot2)
library(reshape2)
library(gridExtra)


sheets <- excel_sheets("E:/MASSANE/Sentinel-2/BEAST/EVAL_modele.xlsx")
print(sheets)

marg_lik <- read_excel("E:/MASSANE/Sentinel-2/BEAST/EVAL_modele.xlsx", sheet = sheets[1])
R2 <- read_excel("E:/MASSANE/Sentinel-2/BEAST/EVAL_modele.xlsx", sheet = sheets[2])
RMSE <- read_excel("E:/MASSANE/Sentinel-2/BEAST/EVAL_modele.xlsx", sheet = sheets[3])
sig_2 <- read_excel("E:/MASSANE/Sentinel-2/BEAST/EVAL_modele.xlsx", sheet = sheets[4])


# Conversion des valeurs en chiffres dans chaque tibble
marg_lik <- marg_lik %>%
  mutate_all(as.numeric)

R2 <- R2 %>%
  mutate_all(as.numeric)

RMSE <- RMSE %>%
  mutate_all(as.numeric)

sig_2 <- sig_2 %>%
  mutate_all(as.numeric)


# Conversion des données en format long
marg_lik_long <- melt(marg_lik, id.vars = "...1")
R2_long <- melt(R2, id.vars = "...1")
RMSE_long <- melt(RMSE, id.vars = "...1")
sig_2_long <- melt(sig_2, id.vars = "...1")


# Fonction pour créer des heatmaps
create_heatmap <- function(data, title) {
  ggplot(data, aes(x = variable, y = factor(...1))) +
    geom_tile(aes(fill = value), color = "white") +
    scale_fill_gradient(low = "blue", high = "red") +
    theme_minimal() +
    labs(title = title, x = "Variables", y = "Années")
}

library(ggplot2)

# Supposons que marg_lik_long est déjà défini et converti en format long

library(ggplot2)

# Supposons que marg_lik_long est déjà défini et converti en format long

# Marginal Likelihood heatmap
heatmap_marg_lik <- ggplot(marg_lik_long, aes(x = variable, y = factor(...1))) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7") +  # Bornes de l'échelle de couleur
  theme_minimal() +
  labs(title = "Marginal Likelihood", x = "Variables", y = "Années") +
  theme(axis.text.x = element_text(size = 14),  # Taille de l'étiquette x
        axis.text.y = element_text(size = 14))  # Taille de l'étiquette y

# Exemple de heatmap avec plus de couleurs et échelle de 0 à 1 pour R2
heatmap_R2 <- ggplot(R2_long, aes(x = variable, y = factor(...1))) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradientn(colors = viridis(12), limits = c(0, 1)) +  # Utilisation de la palette viridis avec 10 couleurs
  theme_minimal() +
  labs(title = "R2", x = "Variables", y = "Années") +
  theme(axis.text.x = element_text(size = 12),  # Taille de l'étiquette x
        axis.text.y = element_text(size = 12))  # Taille de l'étiquette y

# Exemple de heatmap avec plus de couleurs et échelle de 0 à 1 pour RMSE
heatmap_RMSE <- ggplot(RMSE_long, aes(x = variable, y = factor(...1))) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradientn(colors = viridis(15), limits = c(0, 0.5)) +  # Utilisation de la palette plasma avec 10 couleurs
  theme_minimal() +
  labs(title = "RMSE", x = "Variables", y = "Années") +
  theme(axis.text.x = element_text(size = 12),  # Taille de l'étiquette x
        axis.text.y = element_text(size = 12))  # Taille de l'étiquette y

# Exemple de heatmap avec plus de couleurs et échelle de 0 à 1 pour Sigma^2
heatmap_sig_2 <- ggplot(sig_2_long, aes(x = variable, y = factor(...1))) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradientn(colors = magma(10), limits = c(0, 0.05)) +  # Utilisation de la palette magma avec 10 couleurs
  theme_minimal() +
  labs(title = "Sigma^2", x = "Variables", y = "Années") +
  theme(axis.text.x = element_text(size = 12),  # Taille de l'étiquette x
        axis.text.y = element_text(size = 12))  # Taille de l'étiquette y

# Affichage des heatmaps
print(heatmap_marg_lik)
print(heatmap_R2)
print(heatmap_RMSE)
print(heatmap_sig_2)

# Organiser les heatmaps sur la même ligne
grid.arrange(heatmap_marg_lik, heatmap_R2, heatmap_RMSE, heatmap_sig_2, ncol = 4)
