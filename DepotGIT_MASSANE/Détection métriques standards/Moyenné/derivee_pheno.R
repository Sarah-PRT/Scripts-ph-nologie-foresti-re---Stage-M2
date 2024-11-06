library(dplyr)
library(ggplot2)
library(lubridate)

# Calcul de l'amplitude de la variation annuelle pour chaque colonne
annual_variation <- vh_ri %>%
  group_by(year) %>%
  summarize(
    range_mean_ndvi = diff(range(mean_ndvi)),
    range_loess = diff(range(loess)),
    range_SG = diff(range(SG))
  )

# Affichons les premières lignes de annual_variation
head(annual_variation)

# Jointure avec combined_data
vh_ri <- left_join(vh_ri, annual_variation, by = "year") 
vh_ri <- vh_ri %>%
  select(-matches("\\.x|\\.y"))

# Calcul des dérivées
vh_ri <- vh_ri %>%
  group_by(year) %>%
  arrange(DOY) %>%
  mutate(
    diff_mean_ndvi = c(NA, diff(mean_ndvi)),
    diff_loess = c(NA, diff(loess)),
    diff_SG = c(NA, diff(SG))
  ) %>%
  ungroup()

# Calcul des événements saisonniers pour chaque année (points extrêmes)
season_extreme_50 <- vh_ri %>%
  group_by(year) %>%
  summarize(
    SOS_brut = ifelse(any(diff_mean_ndvi > 0 & DOY >= 80 & DOY <= 105), min(DOY[diff_mean_ndvi > 0 & DOY >= 70 & DOY <= 100], na.rm = TRUE), NA),
    SOS_loess = ifelse(any(diff_loess > 0 & DOY >= 80 & DOY <= 105), min(DOY[diff_loess > 0 & DOY >= 70 & DOY <= 100], na.rm = TRUE), NA),
    SOS_SG = ifelse(any(diff_SG > 0 & DOY >= 80 & DOY <= 105), min(DOY[diff_SG > 0 & DOY >= 70 & DOY <= 100], na.rm = TRUE), NA),
    POS_brut = ifelse(any(DOY >= 121 & DOY <= 171), max(DOY[DOY >= 121 & DOY <= 171 & mean_ndvi == max(mean_ndvi[DOY >= 121 & DOY <= 171], na.rm = TRUE)], na.rm = TRUE), NA),
    POS_loess = ifelse(any(DOY >= 121 & DOY <= 171), max(DOY[DOY >= 121 & DOY <= 171 & loess == max(loess[DOY >= 121 & DOY <= 171], na.rm = TRUE)], na.rm = TRUE), NA),
    POS_SG = ifelse(any(DOY >= 121 & DOY <= 171), max(DOY[DOY >= 121 & DOY <= 171 & SG == max(SG[DOY >= 121 & DOY <= 171], na.rm = TRUE)], na.rm = TRUE), NA),
    EOS_brut = ifelse(any(diff_mean_ndvi < 0 & DOY >= 288 & DOY <= 330), min(DOY[diff_mean_ndvi < 0 & DOY >= 288 & DOY <= 330], na.rm = TRUE), NA),
    EOS_loess = ifelse(any(diff_loess < 0 & DOY >= 288 & DOY <= 330), min(DOY[diff_loess < 0 & DOY >= 288 & DOY <= 330], na.rm = TRUE), NA),
    EOS_SG = ifelse(any(diff_SG < 0 & DOY >= 288 & DOY <= 330), min(DOY[diff_SG < 0 & DOY >= 288 & DOY <= 330], na.rm = TRUE), NA)
  )

# Plot avec séparation des années et segments verticaux pour SOS, POS et EOS
ggplot(vh_ri, aes(x = DOY)) +
  geom_line(aes(y = mean_ndvi, color = "Mean NDVI"), size = 1) +
  geom_line(aes(y = SG, color = "SG"), size = 2, linetype = "dotted") +
  geom_line(aes(y = loess, color = "Loess"), size = 2, linetype = "dashed") +
  geom_vline(data = season_extreme_50, aes(xintercept = SOS_brut, color = "SOS (brut)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme_50, aes(xintercept = POS_brut, color = "POS (brut)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme_50, aes(xintercept = EOS_brut, color = "EOS (brut)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme_50, aes(xintercept = SOS_loess, color = "SOS (loess)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme_50, aes(xintercept = POS_loess, color = "POS (loess)"), linetype = "dashed", size = 1) +
  geom_vline(data = season_extreme_50, aes(xintercept = EOS_loess, color = "EOS (loess)"), linetype = "dashed", size = 1) +
  labs(x = "DOY", y = "NDVI", color = "") +
  scale_color_manual(values = c("purple", "lightgreen", "black", "darkred", "pink", "green", "blue", "red", "yellow", "skyblue","grey")) +
  scale_x_continuous(breaks = seq(0, 365, by = 50)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal() +
  theme(
    axis.ticks.x = element_line(color = "black", size = 1),
    axis.ticks.y = element_line(color = "black", size = 1),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  facet_wrap(~ year, scales = "free_x", nrow = 4, ncol = 2) +
  ylim(0, 1)
