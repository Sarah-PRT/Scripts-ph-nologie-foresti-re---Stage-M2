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
library(tidyr)
library(dplyr)
library(openxlsx)
library(writexl)
library(Rbeast)

#pass <- "E:/MASSANE/Sentinel-2/Interp_Gaussien/vh_RI_NEW.xlsx"
#VH_RI <- read_excel(pass)
#print(VH_RI)

pass <- "E:/MASSANE/Sentinel-2/Corr_Rapport_Interp/df_618.xlsx"
#pass <- "E:/MASSANE/Sentinel-2/Corr_Rapport_Interp/JH_RI_noNA.xlsx"
VH_RI <- read_excel(pass)
print(VH_RI)
# Remplacer toutes les valeurs inférieures à 0 par NA
VH_RI <- VH_RI %>%
  mutate(across(mean_ndvi:cv_ndvi, ~ replace(.x, .x < 0, NA)))
print(VH_RI)


# Agréger les données au pas de temps de 5 jours
VH_RI <- VH_RI %>%
  mutate(time = floor_date(time, unit = "5 days")) %>%
  group_by(time) %>%
  summarise(
    mean_ndvi = mean(mean_ndvi, na.rm = TRUE),
    median_ndvi = mean(median_ndvi, na.rm = TRUE),
    std_ndvi = mean(std_ndvi, na.rm = TRUE),
    min_ndvi = mean(min_ndvi, na.rm = TRUE),
    max_ndvi = mean(max_ndvi, na.rm = TRUE),
    cv_ndvi = mean(cv_ndvi, na.rm = TRUE)
  ) %>%
  ungroup()

print(VH_RI)


# Conversion en date 
VH_RI$time <- as.Date(VH_RI$time, format="%d/%m/%Y")

# Extraire l'année de la colonne 'time'
VH_RI$year <- format(VH_RI$time, "%Y")
head(VH_RI)

# Ajouter la colonne has_NA
#VH_RI$has_NA <- is.na(VH_RI$mean_ndvi)
# Ajouter une colonne indiquant si la ligne contient des NA
VH_RI <- VH_RI %>%
  mutate(has_NA = rowSums(is.na(select(., -time))) > 0)

# Perform linear interpolation for NA values in each numeric column
VH_RI <- VH_RI %>%
  mutate(across(where(is.numeric), ~ na.approx(.x, na.rm = FALSE)))
new_VH_RI <- VH_RI

# Utiliser fill pour combler les valeurs manquantes par celles du bas
new_VH_RI <- new_VH_RI %>% fill(mean_ndvi, median_ndvi, std_ndvi, min_ndvi, max_ndvi, cv_ndvi, .direction = "up")
VH_RI <- VH_RI %>% fill(mean_ndvi, median_ndvi, std_ndvi, min_ndvi, max_ndvi, cv_ndvi, .direction = "up")
new_VH_RI <- new_VH_RI %>% fill(mean_ndvi, median_ndvi, std_ndvi, min_ndvi, max_ndvi, cv_ndvi, .direction = "down")
VH_RI <- VH_RI %>% fill(mean_ndvi, median_ndvi, std_ndvi, min_ndvi, max_ndvi, cv_ndvi, .direction = "down")

# Spécifier le chemin du fichier
file_path <- "E:/MASSANE/Sentinel-2/Interp_Gaussien/new_df_618.xlsx"
# Enregistrer le DataFrame dans un fichier Excel
write_xlsx(new_VH_RI, path = file_path)
# Vérifier que le fichier est créé
print(paste("Le fichier", file_path, "a été créé avec succès."))
# Spécifier le chemin du fichier
file_path <- "E:/MASSANE/Sentinel-2/Interp_Gaussien/new_df_618.xlsx"


# Supprimer la dernière colonne
VH_RI <- VH_RI[, -ncol(VH_RI)]
# Utiliser fill pour combler les valeurs manquantes par celles du bas
VH_RI <- VH_RI %>% fill(mean_ndvi, median_ndvi, std_ndvi, min_ndvi, max_ndvi, cv_ndvi, .direction = "up")

# Spécifier le chemin du fichier
file_path <- "E:/MASSANE/Sentinel-2/Interp_Gaussien/ROI_new_df_618.xlsx"
# Enregistrer le DataFrame dans un fichier Excel
write_xlsx(VH_RI, path = file_path)
# Vérifier que le fichier est créé
print(paste("Le fichier", file_path, "a été créé avec succès."))
# Spécifier le chemin du fichier
file_path <- "E:/MASSANE/Sentinel-2/Interp_Gaussien/ROI_new_df_618.xlsx"



# Compter le nombre de dates par année
counts_per_year <- VH_RI%>%
  mutate(year = year(time)) %>%
  group_by(year) %>%
  summarise(count = n())

# Afficher les résultats
print(counts_per_year)

# Supprimer la ligne de la date 2021-12-28 et 2023-12-28
#VH_RI <- VH_RI %>%
#  filter(!(time == as.Date("2021-12-28") | time == as.Date("2023-12-28")))

# Compter le nombre de dates par année
#counts_per_year <- VH_RI %>%
#  mutate(year = year(time)) %>%
#  group_by(year) %>%
#  summarise(count = n())

# Afficher les résultats
#print(counts_per_year)


# Ajouter une nouvelle colonne pour le Jour de l'Année (Day of Year, DOY)
VH_RI$DOY <- yday(VH_RI$time)
# Afficher les premières lignes pour vérifier
head(VH_RI)

# Séparer les données par année
df_list <- split(VH_RI, VH_RI$year)

# Nommer chaque dataframe par son année
for (year in names(df_list)) {
  assign(paste0("df_", year), df_list[[year]])
}

# Afficher les premières lignes des dataframes créés pour vérifier
lapply(df_list, head)


# Spécifier les DOY comme index
index_doy <- df_2017$DOY

mean_ndvi_ts <- zoo(df_2017$mean_ndvi, order.by = index_doy)
print(mean_ndvi_ts)

# Charger le package zoo pour manipuler des séries temporelles irrégulières
library(zoo)
library(dplyr)
library(lubridate)

head(df_2017)

###############################################################################
############# Etape 1, ajout de la courbe modélisée ###################################
###############################################################################

# Séparer les données par année
df_list <- split(VH_RI, VH_RI$year)

# Nommer chaque dataframe par son année
for (year in names(df_list)) {
  assign(paste0("df_", year), df_list[[year]])
}

# Répéter pour chaque année de 2017 à 2023
for (year in 2017:2023) {
  # Sélectionner le dataframe de l'année en cours
  df_year <- get(paste0("df_", year))
  
  # Spécifier les DOY comme index
  index_doy <- df_year$DOY
  
  # Créer une série temporelle zoo pour mean_ndvi
  mean_ndvi_ts <- zoo(df_year$mean_ndvi, order.by = index_doy)
  print(mean_ndvi_ts)
  
  # Spécifier les paramètres prior pour le modèle BEAST
  prior <- list()
  prior$trendMinOrder     = 0          
  prior$trendMaxOrder     = 1          
  prior$trendMinKnotNum   = 0          
  prior$trendMaxKnotNum   = 4         
  prior$trendMinSepDist   = 3          
  prior$trendLeftMargin   = 20         # Aucune détection de points de changement de tendance dans les 15 premiers points de données
  prior$trendRightMargin  = 30         # Aucune détection de points de changement de tendance dans les 10 derniers points de données
  prior$K_MAX             = 5        
  prior$precValue         = 1.5        
  prior$modelPriorType    = 1         
  prior$precPriorType     = 'uniform'
  
  # Ajuster un modèle BEAST sur la série temporelle pour détecter les points de rupture
  out <- beast(mean_ndvi_ts,
               season = 'none',    # Ajustement sans saisonnalité
               sseg.min = 30,      # Exemple: Minimum de 30 jours entre les points de rupture
               method = 'bayes',   # Méthode pour la détection des ruptures (Bayes)
               mcmc.samples = 10000,
               mcmc.burnin = 200,
               mcmc.chains = 3,
               prior = prior)      # Ajout des paramètres prior personnalisés
  
  # Afficher les résultats du modèle
  print(out)
  plot(out)
  
  str(out)
  out$R2
  out$RMSE
  
  # Extraction de time, changepoints et probabilities
  time <- out$time
  changepoints <- out$trend$cp
  probabilities <- out$trend$cpPr
  trend <- out$trend$trend
  
  # Création d'un dataframe avec les résultats pour l'année en cours
  df_results <- data.frame(
    time = time,
    changepoints = rep(NA, length(time)),
    probabilities = rep(NA, length(time))
  )
  
  # Remplissage des valeurs connues de changepoints et probabilities
  for (i in seq_along(changepoints)) {
    idx <- which(time == changepoints[i])
    if (length(idx) > 0) {
      df_results[idx, "changepoints"] <- changepoints[i]
      df_results[idx, "probabilities"] <- probabilities[i]
    }
  }
  
  # Fusionner les résultats dans le dataframe original pour l'année en cours
  df_year <- merge(df_year, df_results, by = "time", all.x = TRUE)
  
  # Extraction de time et Y pour la courbe modélisée
  Y <- out$trend$Y
  
  # Ajouter la courbe modélisée à df_year
  df_year$Y <- Y
  
  # Afficher le dataframe df_year mis à jour
  print(df_year)
  
  # Mettre à jour l'objet global df_year
  assign(paste0("df_", year), df_year, envir = .GlobalEnv)
}

###############################################################################
############### Etape 2, ajout des ruptures ###################################
###############################################################################

# Liste de vos dataframes par année (par exemple, df_2017, df_2018, ...)
df_list <- list(df_2017, df_2018, df_2019, df_2020, df_2021, df_2022, df_2023)  

# Fonction pour ajuster le modèle BEAST et extraire les résultats
adjust_beast_model <- function(df) {
  # Spécifier les DOY comme index pour la série temporelle mean_ndvi
  index_doy <- df$DOY
  mean_ndvi_ts <- zoo(df$mean_ndvi, order.by = index_doy)
  
  # Spécifier les paramètres prior pour le modèle BEAST
  prior <- list()
  prior$trendMinOrder     = 0          
  prior$trendMaxOrder     = 1          
  prior$trendMinKnotNum   = 0          
  prior$trendMaxKnotNum   = 10         
  prior$trendMinSepDist   = 3          
  prior$trendLeftMargin   = 20         # Aucune détection de points de changement de tendance dans les 15 premiers points de données
  prior$trendRightMargin  = 30         # Aucune détection de points de changement de tendance dans les 10 derniers points de données
  prior$K_MAX             = 22         
  prior$precValue         = 1.5        
  prior$modelPriorType    = 1         
  prior$precPriorType     = 'uniform'
  
  # Ajuster le modèle BEAST pour détecter les points de rupture
  out <- beast(mean_ndvi_ts,
               season = 'none',    # Ajustement sans saisonnalité
               sseg.min = 30,      # Exemple: Minimum de 30 jours entre les points de rupture
               method = 'bayes',   # Méthode pour la détection des ruptures (Bayes)
               mcmc.samples = 10000,
               mcmc.burnin = 200,
               mcmc.chains = 3,
               prior = prior)      # Ajout des paramètres prior personnalisés
  
  # Extraction de time, changepoints et probabilities
  time <- out$time
  changepoints <- out$trend$cp
  probabilities <- out$trend$cpPr
  
  # Création d'un dataframe pour les résultats de la détection de rupture
  df_results <- data.frame(
    time = time,
    changepoints = rep(NA, length(time)),
    probabilities = rep(NA, length(time))
  )
  
  # Remplir les valeurs connues de changepoints et probabilities
  for (j in seq_along(changepoints)) {
    idx <- which(time == changepoints[j])
    if (length(idx) > 0) {
      df_results[idx, "changepoints"] <- changepoints[j]
      df_results[idx, "probabilities"] <- probabilities[j]
    }
  }
  
  return(df_results)
}

# Appliquer la fonction adjust_beast_model à chaque dataframe par année
list_of_results <- lapply(df_list, adjust_beast_model)


# Nommer les résultats pour chaque année
names(list_of_results) <- c("df_2017_results", "df_2018_results", "df_2019_results", "df_2020_results", "df_2021_results", "df_2022_results", "df_2023_results")

# Extraire les résultats de la liste par indices
df_2017_results <- list_of_results[[1]]
df_2018_results <- list_of_results[[2]]
df_2019_results <- list_of_results[[3]]
df_2020_results <- list_of_results[[4]]
df_2021_results <- list_of_results[[5]]
df_2022_results <- list_of_results[[6]]
df_2023_results <- list_of_results[[7]]

# Fusionner les résultats de df_2017_results avec df_2017
df_2017 <- cbind(df_2017, df_2017_results[, c("changepoints", "probabilities")])
head(df_2017)
df_2018 <- cbind(df_2018, df_2018_results[, c("changepoints", "probabilities")])
head(df_2018)
df_2019 <- cbind(df_2019, df_2019_results[, c("changepoints", "probabilities")])
head(df_2019)
df_2020 <- cbind(df_2020, df_2020_results[, c("changepoints", "probabilities")])
head(df_2020)
df_2021 <- cbind(df_2021, df_2021_results[, c("changepoints", "probabilities")])
head(df_2021)
df_2022 <- cbind(df_2022, df_2022_results[, c("changepoints", "probabilities")])
head(df_2022)
df_2023 <- cbind(df_2023, df_2023_results[, c("changepoints", "probabilities")])
head(df_2023)


###############################################################################
########################## Etape 3 - Récupérer la Tendence   ###################
###############################################################################

adjust_beast_model <- function(df) {
  # Spécifier les DOY comme index pour la série temporelle mean_ndvi
  index_doy <- df$DOY
  mean_ndvi_ts <- zoo(df$mean_ndvi, order.by = index_doy)
  
  # Spécifier les paramètres prior pour le modèle BEAST
  prior <- list()
  prior$trendMinOrder     = 0          
  prior$trendMaxOrder     = 1          
  prior$trendMinKnotNum   = 0          
  prior$trendMaxKnotNum   = 10         
  prior$trendMinSepDist   = 3          
  prior$trendLeftMargin   = 20         # Aucune détection de points de changement de tendance dans les 20 premiers points de données
  prior$trendRightMargin  = 30         # Aucune détection de points de changement de tendance dans les 30 derniers points de données
  prior$K_MAX             = 22         
  prior$precValue         = 1.5        
  prior$modelPriorType    = 1         
  prior$precPriorType     = 'uniform'
  
  # Ajuster le modèle BEAST pour détecter les points de rupture
  out <- beast(mean_ndvi_ts,
               season = 'none',    # Ajustement sans saisonnalité
               sseg.min = 30,      # Exemple: Minimum de 30 jours entre les points de rupture
               method = 'bayes',   # Méthode pour la détection des ruptures (Bayes)
               mcmc.samples = 10000,
               mcmc.burnin = 200,
               mcmc.chains = 3,
               prior = prior)      # Ajout des paramètres prior personnalisés
  
  # Extraction de time, slp
  time <- out$time
  slp <- out$trend$slp
  
  # Création d'un dataframe pour les résultats de la pente de la tendance
  df_results <- data.frame(
    time = time,
    slp = slp
  )
  
  return(df_results)
}


list_of_slp <- lapply(df_list, adjust_beast_model)

df_2017 <- cbind(df_2017, list_of_slp[[1]][, "slp"])
df_2018 <- cbind(df_2018, list_of_slp[[2]][, "slp"])
df_2019 <- cbind(df_2019, list_of_slp[[3]][, "slp"])
df_2020 <- cbind(df_2020, list_of_slp[[4]][, "slp"])
df_2021 <- cbind(df_2021, list_of_slp[[5]][, "slp"])
df_2022 <- cbind(df_2022, list_of_slp[[6]][, "slp"])
df_2023 <- cbind(df_2023, list_of_slp[[7]][, "slp"])

head(df_2017)
head(df_2018)
head(df_2019)
head(df_2020)
head(df_2021)
head(df_2022)
head(df_2023)



###############################################################################
############# Etape 4 - Récupérer les prObas de changement   ###################
###############################################################################

adjust_beast_model <- function(df) {
  # Spécifier les DOY comme index pour la série temporelle mean_ndvi
  index_doy <- df$DOY
  mean_ndvi_ts <- zoo(df$mean_ndvi, order.by = index_doy)
  
  # Spécifier les paramètres prior pour le modèle BEAST
  prior <- list()
  prior$trendMinOrder     = 0          
  prior$trendMaxOrder     = 1          
  prior$trendMinKnotNum   = 0          
  prior$trendMaxKnotNum   = 10         
  prior$trendMinSepDist   = 3          
  prior$trendLeftMargin   = 50         # Aucune détection de points de changement de tendance dans les 20 premiers points de données
  prior$trendRightMargin  = 30         # Aucune détection de points de changement de tendance dans les 30 derniers points de données
  prior$K_MAX             = 22         
  prior$precValue         = 1.5        
  prior$modelPriorType    = 1         
  prior$precPriorType     = 'uniform'
  
  # Ajuster le modèle BEAST pour détecter les points de rupture
  out <- beast(mean_ndvi_ts,
               season = 'none',    # Ajustement sans saisonnalité
               sseg.min = 30,      # Exemple: Minimum de 30 jours entre les points de rupture
               method = 'bayes',   # Méthode pour la détection des ruptures (Bayes)
               mcmc.samples = 10000,
               mcmc.burnin = 200,
               mcmc.chains = 3,
               prior = prior)      # Ajout des paramètres prior personnalisés
  
  # Extraction de time, cpOccPr
  time <- out$time
  cpOccPr <- out$trend$cpOccPr
  
  # Création d'un dataframe pour les résultats de la probabilité d'occurrence des changepoints
  df_results <- data.frame(
    time = time,
    cpOccPr = cpOccPr
  )
  
  return(df_results)
}

list_of_cpOccPr <- lapply(df_list, adjust_beast_model)


df_2017 <- cbind(df_2017, list_of_cpOccPr[[1]][, "cpOccPr"])
df_2018 <- cbind(df_2018, list_of_cpOccPr[[2]][, "cpOccPr"])
df_2019 <- cbind(df_2019, list_of_cpOccPr[[3]][, "cpOccPr"])
df_2020 <- cbind(df_2020, list_of_cpOccPr[[4]][, "cpOccPr"])
df_2021 <- cbind(df_2021, list_of_cpOccPr[[5]][, "cpOccPr"])
df_2022 <- cbind(df_2022, list_of_cpOccPr[[6]][, "cpOccPr"])
df_2023 <- cbind(df_2023, list_of_cpOccPr[[7]][, "cpOccPr"])


head(df_2017)
head(df_2018)
head(df_2019)
head(df_2020)
head(df_2021)
head(df_2023)


################################################################################
###############################################################################
################### Récupérer les métriques de vérifications ###############
adjust_beast_model <- function(df) {
  # Spécifier les DOY comme index pour la série temporelle mean_ndvi
  index_doy <- df$DOY
  mean_ndvi_ts <- zoo(df$mean_ndvi, order.by = index_doy)
  
  # Ajuster le modèle BEAST pour détecter les points de rupture
  out <- beast(mean_ndvi_ts,
               season = 'none',    # Ajustement sans saisonnalité
               sseg.min = 30,      # Exemple: Minimum de 30 jours entre les points de rupture
               method = 'bayes',   # Méthode pour la détection des ruptures (Bayes)
               mcmc.samples = 10000,
               mcmc.burnin = 200,
               mcmc.chains = 3)
  
  # Extraction de marg_lik, R2, RMSE et sig2
  marg_lik <- out$marg_lik
  R2 <- out$R2
  RMSE <- out$RMSE
  sig2 <- out$sig2
  
  # Retourner les résultats sous forme de liste
  return(list(marg_lik = marg_lik, R2 = R2, RMSE = RMSE, sig2 = sig2))
}

# Liste des dataframes par année (ajustée selon votre structure de noms)
df_list <- list(df_2017, df_2018, df_2019, df_2020, df_2021, df_2022, df_2023)  

  # Initialisation d'une liste pour stocker les résultats
list_of_results <- list()

# Boucle pour ajuster le modèle et extraire les métriques pour chaque année
for (i in seq_along(df_list)) {
  # Appliquer la fonction adjust_beast_model à chaque dataframe
  metrics <- adjust_beast_model(df_list[[i]])
  
  # Stocker les résultats dans une liste avec des noms d'année appropriés
  year <- 2017 + i - 1  # Calculer l'année en cours
  list_of_results[[paste0("df_", year, "_metrics")]] <- metrics
}

# Afficher les résultats pour chaque année
list_of_results


# Initialisation d'une liste pour stocker les résultats
list_of_results <- list()

# Boucle pour ajuster le modèle et extraire les métriques pour chaque année
for (i in seq_along(df_list)) {
  # Appliquer la fonction adjust_beast_model à chaque dataframe
  metrics <- adjust_beast_model(df_list[[i]])
  
  # Stocker les résultats dans une liste
  list_of_results[[i]] <- metrics
}

# Créer un dataframe global à partir de la liste des résultats
results_df <- do.call(rbind, list_of_results)

# Afficher le dataframe global
print(results_df)

# Plot interactif
#plot(out, interactive=TRUE)

###########

# Suppression des colonnes 10 et 11 pour chaque dataframe
df_2017 <- df_2017[, -c(10, 11)]
df_2018 <- df_2018[, -c(10, 11)]
df_2019 <- df_2019[, -c(10, 11)]
df_2020 <- df_2020[, -c(10, 11)]
df_2021 <- df_2021[, -c(10, 11)]
df_2022 <- df_2022[, -c(10, 11)]
df_2023 <- df_2023[, -c(10, 11)]

# Renommer les colonnes de chaque dataframe
colnames(df_2017) <- c("time", "mean_ndvi", "median_ndvi", "std_ndvi", "min_ndvi", "max_ndvi", "cv_ndvi", "year", "DOY", "mod", "breakpoint", "proba_sign", "trend", "poba" )
colnames(df_2018) <- c("time", "mean_ndvi", "median_ndvi", "std_ndvi", "min_ndvi", "max_ndvi", "cv_ndvi", "year", "DOY", "mod", "breakpoint", "proba_sign", "trend", "poba" )
colnames(df_2019) <- c("time", "mean_ndvi", "median_ndvi", "std_ndvi", "min_ndvi", "max_ndvi", "cv_ndvi", "year", "DOY", "mod", "breakpoint", "proba_sign", "trend", "poba" )
colnames(df_2020) <- c("time", "mean_ndvi", "median_ndvi", "std_ndvi", "min_ndvi", "max_ndvi", "cv_ndvi", "year", "DOY", "mod", "breakpoint", "proba_sign", "trend", "poba" )
colnames(df_2021) <- c("time", "mean_ndvi", "median_ndvi", "std_ndvi", "min_ndvi", "max_ndvi", "cv_ndvi", "year", "DOY", "mod", "breakpoint", "proba_sign", "trend", "poba" )
colnames(df_2022) <- c("time", "mean_ndvi", "median_ndvi", "std_ndvi", "min_ndvi", "max_ndvi", "cv_ndvi", "year", "DOY", "mod", "breakpoint", "proba_sign", "trend", "poba" )
colnames(df_2023) <- c("time", "mean_ndvi", "median_ndvi", "std_ndvi", "min_ndvi", "max_ndvi", "cv_ndvi", "year", "DOY", "mod", "breakpoint", "proba_sign", "trend", "poba" )

# Assembler tous les dataframes en un seul
df_combined1 <- rbind(df_2017, df_2018, df_2019, df_2020, df_2021, df_2022, df_2023)

# Afficher le dataframe combiné
print(head(df_combined1))




##################### Sauvegarde ##############################################
library(openxlsx)

#  sauvegarder un dataframe dans un fichier Excel
save_to_excel <- function(dataframe, file_name) {
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(wb, "Sheet1", dataframe)
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

# Spécifier le chemin du fichier Excel
file_path <- "E:/MASSANE/Sentinel-2/BEAST.xlsx"

# Spécifier les chemins des fichiers Excel pour chaque dataframe
file_paths <- list(
  "E:/MASSANE/Sentinel-2/BEAST/BEAST_df_618_CORR.xlsx",
  "E:/MASSANE/Sentinel-2/BEAST/perf_BEAST_df_618_CORR.xlsx"
)

# Liste des dataframes
df_list <- list(df_combined1, results_df)

# Enregistrer chaque dataframe dans un fichier Excel séparé
for (i in 1:length(df_list)) {
  save_to_excel(df_list[[i]], file_paths[[i]])
  cat("Fichier Excel enregistré avec succès à l'emplacement :", file_paths[[i]], "\n")
}


