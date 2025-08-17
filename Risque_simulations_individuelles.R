##Bloc d'installation des packages
if (1 == 1) {
# Liste des packages nécessaires
required_packages <- c("raster", "readxl", "lubridate", "dplyr", "ggplot2", "RColorBrewer", "scales")

# Fonction d’installation si manquant
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Installer les packages manquants
invisible(sapply(required_packages, install_if_missing))

# Charger les packages
lapply(required_packages, library, character.only = TRUE)
}

setwd("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R")
##Conversion des fichiers de dépôts txt en GeoTIFF
if (1 == 1) {
##Conversion des fichiers txt en GeoTIFF pour etna_2002_1_8
library(raster)

# Dossiers d'entrée et de sortie
input_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/Simulations individuelles txt/SUM_manual"
output_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/Simulations individuelles GeoTiff"

# Boucle sur les fichiers
for (i in 1:1000) {
  # Générer le nom du fichier avec zéro-padding
  file_name <- sprintf("SUM_sim_%04d.txt", i)
  file_path <- file.path(input_dir, file_name)
  
  if (file.exists(file_path)) {
    # Lire et traiter le fichier
    df <- read.csv(file_path, header = FALSE)
    colnames(df) <- c("x", "y", "z", "val")
    
    r <- rasterFromXYZ(df[, c("x", "y", "val")])
    
    # Définir le nom de sortie
    output_file <- file.path(output_dir, sprintf("SUM_sim_%04d.tif", i))
    writeRaster(r, output_file, format = "GTiff", overwrite = TRUE)
  } else {
    cat(sprintf("Fichier non trouvé : %s\n", file_path))
  }
}


##Conversion des fichiers txt en GeoTIFF pour sub_plinian_1

# Dossiers d'entrée et de sortie
input_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/Simulations individuelles txt/SUM_short"
output_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/Simulations individuelles GeoTiff"

# Boucle sur les fichiers
for (i in 1:1000) {
  # Générer le nom du fichier avec zéro-padding
  file_name <- sprintf("OUT_sim_%04d.txt", i)
  file_path <- file.path(input_dir, file_name)
  
  if (file.exists(file_path)) {
    # Lire et traiter le fichier
    df <- read.csv(file_path, header = FALSE)
    colnames(df) <- c("x", "y", "z", "val")
    
    r <- rasterFromXYZ(df[, c("x", "y", "val")])
    
    # Définir le nom de sortie
    output_file <- file.path(output_dir, sprintf("OUT_sim_%04d.tif", i))
    writeRaster(r, output_file, format = "GTiff", overwrite = TRUE)
  } else {
    cat(sprintf("Fichier non trouvé : %s\n", file_path))
  }
}

}


library("readxl")
library(lubridate)
library(dplyr)
library(ggplot2)
library(raster)

lulc <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/lulc_current.tif")
xy <- coordinates(lulc)
lu <- getValues(lulc)
npixels <- ncell(lulc)

###MODELE POUR SUB PLINIAN ERUPTION
if (1 == 1) {

## === Étape 1 Construire la matrice de dépôts des simulations subpliniennes
mat_dep_subp <- data.frame(x = xy[,1], y = xy[,2], lu = lu)
mat_dep_subp$lu[is.na(mat_dep_subp$lu)] <- 0
mat_dep_subp$lu[mat_dep_subp$lu %in% c(101, 110)] <- 100

## === Étape 2 Lire les données
# Lire la table des périodes (jours julien)
calendrier <- read_excel("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/parametres.xlsx", sheet = "calendrier")

# Lire les dates de simulation
dates_subp <- read_excel("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/dates_simulations_subplinian_1.xlsx")
dates_subp$yday <- yday(dates_subp$StartDate)

## === Étape 3 simulation à une période
find_period <- function(day) {
  i <- which(day >= calendrier$start_day & day <= calendrier$end_day)
  if (length(i) == 1) return(i) else return(NA)
}
dates_subp$period <- sapply(dates_subp$yday, find_period)

## === Étape 4 Initialiser des vecteurs pour stocker la somme des dépôts et le nombre de simulations par période
dep_sums <- list()
dep_counts <- list()

for (p in 1:24) {
  dep_sums[[p]] <- rep(0, npixels)   # vecteur de taille = nombre de pixels
  dep_counts[[p]] <- 0               # compteur de simulations
}

##Boucle sur les 1000 simulations et accumulation
input_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/Simulations individuelles GeoTiff"
pb <- txtProgressBar(min = 1, max = 1000, style = 3)

for (i in 1:1000) {
  tif_path <- file.path(input_dir, sprintf("OUT_sim_%04d.tif", i))
  if (!file.exists(tif_path)) {
    setTxtProgressBar(pb, i)
    next
  }
  
  p <- dates_subp$period[i]
  if (is.na(p)) {
    setTxtProgressBar(pb, i)
    next
  }
  
  dep <- raster(tif_path)
  dep_res <- resample(dep, lulc, method = "bilinear")
  dep_res[is.na(dep_res)] <- 0
  dep_vals <- getValues(dep_res)
  
  dep_sums[[p]] <- dep_sums[[p]] + dep_vals
  dep_counts[[p]] <- dep_counts[[p]] + 1
  
  setTxtProgressBar(pb, i)
}
close(pb)
## === Étape 5 Moyenne des dépôts par pixel et ajout dans mat_yl_subp
# Ajouter les colonnes de dépôts moyens par période dans la matrice de dépôt
for (p in 1:24) {
  colname <- sprintf("mean_loading_p%d", p)
  if (dep_counts[[p]] > 0) {
    mat_dep_subp[[colname]] <- dep_sums[[p]] / dep_counts[[p]]
  } else {
    mat_dep_subp[[colname]] <- 0
  }
}

##  === Étape 6 Calcul de la perte par période
#Paramètres des équations de fonctions de fragilité
eq <- read_excel("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/parametres.xlsx", sheet = "eq")
# Fonction pour récupérer les bons paramètres dans eq
get_params_from_eq <- function(stage_code, is_vigne) {
  prefix <- if (is_vigne) "v" else "ft"
  a_col <- paste0(prefix, "_", stage_code, "_a")
  b_col <- paste0(prefix, "_", stage_code, "_b")
  list(a = eq[[a_col]], b = eq[[b_col]])
}

#Equation générique des fonctions de fragilité
exp_model <- function(x, a, b) {
  b * (1 - exp(-x / a))
}

# Lire la feuille "s" pour les stades par période des cultures
s <- read_excel("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/parametres.xlsx", sheet = "s")

# Boucle sur les périodes 1 à 24
for (p in 1:24) {
  
  varname <- paste0("mean_loading_p", p)  # colonne des dépôts moyens
  yl_temp <- rep(-9999, nrow(mat_dep_subp))  # valeurs par défaut
  
  # Olives = 100
  stage_code <- s[p, "ol"][[1]]
  if (!is.na(stage_code)) {
    pars <- get_params_from_eq(stage_code, is_vigne = FALSE)
    yl_temp[mat_dep_subp$lu == 100] <- exp_model(mat_dep_subp[mat_dep_subp$lu == 100, varname], pars$a, pars$b)
  }
  
  # Vergers = 92
  stage_code <- s[p, "or"][[1]]
  if (!is.na(stage_code)) {
    pars <- get_params_from_eq(stage_code, is_vigne = FALSE)
    yl_temp[mat_dep_subp$lu == 92] <- exp_model(mat_dep_subp[mat_dep_subp$lu == 92, varname], pars$a, pars$b)
  }
  
  # Vignes = 73
  stage_code <- s[p, "v"][[1]]
  if (!is.na(stage_code)) {
    pars <- get_params_from_eq(stage_code, is_vigne = TRUE)
    yl_temp[mat_dep_subp$lu == 73] <- exp_model(mat_dep_subp[mat_dep_subp$lu == 73, varname], pars$a, pars$b)
  }
  
  # Ajouter la colonne dans mat_dep_subp
  new_col <- paste0("yl_p", p)
  mat_dep_subp[[new_col]] <- yl_temp
}

## Convertir les colonnes yl_p1 à yl_p24 en rasters
dir.create("Tiff risk pertes sub plinian", showWarnings = FALSE)
dir.create("png risk pertes sub plinian", showWarnings = FALSE)

library(RColorBrewer)

# Boucle sur les périodes p1 à p24
for (p in 1:24) {
  
  colname <- sprintf("yl_p%d", p)
  
  # Vérifie que la colonne existe
  if (!colname %in% names(mat_dep_subp)) next
  
  # Crée le raster à partir des pertes
  r <- rasterFromXYZ(cbind(mat_dep_subp$x, mat_dep_subp$y, mat_dep_subp[[colname]]), 
                     crs = crs(lulc))
  
  # Enregistre GeoTIFF
  tif_path <- file.path("Tiff risk pertes sub plinian", paste0("yl_p", p, ".tif"))
  writeRaster(r, tif_path, format = "GTiff", overwrite = TRUE)
  
  # === Plot PNG ===
  png_path <- file.path("png risk pertes sub plinian", paste0("yl_p", p, ".png"))
  png(png_path, width = 700, height = 700)
  
  # Palette de couleurs du vert (0%) au rouge (100%) avec + d'orange
  pal <- colorRampPalette(c("green", "yellow", "orange", "red"))(100)
  breaks <- seq(0, 100, by = 1)
  
  plot(r, 
       main = sprintf("Expected crop yield loss for a sub plinian eruption in period %d", p),
       col = pal,
       breaks = breaks,
       zlim = c(0, 100),
       legend.args = list(text = "Crop loss (%)", side = 4, font = 2, line = 2.5, cex = 1.2)
  )
  
  dev.off()
}

}
###MODELE POUR STROMBOLIAN ERUPTION
if (1 == 1) {
library(raster)
library(readxl)
library(lubridate)
library(dplyr)

## === Étape 1 : initialisation des bases ===
mat_dep_stromb <- data.frame(x = xy[,1], y = xy[,2], lu = lu)
mat_dep_stromb$lu[is.na(mat_dep_stromb$lu)] <- 0
mat_dep_stromb$lu[mat_dep_stromb$lu %in% c(101, 110)] <- 100

## === Étape 2 : lire les données ===
calendrier <- read_excel("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/parametres.xlsx", sheet = "calendrier")
dates_stromb <- read_excel("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/dates_simulations_etna_2002_1_8.xlsx")
s <- read_excel("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/parametres.xlsx", sheet = "s")
eq <- read_excel("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/parametres.xlsx", sheet = "eq")

## === Étape 3 : associer chaque simulation à la période la plus sensible qu'elle chevauche ===

# Ajouter le jour julien
dates_stromb$start_yday <- yday(dates_stromb$StartDate)
dates_stromb$end_yday   <- yday(dates_stromb$EndDate)

# Fonction pour trouver la période la plus sensible
get_most_sensitive_period <- function(start_day, end_day) {
  chevauche <- calendrier$p[calendrier$end_day >= start_day & calendrier$start_day <= end_day]
  if (length(chevauche) == 0) return(NA)
  
  a_vals <- sapply(chevauche, function(p) {
    codes <- as.character(s[p, c("v", "ol", "or")])
    a_list <- c()
    for (j in 1:3) {
      if (!is.na(codes[j])) {
        prefix <- if (j == 1) "v_" else "ft_"
        colname <- paste0(prefix, codes[j], "_a")
        if (colname %in% names(eq)) {
          a_list <- c(a_list, eq[[colname]])
        }
      }
    }
    if (length(a_list) > 0) min(a_list) else Inf
  })
  
  chevauche[which.min(a_vals)]
}

# Appliquer à toutes les lignes
dates_stromb$period <- mapply(get_most_sensitive_period, dates_stromb$start_yday, dates_stromb$end_yday)
dates_stromb$period <- as.integer(gsub("p", "", dates_stromb$period))

## === Étape 4 : accumulation des dépôts ===
dep_sums <- list()
dep_counts <- list()
for (p in 1:24) {
  dep_sums[[p]] <- rep(0, npixels)
  dep_counts[[p]] <- 0
}

input_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/Simulations individuelles GeoTiff"
pb <- txtProgressBar(min = 1, max = 1000, style = 3)

for (i in 1:1000) {
  tif_path <- file.path(input_dir, sprintf("SUM_sim_%04d.tif", i))
  if (!file.exists(tif_path)) {
    setTxtProgressBar(pb, i)
    next
  }
  
  p <- dates_stromb$period[i]
  if (is.na(p)) {
    setTxtProgressBar(pb, i)
    next
  }
  
  dep <- raster(tif_path)
  dep_res <- resample(dep, lulc, method = "bilinear")
  dep_res[is.na(dep_res)] <- 0
  dep_vals <- getValues(dep_res)
  
  dep_sums[[p]] <- dep_sums[[p]] + dep_vals
  dep_counts[[p]] <- dep_counts[[p]] + 1
  
  setTxtProgressBar(pb, i)
}
close(pb)

## === Étape 5 : moyenne des dépôts par période ===
for (p in 1:24) {
  colname <- sprintf("mean_loading_p%d", p)
  if (dep_counts[[p]] > 0) {
    mat_dep_stromb[[colname]] <- dep_sums[[p]] / dep_counts[[p]]
  } else {
    mat_dep_stromb[[colname]] <- 0
  }
}

## === Étape 6 : calcul des pertes (yield loss) ===
get_params_from_eq <- function(stage_code, is_vigne) {
  prefix <- if (is_vigne) "v" else "ft"
  a_col <- paste0(prefix, "_", stage_code, "_a")
  b_col <- paste0(prefix, "_", stage_code, "_b")
  list(a = eq[[a_col]], b = eq[[b_col]])
}

exp_model <- function(x, a, b) {
  b * (1 - exp(-x / a))
}

for (p in 1:24) {
  varname <- paste0("mean_loading_p", p)
  yl_temp <- rep(-9999, nrow(mat_dep_stromb))
  
  stage_code <- s[p, "ol"][[1]]
  if (!is.na(stage_code)) {
    pars <- get_params_from_eq(stage_code, FALSE)
    yl_temp[mat_dep_stromb$lu == 100] <- exp_model(mat_dep_stromb[mat_dep_stromb$lu == 100, varname], pars$a, pars$b)
  }
  
  stage_code <- s[p, "or"][[1]]
  if (!is.na(stage_code)) {
    pars <- get_params_from_eq(stage_code, FALSE)
    yl_temp[mat_dep_stromb$lu == 92] <- exp_model(mat_dep_stromb[mat_dep_stromb$lu == 92, varname], pars$a, pars$b)
  }
  
  stage_code <- s[p, "v"][[1]]
  if (!is.na(stage_code)) {
    pars <- get_params_from_eq(stage_code, TRUE)
    yl_temp[mat_dep_stromb$lu == 73] <- exp_model(mat_dep_stromb[mat_dep_stromb$lu == 73, varname], pars$a, pars$b)
  }
  
  mat_dep_stromb[[paste0("yl_p", p)]] <- yl_temp
}
## === Étape 7 : export en GeoTiff et PNG ===
# Créer les dossiers de sortie s'ils n'existent pas
dir.create("Tiff risk pertes strombolien", showWarnings = FALSE)
dir.create("png risk pertes strombolien", showWarnings = FALSE)

# Palette de couleurs du vert (0%) au rouge (100%) avec une large plage orange
pal <- colorRampPalette(c("green", "yellow", "orange", "red"))(100)
breaks <- seq(0, 100, by = 1)

# Boucle sur les périodes
for (p in 1:24) {
  
  colname <- sprintf("yl_p%d", p)
  
  if (!colname %in% names(mat_dep_stromb)) next  # sécurité
  
  # === Création du raster ===
  vals <- mat_dep_stromb[[colname]]
  vals[vals == -9999] <- NA  # Masquer les pertes hors culture
  r <- rasterFromXYZ(cbind(mat_dep_stromb$x, mat_dep_stromb$y, vals), 
                     crs = crs(lulc))
  
  # === Export GeoTIFF ===
  tif_path <- file.path("Tiff risk pertes strombolien", paste0("yl_p", p, ".tif"))
  writeRaster(r, tif_path, format = "GTiff", overwrite = TRUE)
  
  # === Export PNG ===
  png_path <- file.path("png risk pertes strombolien", paste0("yl_p", p, ".png"))
  png(png_path, width = 700, height = 700)
  
  plot(r,
       main = sprintf("Expected crop yield loss for a strombolian eruption in period %d", p),
       col = pal,
       breaks = breaks,
       zlim = c(0, 100),
       legend.args = list(text = "Crop loss (%)", side = 4, font = 2, line = 2.5, cex = 1.2))
  
  dev.off()
}
}
###Annualized Index of Yield Loss
if (1 == 1) {

##Chargement des données de pertes et calcul du AIYL
  #Sub-plinienne
# Initialiser ton dataframe
  mat_dep_subp <- data.frame(x = xy[,1], y = xy[,2], lu = lu)
  mat_dep_subp$lu[is.na(mat_dep_subp$lu)] <- 0
  mat_dep_subp$lu[mat_dep_subp$lu %in% c(101, 110)] <- 100
  
# Boucle sur p = 1 à 24
  for (p in 1:24) {
    tif_path <- sprintf("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/Tiff risk pertes sub plinian ind/yl_p%d.tif", p)
    r <- raster(tif_path)
    vals <- getValues(r)
    
    # Vérifie que la longueur match
    if (length(vals) != nrow(mat_dep_subp)) {
      stop(sprintf("Mauvaise longueur : raster p%d (%d) vs mat_dep_subp (%d)", 
                   p, length(vals), nrow(mat_dep_subp)))
    }
    
    # Ajouter colonne
    mat_dep_subp[[paste0("yl_p", p)]] <- vals
  }
  
# Calcul de l'AIYL 
cols_yl_subp <- grep("^yl_p\\d+$", names(mat_dep_subp), value = TRUE)
mat_dep_subp$AIYL_subp <- rowSums(mat_dep_subp[, cols_yl_subp], na.rm = TRUE) / 24

  #Strombolienne
# Initialiser ton dataframe
mat_dep_stromb <- data.frame(x = xy[,1], y = xy[,2], lu = lu)
mat_dep_stromb$lu[is.na(mat_dep_stromb$lu)] <- 0
mat_dep_stromb$lu[mat_dep_stromb$lu %in% c(101, 110)] <- 100

# Boucle sur p = 1 à 24
for (p in 1:24) {
  tif_path <- sprintf("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/Tiff risk pertes strombolien ind/yl_p%d.tif", p)
  r <- raster(tif_path)
  vals <- getValues(r)
  
  # Vérifie que la longueur match
  if (length(vals) != nrow(mat_dep_stromb)) {
    stop(sprintf("Mauvaise longueur : raster p%d (%d) vs mat_dep_stromb (%d)", 
                 p, length(vals), nrow(mat_dep_stromb)))
  }
  
  # Ajouter colonne
  mat_dep_stromb[[paste0("yl_p", p)]] <- vals
}

#Calcul de l'AIYL
cols_yl_stromb <- grep("^yl_p\\d+$", names(mat_dep_stromb), value = TRUE)
mat_dep_stromb$AIYL_stromb <- rowSums(mat_dep_stromb[, cols_yl_stromb], na.rm = TRUE) / 24
}
###Sauvegarde en GeoTIFF du AIYL pour le modèle 1000 simulations
if (1 == 1) {
  ref_raster <- raster("lulc_current.tif")
  crs_correct <- CRS("+init=EPSG:32633")
  crs(ref_raster) <- crs_correct
  dir.create("TIF_AIYL_STROMB_ind", showWarnings = FALSE)
  dir.create("TIF_AIYL_SUBP_ind", showWarnings = FALSE)
  # --- Strombolien ---
  vals_stromb <- rep(NA, nrow(mat_dep_stromb))
  sel_stromb <- mat_dep_stromb$lu %in% c(100, 92, 73)
  vals_stromb[sel_stromb] <- mat_dep_stromb$AIYL_stromb[sel_stromb]
  r_stromb <- rasterFromXYZ(cbind(mat_dep_stromb$x, mat_dep_stromb$y, vals_stromb),
                            crs = crs_correct,
                            res = res(ref_raster))
  extent(r_stromb) <- extent(ref_raster)
  projection(r_stromb) <- crs_correct
  
  writeRaster(r_stromb, "TIF_AIYL_STROMB_ind/AIYL_stromb_ind.tif", overwrite = TRUE)
  
  # --- Sub-plinien ---
  vals_subp <- rep(NA, nrow(mat_dep_subp))
  sel_subp <- mat_dep_subp$lu %in% c(100, 92, 73)
  vals_subp[sel_subp] <- mat_dep_subp$AIYL_subp[sel_subp]
  r_subp <- rasterFromXYZ(cbind(mat_dep_subp$x, mat_dep_subp$y, vals_subp),
                          crs = crs_correct,
                          res = res(ref_raster))
  extent(r_subp) <- extent(ref_raster)
  projection(r_subp) <- crs_correct
  
  writeRaster(r_subp, "TIF_AIYL_SUBP_ind/AIYL_subp_ind.tif", overwrite = TRUE)
}



###Violin plots AIYL, toutes cultures confondues
if (1 == 1) {
## Créer dataframes AIYL pour chaque scénario
mat_dep_stromb_agri <- mat_dep_stromb[mat_dep_stromb$lu %in% c(100,92,73), ]
AIYL_df_stromb <- data.frame(
  AIYL = mat_dep_stromb_agri$AIYL_stromb,
  Scenario = "Strombolian"
)
mat_dep_subp_agri <- mat_dep_subp[mat_dep_subp$lu %in% c(100,92,73), ]
AIYL_df_subp <- data.frame(
  AIYL = mat_dep_subp_agri$AIYL_subp,
  Scenario = "Sub-plinian"
)

## Combiner dans un seul dataframe
AIYL_combined <- bind_rows(AIYL_df_stromb, AIYL_df_subp)

## Violin plot avec légende et couleurs diff par scénario
ggplot(AIYL_combined, aes(x = Scenario, y = AIYL, fill = Scenario)) +
  geom_violin(alpha = 0.5, scale = "width", color = "black") +
  scale_fill_manual(values = c("Strombolian" = "orange", "Sub-plinian" = "red")) +
  labs(x = "", y = "Annualized Index of Yield Loss (AIYL, %)") +
  theme_minimal()


## Violin plot sans légende et mm couleurs pour 2 scénario
ggplot(AIYL_combined, aes(x = Scenario, y = AIYL, fill = Scenario)) +
  geom_violin(alpha = 0.5, scale = "width", color = "black") +
  scale_fill_manual(values = c("Strombolian" = "grey30", "Sub-plinian" = "grey30")) +
  labs(x = "", y = "Annualized Index of Yield Loss (AIYL, %)") +
  theme_minimal() +
  theme(legend.position = "none")
}
###Violin plot AIYL, par culture
if (1 == 1) {
  # 🎯 Définir les couleurs par culture
  culture_colors <- c(
    "Olive trees" = "#808000",
    "Orchards" = "#1f78b4",
    "Vineyards" = "darkred"
  )
  
  # 🎯 Créer les sous-dataframes pour chaque culture et scénario
  # Strombolian
  AIYL_stromb_cult <- mat_dep_stromb %>%
    filter(lu %in% c(100, 92, 73)) %>%
    mutate(Crop = case_when(
      lu == 100 ~ "Olive trees",
      lu == 92 ~ "Orchards",
      lu == 73 ~ "Vineyards"
    )) %>%
    select(AIYL = AIYL_stromb, Crop)
  
  # Sub-plinian
  AIYL_subp_cult <- mat_dep_subp %>%
    filter(lu %in% c(100, 92, 73)) %>%
    mutate(Crop = case_when(
      lu == 100 ~ "Olive trees",
      lu == 92 ~ "Orchards",
      lu == 73 ~ "Vineyards"
    )) %>%
    select(AIYL = AIYL_subp, Crop)
  
  # 📈 Violin plot Strombolian
  ggplot(AIYL_stromb_cult, aes(x = Crop, y = AIYL, fill = Crop)) +
    geom_violin(alpha = 0.5, scale = "width", color = "black") +
    scale_fill_manual(values = culture_colors) +
    labs(title = "Strombolian eruption – AIYL by Crop",
         x = "", y = "Annualized Index of Yield Loss (AIYL, %)") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 11),
      axis.title.y = element_text(size = 12)
    )
  
  # 📈 Violin plot Sub-plinian
  ggplot(AIYL_subp_cult, aes(x = Crop, y = AIYL, fill = Crop)) +
    geom_violin(alpha = 0.5, scale = "width", color = "black") +
    scale_fill_manual(values = culture_colors) +
    labs(title = "Sub-plinian eruption – AIYL by Crop",
         x = "", y = "Annualized Index of Yield Loss (AIYL, %)") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 11),
      axis.title.y = element_text(size = 12)
    )
}
### Seasonal Index of Yield Loss
if (1 == 1) {
  #sub-plinienne
cols_spring_subp <- grep("^yl_p(6|7|8|9|10|11)$", names(mat_dep_subp), value = TRUE)
cols_summer_subp <- grep("^yl_p(12|13|14|15|16|17)$", names(mat_dep_subp), value = TRUE)
cols_autumn_subp <- grep("^yl_p(18|19|20|21|22|23)$", names(mat_dep_subp), value = TRUE)
cols_winter_subp <- grep("^yl_p(1|2|3|4|5|24)$", names(mat_dep_subp), value = TRUE)

mat_dep_subp$SIYL_spring_subp <- rowSums(mat_dep_subp[, cols_spring_subp], na.rm = TRUE) / 6
mat_dep_subp$SIYL_summer_subp <- rowSums(mat_dep_subp[, cols_summer_subp], na.rm = TRUE) / 6
mat_dep_subp$SIYL_autumn_subp <- rowSums(mat_dep_subp[, cols_autumn_subp], na.rm = TRUE) / 6
mat_dep_subp$SIYL_winter_subp <- rowSums(mat_dep_subp[, cols_winter_subp], na.rm = TRUE) / 6

  #strombolienne
cols_spring_stromb <- grep("^yl_p(6|7|8|9|10|11)$", names(mat_dep_stromb), value = TRUE)
cols_summer_stromb <- grep("^yl_p(12|13|14|15|16|17)$", names(mat_dep_stromb), value = TRUE)
cols_autumn_stromb <- grep("^yl_p(18|19|20|21|22|23)$", names(mat_dep_stromb), value = TRUE)
cols_winter_stromb <- grep("^yl_p(1|2|3|4|5|24)$", names(mat_dep_stromb), value = TRUE)

mat_dep_stromb$SIYL_spring_stromb <- rowSums(mat_dep_stromb[, cols_spring_stromb], na.rm = TRUE) / 6
mat_dep_stromb$SIYL_summer_stromb <- rowSums(mat_dep_stromb[, cols_summer_stromb], na.rm = TRUE) / 6
mat_dep_stromb$SIYL_autumn_stromb <- rowSums(mat_dep_stromb[, cols_autumn_stromb], na.rm = TRUE) / 6
mat_dep_stromb$SIYL_winter_stromb <- rowSums(mat_dep_stromb[, cols_winter_stromb], na.rm = TRUE) / 6
}
###Violin plots SIYL, toutes cultures confondues
if (1 == 1) {
#Sub-plinienne
mat_dep_subp_agri <- mat_dep_subp[mat_dep_subp$lu %in% c(100,92,73), ]
df_subp <- data.frame(
  SIYL = c(mat_dep_subp_agri$SIYL_spring_subp,
           mat_dep_subp_agri$SIYL_summer_subp,
           mat_dep_subp_agri$SIYL_autumn_subp,
           mat_dep_subp_agri$SIYL_winter_subp),
  Season = factor(rep(c("Spring", "Summer", "Autumn", "Winter"),
                      each = nrow(mat_dep_subp_agri)),
                  levels = c("Winter", "Spring", "Summer", "Autumn")),
  Scenario = "Sub-plinian"
)
#Strombolienne
mat_dep_stromb_agri <- mat_dep_stromb[mat_dep_stromb$lu %in% c(100,92,73), ]
df_stromb <- data.frame(
  SIYL = c(mat_dep_stromb_agri$SIYL_spring_stromb,
           mat_dep_stromb_agri$SIYL_summer_stromb,
           mat_dep_stromb_agri$SIYL_autumn_stromb,
           mat_dep_stromb_agri$SIYL_winter_stromb),
  Season = factor(rep(c("Spring", "Summer", "Autumn", "Winter"),
                      each = nrow(mat_dep_stromb_agri)),
                  levels = c("Winter", "Spring", "Summer", "Autumn")),
  Scenario = "Strombolian"
)
# Fusionner en un seul dataframe
SIYL_long_agri <- bind_rows(df_stromb, df_subp)


ggplot(SIYL_long_agri, aes(x = Season, y = SIYL, fill = Season)) +
  geom_violin(alpha = 0.5, color = "black", scale = "width") +
  scale_fill_manual(values = c("Winter" = "lightblue",
                               "Spring" = "lightgreen",
                               "Summer" = "khaki",
                               "Autumn" = "peachpuff")) +
  facet_wrap(~ Scenario, ncol = 1) +
  labs(x = "", y = "Seasonal Index of Yield Loss (SIYL, %)",
       fill = "Season") +
  theme_minimal()
}
###Calcul des moyennes de pertes le long des périodes, toutes cultures confondues
if (1 == 1) {
library(ggplot2)
library(dplyr)

# 📌 Définir les saisons (comme dans tout ton script)
seasons_df <- data.frame(
  season = c("Winter", "Spring", "Summer", "Autumn", "Winter"),
  start = c(1, 6, 12, 18, 24),
  end   = c(5, 11, 17, 23, 24.5),
  fill  = c("lightblue", "lightgreen", "khaki", "peachpuff", "lightblue")
)

# 📌 Fonction pour extraire moyennes et écarts-types à partir de yl_p1 à yl_p24
get_yl_stats_simple <- function(mat, scenario_label) {
  # Garder uniquement les pixels agricoles
  mat <- mat[mat$lu %in% c(100, 92, 73), ]
  
  cols <- grep("^yl_p\\d+$", names(mat), value = TRUE)
  periods <- as.integer(gsub("yl_p", "", cols))
  df <- mat[, cols]
  df[df == -9999] <- NA
  
  data.frame(
    period = periods,
    mean = colMeans(df, na.rm = TRUE),
    sd = apply(df, 2, sd, na.rm = TRUE),
    scenario = scenario_label
  )
}

# 📊 Calcul des stats pour les deux scénarios
stats_stromb <- get_yl_stats_simple(mat_dep_stromb, "Strombolian")
stats_subp <- get_yl_stats_simple(mat_dep_subp, "Sub-plinian")
}
###Graph moyennes de pertes le long des périodes, toutes cultures confondues
if (1 == 1) {
# Fonction de plot
plot_yl_mean_simple <- function(data_stats, title) {
  
  # S'assurer que l’ordre des saisons est conservé dans la légende
  seasons_df$season <- factor(seasons_df$season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  
  ggplot(data_stats, aes(x = period, y = mean)) +
    # bandes de saison avec légende activée
    geom_rect(data = seasons_df, inherit.aes = FALSE,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = season),
              alpha = 0.2,
              show.legend = TRUE) +
    # bande d'écart-type
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), fill = "grey50", alpha = 0.3) +
    # courbe de moyenne
    geom_line(linewidth = 1.2) +
    # abscisses : périodes
    scale_x_continuous(breaks = 1:24) +
    # couleurs des saisons
    scale_fill_manual(values = setNames(seasons_df$fill, seasons_df$season)) +
    # limiter l’axe des Y à 100 %
    coord_cartesian(ylim = c(0, 100)) +
    # titres
    labs(
      title = title,
      x = "Period (1–24)",
      y = "Mean yield loss (%)",
      fill = "Season"
    ) +
    # thème graphique
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
}



# 📊 Tracer les deux graphiques
plot_yl_mean_simple(stats_stromb, "Strombolian eruption – Mean Yield Loss per Period")
plot_yl_mean_simple(stats_subp, "Sub-plinian eruption – Mean Yield Loss per Period")
}
###Tableau des moyennes et tableau des écarts types (faire tourner graph des moyennes avant)
if (1 == 1) {
  #Moyennes
  # Strombolian
  table_means_stromb_1000 <- stats_stromb |>
    select(period, mean) |>
    tidyr::pivot_wider(names_from = period, values_from = mean, names_prefix = "p")
  print(table_means_stromb_1000)
  
  # Sub-plinian
  table_means_subp_1000 <- stats_subp |>
    select(period, mean) |>
    tidyr::pivot_wider(names_from = period, values_from = mean, names_prefix = "p")
  print(table_means_subp_1000)
  #Ecart-types
  # Strombolian
  table_sds_stromb_1000 <- stats_stromb |>
    select(period, sd) |>
    tidyr::pivot_wider(names_from = period, values_from = sd, names_prefix = "p")
  print(table_sds_stromb_1000)
  # Sub-plinian
  table_sds_subp_1000 <- stats_subp |>
    select(period, sd) |>
    tidyr::pivot_wider(names_from = period, values_from = sd, names_prefix = "p")
  print(table_sds_subp_1000)
  #Export en excel
  library(openxlsx)
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "Mean - Strombolian 1000")
  writeData(wb, "Mean - Strombolian 1000", table_means_stromb_1000)
  
  addWorksheet(wb, "SD - Strombolian 1000")
  writeData(wb, "SD - Strombolian 1000", table_sds_stromb_1000)
  
  addWorksheet(wb, "Mean - Subplinian 1000")
  writeData(wb, "Mean - Subplinian 1000", table_means_subp_1000)
  
  addWorksheet(wb, "SD - Subplinian 1000")
  writeData(wb, "SD - Subplinian 1000", table_sds_subp_1000)
  
  saveWorkbook(wb, "table_yl_mean_sd_all_MODELE_1000simulations.xlsx", overwrite = TRUE)
}
###Calcul des pertes le long des périodes par culture
if (1 == 1) {
get_yl_stats_by_culture <- function(mat, scenario_label) {
  # Filtrer uniquement les pixels agricoles pertinents
  mat <- mat[mat$lu %in% c(100, 92, 73), ]
  
  # Extraire les colonnes yl_p1 à yl_p24
  cols <- grep("^yl_p\\d+$", names(mat), value = TRUE)
  periods <- as.integer(gsub("yl_p", "", cols))
  
  # Cultures à analyser
  cultures <- c("Olive trees" = 100, "Orchards" = 92, "Vineyards" = 73)
  
  # Initialiser un tableau vide
  all_stats <- data.frame()
  
  for (cult in names(cultures)) {
    submat <- mat[mat$lu == cultures[[cult]], cols]
    submat[submat == -9999] <- NA
    
    df <- data.frame(
      period = periods,
      mean = colMeans(submat, na.rm = TRUE),
      sd = apply(submat, 2, sd, na.rm = TRUE),
      scenario = scenario_label,
      culture = cult
    )
    
    all_stats <- rbind(all_stats, df)
  }
  
  return(all_stats)
}
}
###Graph moyennes de pertes le long des périodes par culture
if (1 == 1) {
plot_yl_by_culture <- function(data_stats, title) {
  
  # S'assurer que l’ordre des saisons est conservé
  seasons_df$season <- factor(seasons_df$season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  
  ggplot(data_stats, aes(x = period, y = mean, color = culture, fill = culture)) +
    # Bandes saisonnières
    geom_rect(data = seasons_df, inherit.aes = FALSE,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = season),
              alpha = 0.2,
              show.legend = FALSE) +

    # Courbes moyennes par culture
    geom_line(linewidth = 1.2) +
    scale_x_continuous(breaks = 1:24) +
    coord_cartesian(ylim = c(0, 100)) +
    
    # Couleurs 
    scale_color_manual(values = c(
      "Olive trees" = 	"#808000",
      "Orchards" = "#1f78b4",
      "Vineyards" = "darkred"
    )) +
    
    scale_fill_manual(values = c("Winter" = "lightblue", "Spring" = "lightgreen",
                                 "Summer" = "khaki", "Autumn" = "peachpuff")) +
    labs(
      title = title,
      x = "Period (1–24)",
      y = "Mean yield loss (%)",
      color = "Crop type",
      fill = "Crop type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
}
# Calcul des stats
stats_stromb_by_cult <- get_yl_stats_by_culture(mat_dep_stromb, "Strombolian")
stats_subp_by_cult   <- get_yl_stats_by_culture(mat_dep_subp, "Sub-plinian")

# Tracés
plot_yl_by_culture(stats_stromb_by_cult, "Strombolian eruption – Mean Yield Loss per Period by Crop")
plot_yl_by_culture(stats_subp_by_cult, "Sub-plinian eruption – Mean Yield Loss per Period by Crop")
}
###Tableau des moyennes et écart-type de pertes le long des périodes par culture
if (1 == 1) {
  library(dplyr)
  library(tidyr)
  library(openxlsx)
  
  ### Moyennes
  
  # Strombolian
  table_means_stromb_by_crop <- stats_stromb_by_cult |>
    select(period, culture, mean) |>
    pivot_wider(names_from = culture, values_from = mean)
  
  # Sub-plinian
  table_means_subp_by_crop <- stats_subp_by_cult |>
    select(period, culture, mean) |>
    pivot_wider(names_from = culture, values_from = mean)
  
  ### Écarts-types
  
  # Strombolian
  table_sds_stromb_by_crop <- stats_stromb_by_cult |>
    select(period, culture, sd) |>
    pivot_wider(names_from = culture, values_from = sd)
  
  # Sub-plinian
  table_sds_subp_by_crop <- stats_subp_by_cult |>
    select(period, culture, sd) |>
    pivot_wider(names_from = culture, values_from = sd)
  
  ### Export vers Excel
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "Mean by crop - Strombolian")
  writeData(wb, "Mean by crop - Strombolian", table_means_stromb_by_crop)
  
  addWorksheet(wb, "SD by crop - Strombolian")
  writeData(wb, "SD by crop - Strombolian", table_sds_stromb_by_crop)
  
  addWorksheet(wb, "Mean by crop - Sub-plinian")
  writeData(wb, "Mean by crop - Sub-plinian", table_means_subp_by_crop)
  
  addWorksheet(wb, "SD by crop - Sub-plinian")
  writeData(wb, "SD by crop - Sub-plinian", table_sds_subp_by_crop)
  
  saveWorkbook(wb, "table_yl_mean_sd_by_crop_MODELE_1000.xlsx", overwrite = TRUE)
}
###CDF plot
if (1 == 1) {
#CDF plot, toutes cultures confondues
AIYL_stromb <- mat_dep_stromb_agri %>%
  select(AIYL = AIYL_stromb) %>%
  mutate(Scenario = "Strombolian")

AIYL_subp <- mat_dep_subp_agri %>%
  select(AIYL = AIYL_subp) %>%
  mutate(Scenario = "Sub-plinian")

AIYL_combined <- bind_rows(AIYL_stromb, AIYL_subp)

AIYL_cdf_combined <- AIYL_combined %>%
  group_by(Scenario) %>%
  do({
    xvals <- sort(.$AIYL)
    data.frame(
      AIYL = xvals,
      Exceedance = 1 - ecdf(.$AIYL)(xvals)
    )
  })

ggplot(AIYL_cdf_combined, aes(x = AIYL, y = Exceedance, color = Scenario)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Annualized Index of Yield Loss (AIYL, %)",
       y = "Probability of exceeding") +
  theme_minimal()

#CDF plot par culture
AIYL_long_stromb <- mat_dep_stromb_agri %>%
  select(lu, AIYL = AIYL_stromb) %>%
  mutate(Culture = case_when(
    lu == 100 ~ "Olive trees",
    lu == 92 ~ "Orchards",
    lu == 73 ~ "Vineyards"
  ),
  Scenario = "Strombolian") %>%
  filter(!is.na(Culture))

AIYL_long_subp <- mat_dep_subp_agri %>%
  select(lu, AIYL = AIYL_subp) %>%
  mutate(Culture = case_when(
    lu == 100 ~ "Olive trees",
    lu == 92 ~ "Orchards",
    lu == 73 ~ "Vineyards"
  ),
  Scenario = "Sub-plinian") %>%
  filter(!is.na(Culture))

AIYL_long_combined <- bind_rows(AIYL_long_stromb, AIYL_long_subp)

AIYL_cdf_culture <- AIYL_long_combined %>%
  group_by(Scenario, Culture) %>%
  do({
    xvals <- sort(.$AIYL)
    data.frame(
      AIYL = xvals,
      Exceedance = 1 - ecdf(.$AIYL)(xvals)
    )
  })

ggplot(AIYL_cdf_culture, aes(x = AIYL, y = Exceedance, color = Scenario)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Annualized Index of Yield Loss (AIYL, %)",
       y = "Probability of exceeding") +
  facet_wrap(~ Culture) +
  theme_minimal()
}
###Seasonalized Index of Yield Loss et probabilité de dépasser celui-ci
if (1 == 1) {
#Calcul du SIYL par saison
  #Strombolienne
cols_printemps <- grep("^yl_p(6|7|8|9|10|11)$", names(mat_dep_stromb), value = TRUE)
cols_ete       <- grep("^yl_p(12|13|14|15|16|17)$", names(mat_dep_stromb), value = TRUE)
cols_automne   <- grep("^yl_p(18|19|20|21|22|23)$", names(mat_dep_stromb), value = TRUE)
cols_hiver     <- grep("^yl_p(1|2|3|4|5|24)$", names(mat_dep_stromb), value = TRUE)

mat_dep_stromb$SIYL_spring <- rowSums(mat_dep_stromb[, cols_printemps], na.rm = TRUE) / 6
mat_dep_stromb$SIYL_summer <- rowSums(mat_dep_stromb[, cols_ete], na.rm = TRUE) / 6
mat_dep_stromb$SIYL_autumn <- rowSums(mat_dep_stromb[, cols_automne], na.rm = TRUE) / 6
mat_dep_stromb$SIYL_winter <- rowSums(mat_dep_stromb[, cols_hiver], na.rm = TRUE) / 6
  #Pour la sub-plinienne
cols_printemps <- grep("^yl_p(6|7|8|9|10|11)$", names(mat_dep_subp), value = TRUE)
cols_ete       <- grep("^yl_p(12|13|14|15|16|17)$", names(mat_dep_subp), value = TRUE)
cols_automne   <- grep("^yl_p(18|19|20|21|22|23)$", names(mat_dep_subp), value = TRUE)
cols_hiver     <- grep("^yl_p(1|2|3|4|5|24)$", names(mat_dep_subp), value = TRUE)

mat_dep_subp$SIYL_spring <- rowSums(mat_dep_subp[, cols_printemps], na.rm = TRUE) / 6
mat_dep_subp$SIYL_summer <- rowSums(mat_dep_subp[, cols_ete], na.rm = TRUE) / 6
mat_dep_subp$SIYL_autumn <- rowSums(mat_dep_subp[, cols_automne], na.rm = TRUE) / 6
mat_dep_subp$SIYL_winter <- rowSums(mat_dep_subp[, cols_hiver], na.rm = TRUE) / 6

# Filtrer que pixels agricoles
mat_dep_stromb_agri <- mat_dep_stromb[mat_dep_stromb$lu %in% c(100,92,73), ]
mat_dep_subp_agri   <- mat_dep_subp[mat_dep_subp$lu %in% c(100,92,73), ]

# Créer dataframes longs
SIYL_season_stromb <- data.frame(
  SIYL = c(mat_dep_stromb_agri$SIYL_spring,
           mat_dep_stromb_agri$SIYL_summer,
           mat_dep_stromb_agri$SIYL_autumn,
           mat_dep_stromb_agri$SIYL_winter),
  Season = factor(rep(c("Spring", "Summer", "Autumn", "Winter"),
                      each = nrow(mat_dep_stromb_agri)),
                  levels = c("Spring", "Summer", "Autumn", "Winter")),
  Scenario = "Strombolian"
)

SIYL_season_subp <- data.frame(
  SIYL = c(mat_dep_subp_agri$SIYL_spring,
           mat_dep_subp_agri$SIYL_summer,
           mat_dep_subp_agri$SIYL_autumn,
           mat_dep_subp_agri$SIYL_winter),
  Season = factor(rep(c("Spring", "Summer", "Autumn", "Winter"),
                      each = nrow(mat_dep_subp_agri)),
                  levels = c("Spring", "Summer", "Autumn", "Winter")),
  Scenario = "Sub-plinian"
)

# Fusionner
SIYL_season_combined <- bind_rows(SIYL_season_stromb, SIYL_season_subp)

# === Calcul explicite de la complementary CDF
SIYL_cdf_combined <- SIYL_season_combined %>%
  group_by(Season, Scenario) %>%
  do({
    xvals <- sort(.$SIYL)
    data.frame(
      SIYL = xvals,
      Exceedance = 1 - ecdf(.$SIYL)(xvals)
    )
  })

# Graph final
ggplot(SIYL_cdf_combined, aes(x = SIYL, y = Exceedance, color = Season, linetype = Scenario)) +
  geom_line() +
  scale_color_manual(values = c("Spring" = "green",
                                "Summer" = "orange",
                                "Autumn" = "red",
                                "Winter" = "blue")) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1)) +
  labs(x = "Seasonal Index of Yield Loss (SIYL, %)",
       y = "Probability of exceeding",
       color = "Season",
       linetype = "Scenario") +
  theme_minimal()
}
