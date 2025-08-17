setwd("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R")
required_packages <- c(
  "readxl", "openxlsx", "gstat", "foreign", "geoR",
  "raster", "dplyr", "tidyr", "ggplot2", "reshape2", "ggridges"
)


# 🛠️ Fonction d’installation si manquant
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# 📥 Installer les packages manquants
invisible(sapply(required_packages, install_if_missing))
library(readxl)
library(gstat)
library(foreign)
library(geoR)
library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(ggridges)


install.packages("ggnewscale")
library(ggnewscale)

###DEBUT DU MODELE
##Conversion des fichiers txt en GeoTIFF pour etna_2002_1_8
if (1 == 1) {
# Dossiers d'entrée et de sortie
input_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/IM_strombolian_txt"
output_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/IM_strombolian_tif"
# Liste des noms de fichiers à traiter
IM_strombolian <- c("IM_all_10_%_strombolian.txt",
                    "IM_all_25_%_strombolian.txt",
                    "IM_all_50_%_strombolian.txt",
                    "IM_all_75_%_strombolian.txt",
                    "IM_all_90_%_strombolian.txt")
# Boucle
for (file_name in IM_strombolian) {
  file_path <- file.path(input_dir, file_name)
  
  if (file.exists(file_path)) {
    r <- raster(file_path)
    
    # Nettoyer le nom pour enlever % et .txt -> .tif
    tif_name <- gsub("_%", "", file_name)
    tif_name <- sub(".txt$", ".tif", tif_name)
    output_file <- file.path(output_dir, tif_name)
    
    writeRaster(r, output_file, format = "GTiff", overwrite = TRUE)
  } else {
    cat(sprintf("Fichier non trouvé : %s\n", file_path))
  }
}
}
##Conversion des fichiers txt en GeoTIFF pour sub_plinian
if (1 == 1) {
  # Dossiers d'entrée et de sortie
  input_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/IM_subplinian_txt"
  output_dir <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/IM_subplinian_tif"
  # Liste des noms de fichiers à traiter
  IM_subplinian <- c("IM_all_10_kgm2_subplinian.txt",
                      "IM_all_25_kgm2_subplinian.txt",
                      "IM_all_50_kgm2_subplinian.txt",
                      "IM_all_75_kgm2_subplinian.txt",
                      "IM_all_90_kgm2_subplinian.txt")
  # Boucle
  for (file_name in IM_subplinian) {
    file_path <- file.path(input_dir, file_name)
    
    if (file.exists(file_path)) {
      r <- raster(file_path)
      
      # Nettoyer le nom pour enlever % et .txt -> .tif
      tif_name <- gsub("_kgm2", "", file_name)
      tif_name <- sub(".txt$", ".tif", tif_name)
      output_file <- file.path(output_dir, tif_name)
      
      writeRaster(r, output_file, format = "GTiff", overwrite = TRUE)
    } else {
      cat(sprintf("Fichier non trouvé : %s\n", file_path))
    }
  }
}
##raster des IM des 2 scénarios éruptifs
if (1 == 1) {
i10_stromb <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/IM_strombolian_tif/IM_all_10_strombolian.tif")
i25_stromb <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/IM_strombolian_tif/IM_all_25_strombolian.tif")
i50_stromb <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/IM_strombolian_tif/IM_all_50_strombolian.tif")
i75_stromb <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/IM_strombolian_tif/IM_all_75_strombolian.tif")
i90_stromb <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/etna_2002_1_8/IM_strombolian_tif/IM_all_90_strombolian.tif")
i10_subp <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/IM_subplinian_tif/IM_all_10_subplinian.tif")
i25_subp <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/IM_subplinian_tif/IM_all_25_subplinian.tif")
i50_subp <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/IM_subplinian_tif/IM_all_50_subplinian.tif")
i75_subp <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/IM_subplinian_tif/IM_all_75_subplinian.tif")
i90_subp <- raster("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Tephraprob/sub_plinian_1/IM_subplinian_tif/IM_all_90_subplinian.tif")
}
##resample des IM des 2 scénarios éruptifs
if (1 == 1) {
lulc <- raster("lulc_current.tif")
i10_stromb_res <- resample(i10_stromb, lulc, method = "bilinear"); i10_stromb_res[i10_stromb_res < 0] <- 0
i25_stromb_res <- resample(i25_stromb, lulc, method = "bilinear"); i25_stromb_res[i25_stromb_res < 0] <- 0
i50_stromb_res <- resample(i50_stromb, lulc, method = "bilinear"); i50_stromb_res[i50_stromb_res < 0] <- 0
i75_stromb_res <- resample(i75_stromb, lulc, method = "bilinear"); i75_stromb_res[i75_stromb_res < 0] <- 0
i90_stromb_res <- resample(i90_stromb, lulc, method = "bilinear"); i90_stromb_res[i90_stromb_res < 0] <- 0

i10_subp_res <- resample(i10_subp, lulc, method = "bilinear"); i10_subp_res[i10_subp_res < 0] <- 0
i25_subp_res <- resample(i25_subp, lulc, method = "bilinear"); i25_subp_res[i25_subp_res < 0] <- 0
i50_subp_res <- resample(i50_subp, lulc, method = "bilinear"); i50_subp_res[i50_subp_res < 0] <- 0
i75_subp_res <- resample(i75_subp, lulc, method = "bilinear"); i75_subp_res[i75_subp_res < 0] <- 0
i90_subp_res <- resample(i90_subp, lulc, method = "bilinear"); i90_subp_res[i90_subp_res < 0] <- 0
}
##Création de mat_stromb
if (1 == 1) {
mat_stromb <- cbind(
  as.data.frame(lulc),
  as.data.frame(i10_stromb_res),
  as.data.frame(i25_stromb_res),
  as.data.frame(i50_stromb_res),
  as.data.frame(i75_stromb_res),
  as.data.frame(i90_stromb_res)
)
names(mat_stromb) <- c("lu", "i10_stromb", "i25_stromb", "i50_stromb", "i75_stromb", "i90_stromb")

#changer NA en 0
mat_stromb$lu[is.na(mat_stromb$lu)] <- 0

#grouper les olives
mat_stromb$lu[mat_stromb$lu == 101] <- 100; mat_stromb$lu[mat_stromb$lu == 110] <- 100
}
##Création de mat_subp
if (1 == 1) {
mat_subp <- cbind(
  as.data.frame(lulc),
  as.data.frame(i10_subp_res),
  as.data.frame(i25_subp_res),
  as.data.frame(i50_subp_res),
  as.data.frame(i75_subp_res),
  as.data.frame(i90_subp_res)
)
names(mat_subp) <- c("lu", "i10_subp", "i25_subp", "i50_subp", "i75_subp", "i90_subp")

#changer NA en 0
mat_subp$lu[is.na(mat_subp$lu)] <- 0

#grouper les olives
mat_subp$lu[mat_subp$lu == 101] <- 100; mat_subp$lu[mat_subp$lu == 110] <- 100
}
##Chargement des paramètres
if (1 == 1) {
parameters <- "C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/parametres.xlsx"
s <- data <- read_excel(parameters, sheet = "s")
eq <- data <- read_excel(parameters, sheet = "eq")

exp_model <- function(x, a, b) {
  losses <- rep(0, length(x))
  mask <- (x != 0) & (a != 0) & (b != 0)
  losses[mask] <- b * (1 - exp(-x[mask] / a))
  return(losses)
}

get_params_from_eq <- function(type_code, is_vigne = FALSE) {
  prefix <- ifelse(is_vigne, "v_", "ft_")
  type_code <- trimws(as.character(type_code))
  
  a_col <- paste0(prefix, type_code, "_a")
  b_col <- paste0(prefix, type_code, "_b")
  
  a <- eq[[a_col]]
  b <- eq[[b_col]]
  
  return(list(a = a, b = b))
}
quantiles <- c(10, 25, 50, 75, 90)
}
##Pertes pour éruption strombolienne
if (1 == 1) {
# Liste des couches de cendres (couches de mat à utiliser)
probs <- c(10, 25, 50, 75, 90)
# Boucle sur les couches de probabilité pour mat_stromb
for (prob in probs) {
  
  varname <- paste0("i", prob, "_stromb")
  
  for (p in 1:nrow(s)) {
    
    yl_temp <- rep(-9999, nrow(mat_stromb))
    
    # olives
    type_code <- s[p, "ol"][[1]]
    params <- get_params_from_eq(type_code, is_vigne = FALSE)
    yl_temp[mat_stromb$lu == 100] <- exp_model(mat_stromb[mat_stromb$lu == 100, varname], params$a, params$b)
    
    # vergers
    type_code <- s[p, "or"][[1]]
    params <- get_params_from_eq(type_code, is_vigne = FALSE)
    yl_temp[mat_stromb$lu == 92] <- exp_model(mat_stromb[mat_stromb$lu == 92, varname], params$a, params$b)
    
    # vignes
    type_code <- s[p, "v"][[1]]
    params <- get_params_from_eq(type_code, is_vigne = TRUE)
    yl_temp[mat_stromb$lu == 73] <- exp_model(mat_stromb[mat_stromb$lu == 73, varname], params$a, params$b)
    
    # Créer la colonne
    new_col_name <- paste0("yl", prob, "_p", p, "_stromb")
    mat_stromb[[new_col_name]] <- yl_temp
  }
}
}
##Pertes pour éruption subplinienne
if (1 == 1) {
# Boucle sur les couches de probabilité pour mat_subp
for (prob in probs) {
  
  varname <- paste0("i", prob, "_subp")
  
  for (p in 1:nrow(s)) {
    
    yl_temp <- rep(-9999, nrow(mat_subp))
    
    # olives
    type_code <- s[p, "ol"][[1]]
    params <- get_params_from_eq(type_code, is_vigne = FALSE)
    yl_temp[mat_subp$lu == 100] <- exp_model(mat_subp[mat_subp$lu == 100, varname], params$a, params$b)
    
    # vergers
    type_code <- s[p, "or"][[1]]
    params <- get_params_from_eq(type_code, is_vigne = FALSE)
    yl_temp[mat_subp$lu == 92] <- exp_model(mat_subp[mat_subp$lu == 92, varname], params$a, params$b)
    
    # vignes
    type_code <- s[p, "v"][[1]]
    params <- get_params_from_eq(type_code, is_vigne = TRUE)
    yl_temp[mat_subp$lu == 73] <- exp_model(mat_subp[mat_subp$lu == 73, varname], params$a, params$b)
    
    # Créer la colonne
    new_col_name <- paste0("yl", prob, "_p", p, "_subp")
    mat_subp[[new_col_name]] <- yl_temp
  }
}
}
##Exports en tiff
if (1 == 1) {
## Export des yl_stromb
cols_export_stromb <- names(mat_stromb)[grepl("^yl\\d+_p\\d+_stromb$", names(mat_stromb))]
unlink("Tiffrisk_stromb", recursive = TRUE)
dir.create("Tiffrisk_stromb")

for (col_name in cols_export_stromb) {
  r <- setValues(lulc, mat_stromb[[col_name]])
  r[r == -9999] <- NA
  writeRaster(r, filename = file.path("Tiffrisk_stromb", paste0(col_name, ".tif")),
              format = "GTiff", NAflag = -9999, overwrite = TRUE)
}

## Export des yl_subp
cols_export_subp <- names(mat_subp)[grepl("^yl\\d+_p\\d+_subp$", names(mat_subp))]
unlink("Tiffrisk_subp", recursive = TRUE)
dir.create("Tiffrisk_subp")

for (col_name in cols_export_subp) {
  r <- setValues(lulc, mat_subp[[col_name]])
  r[r == -9999] <- NA
  writeRaster(r, filename = file.path("Tiffrisk_subp", paste0(col_name, ".tif")),
              format = "GTiff", NAflag = -9999, overwrite = TRUE)
}
}
###FIN DU MODELE
##Chargement des matrices de pertes (trop long de refaire tourner le modèle)
if (1 == 1) {
lulc <- raster("lulc_current.tif")
xy <- coordinates(lulc)
lu <- getValues(lulc)
quantiles <- c(10, 25, 50, 75, 90)
  
# Sub-plinienne
mat_subp <- data.frame(x = xy[,1], y = xy[,2], lu = lu)
mat_subp$lu[is.na(mat_subp$lu)] <- 0
mat_subp$lu[mat_subp$lu %in% c(101,110)] <- 100

for (q in quantiles) {
  for (p in 1:24) {
    file_path <- sprintf("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/Tiffrisk_subp/yl%d_p%d_subp.tif", q, p)
    r <- raster(file_path)
    vals <- getValues(r)
    
    if (length(vals) != nrow(mat_subp)) {
      stop(sprintf("Erreur : longueur mismatch yl%d_p%d_subp.tif (%d vs %d)",
                   q, p, length(vals), nrow(mat_subp)))
    }
    
    col_name <- sprintf("yl%d_p%d_subp", q, p)
    mat_subp[[col_name]] <- vals
  }
}

mat_subp_agri   <- mat_subp[mat_subp$lu %in% c(100,92,73), ]
# Strombolienne
mat_stromb <- data.frame(x = xy[,1], y = xy[,2], lu = lu)
mat_stromb$lu[is.na(mat_stromb$lu)] <- 0
mat_stromb$lu[mat_stromb$lu %in% c(101,110)] <- 100

for (q in quantiles) {
  for (p in 1:24) {
    file_path <- sprintf("C:/Users/zoesa/OneDrive - UCL/M2/Q2/Mémoire/Modèle R/Tiffrisk_stromb/yl%d_p%d_stromb.tif", q, p)
    r <- raster(file_path)
    vals <- getValues(r)
    
    # Vérifie la taille
    if (length(vals) != nrow(mat_stromb)) {
      stop(sprintf("Erreur : longueur mismatch yl%d_p%d_stromb.tif (%d vs %d)",
                   q, p, length(vals), nrow(mat_stromb)))
    }
    
    # Nom de colonne cohérent
    col_name <- sprintf("yl%d_p%d_stromb", q, p)
    mat_stromb[[col_name]] <- vals
  }
}
mat_stromb_agri <- mat_stromb[mat_stromb$lu %in% c(100,92,73), ]
}
##Graph des moyennes de pertes, toutes cultures confondues --> graph à améliorer
if (1 == 1) {
# Fonction pour extraire les stats (moyenne et écart-type) pour les quantiles d’un modèle IM
get_yl_im_stats <- function(mat, suffix_label, scenario_label) {
  lapply(c(10, 50, 90), function(p) {
    # 🔍 Extraire les colonnes de la probabilité donnée
    cols <- grep(paste0("^yl", p, "_p\\d+_", suffix_label, "$"), names(mat), value = TRUE)
    df <- mat[, cols]
    df[df == -9999] <- NA
    means <- colMeans(df, na.rm = TRUE)
    sds <- apply(df, 2, sd, na.rm = TRUE)
    periods <- as.integer(gsub(paste0(".*_p(\\d+)_.*"), "\\1", cols))
    data.frame(
      period = periods,
      mean = means,
      sd = sds,
      prob = paste0(p, "%"),
      scenario = scenario_label
    )
  }) |> bind_rows()
}

# ✅ Appliquer à chaque scénario
stats_stromb_im <- get_yl_im_stats(mat_stromb_agri, "stromb", "Strombolian IM")
stats_subp_im   <- get_yl_im_stats(mat_subp_agri,   "subp",   "Sub-plinian IM")

#Fonction du graph
# 📌 Définir les saisons (comme dans tout ton script)
seasons_df <- data.frame(
  season = c("Winter", "Spring", "Summer", "Autumn", "Winter"),
  start = c(1, 6, 12, 18, 24),
  end   = c(5, 11, 17, 23, 24.5),
  fill  = c("lightblue", "lightgreen", "khaki", "peachpuff", "lightblue")
)


plot_yl_mean_quantiles <- function(data_stats, title) {
  
  # S'assurer que l’ordre des saisons est conservé
  seasons_df$season <- factor(seasons_df$season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  
  # Couleurs des quantiles
  quantile_colors <- c("10%" = "#F8766D", "50%" = "#FDB863", "90%" = "#FFFFB3")
  
  # Couleurs des saisons
  season_colors <- setNames(seasons_df$fill, seasons_df$season)
  
  ggplot(data_stats, aes(x = period)) +
    
    # 🟦 Bandes de saison (avec fill_season)
    geom_rect(data = seasons_df, inherit.aes = FALSE,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = season),
              alpha = 0.2) +
    scale_fill_manual(name = "Season", values = season_colors) +
    
    # 🔄 Nouvelle échelle pour fill (évite conflit avec seasons)
    new_scale_fill() +
    new_scale_color() +
    
    # 📊 Rubans d’écart-type (quantiles)
    geom_ribbon(data = data_stats,
                aes(ymin = mean - sd, ymax = mean + sd, fill = prob, group = prob),
                alpha = 0.3, color = NA) +
    
    # 📈 Courbes des moyennes
    geom_line(data = data_stats, aes(y = mean, color = prob, group = prob), linewidth = 1.2) +
    
    # 🎨 Palettes personnalisées
    scale_fill_manual(name = "Quantile (ashfall exceedance)", values = quantile_colors) +
    scale_color_manual(name = "Quantile (ashfall exceedance)", values = quantile_colors) +
    
    # Axes et titres
    scale_x_continuous(breaks = 1:24) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(
      title = title,
      x = "Period (1–24)",
      y = "Mean yield loss (%)"
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
}

#Tracer
plot_yl_mean_quantiles(stats_stromb_im, "Strombolian eruption – Mean Yield Loss per Period")
plot_yl_mean_quantiles(stats_subp_im, "Sub-plinian eruption – Mean Yield Loss per Period")
}
##Tableau des moyennes et tableau des écarts types (faire tourner graph des moyennes avant)
if (1 == 1) {
#Moyenne
#Strombolian
table_means_stromb <- stats_stromb_im |>
  select(prob, period, mean) |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = mean,
    names_prefix = "p"
  )
print(table_means_stromb)

#Subplinian
table_means_subp <- stats_subp_im |>
  select(prob, period, mean) |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = mean,
    names_prefix = "p"
  )
print(table_means_subp)

#écarts types
table_sds_subp <- stats_subp_im |>
  select(prob, period, sd) |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = sd,
    names_prefix = "p"
  )
print(table_sds_subp)

table_sds_stromb <- stats_stromb_im |>
  select(prob, period, sd) |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = sd,
    names_prefix = "p"
  )
print(table_sds_stromb)

#Sauvegarde Excel
library(openxlsx)

# Créer un nouveau workbook
wb <- createWorkbook()

# Ajouter chaque feuille avec son tableau
addWorksheet(wb, "Mean - Strombolian")
writeData(wb, "Mean - Strombolian", table_means_stromb)

addWorksheet(wb, "SD - Strombolian")
writeData(wb, "SD - Strombolian", table_sds_stromb)

addWorksheet(wb, "Mean - Subplinian")
writeData(wb, "Mean - Subplinian", table_means_subp)

addWorksheet(wb, "SD - Subplinian")
writeData(wb, "SD - Subplinian", table_sds_subp)

# Sauvegarder le fichier
saveWorkbook(wb, file = "table_yl_mean_sd_all.xlsx", overwrite = TRUE)
}
###Calcul de l'écart entre les moyennes 
if (1 == 1) {
get_diff_table <- function(df_stats) {
  # Séparer les moyennes par quantile
  df_10 <- df_stats[df_stats$prob == "10%", c("period", "mean")]
  df_50 <- df_stats[df_stats$prob == "50%", c("period", "mean")]
  df_90 <- df_stats[df_stats$prob == "90%", c("period", "mean")]
  
  # Renommer les colonnes pour les identifier
  colnames(df_10)[2] <- "mean_10"
  colnames(df_50)[2] <- "mean_50"
  colnames(df_90)[2] <- "mean_90"
  
  # Faire les jointures manuellement
  df_joined <- merge(df_10, df_50, by = "period")
  df_joined <- merge(df_joined, df_90, by = "period")
  
  # Calcul des différences
  diff_10_50 <- df_joined$mean_50 - df_joined$mean_10
  diff_90_50 <- df_joined$mean_90 - df_joined$mean_50
  
  # Création du tableau final
  df_diff <- rbind(
    `50% - 10%` = diff_10_50,
    `90% - 50%` = diff_90_50
  )
  colnames(df_diff) <- paste0("p", df_joined$period)
  
  return(df_diff)
}
diff_table_subp <- get_diff_table(stats_subp_im)
diff_table_stromb <- get_diff_table(stats_stromb_im)

# Affichage
diff_table_subp


#EN POSITIF
get_diff_table <- function(df_stats) {
  # Extraire les lignes par quantile
  df_10 <- df_stats[df_stats$prob == "10%", c("period", "mean")]
  df_50 <- df_stats[df_stats$prob == "50%", c("period", "mean")]
  df_90 <- df_stats[df_stats$prob == "90%", c("period", "mean")]
  
  # Renommer les colonnes
  colnames(df_10)[2] <- "mean_10"
  colnames(df_50)[2] <- "mean_50"
  colnames(df_90)[2] <- "mean_90"
  
  # Fusionner
  df_joined <- merge(df_10, df_50, by = "period")
  df_joined <- merge(df_joined, df_90, by = "period")
  
  # Calcul des écarts absolus
  diff_10_50 <- df_joined$mean_10 - df_joined$mean_50
  diff_50_90 <- df_joined$mean_50 - df_joined$mean_90
  
  # Création du tableau final
  df_diff <- rbind(
    `10% - 50%` = abs(diff_10_50),
    `50% - 90%` = abs(diff_50_90)
  )
  colnames(df_diff) <- paste0("p", df_joined$period)
  
  return(df_diff)
}
diff_table_subp <- get_diff_table(stats_subp_im)
diff_table_stromb <- get_diff_table(stats_stromb_im)

diff_table_subp
}

##Annualized Index of Yield Loss
if (1 == 1) {
#Calcul du Annualized Index of Yield Loss pour chaque pixel et ajout à mat_stromb
  #Strombolienne
cols_yl10 <- grep("^yl10_p\\d+_stromb$", names(mat_stromb), value = TRUE)
mat_stromb$AIYL10_stromb <- rowSums(mat_stromb[, cols_yl10], na.rm = TRUE) / 24
cols_yl25 <- grep("^yl25_p\\d+_stromb$", names(mat_stromb), value = TRUE)
mat_stromb$AIYL25_stromb <- rowSums(mat_stromb[, cols_yl25], na.rm = TRUE) / 24
cols_yl50 <- grep("^yl50_p\\d+_stromb$", names(mat_stromb), value = TRUE)
mat_stromb$AIYL50_stromb <- rowSums(mat_stromb[, cols_yl50], na.rm = TRUE) / 24
cols_yl75 <- grep("^yl75_p\\d+_stromb$", names(mat_stromb), value = TRUE)
mat_stromb$AIYL75_stromb <- rowSums(mat_stromb[, cols_yl75], na.rm = TRUE) / 24
cols_yl90 <- grep("^yl90_p\\d+_stromb$", names(mat_stromb), value = TRUE)
mat_stromb$AIYL90_stromb <- rowSums(mat_stromb[, cols_yl90], na.rm = TRUE) / 24



  #Sub-plinienne
cols_yl10 <- grep("^yl10_p\\d+_subp$", names(mat_subp), value = TRUE)
mat_subp$AIYL10_subp <- rowSums(mat_subp[, cols_yl10], na.rm = TRUE) / 24
cols_yl25 <- grep("^yl25_p\\d+_subp$", names(mat_subp), value = TRUE)
mat_subp$AIYL25_subp <- rowSums(mat_subp[, cols_yl25], na.rm = TRUE) / 24
cols_yl50 <- grep("^yl50_p\\d+_subp$", names(mat_subp), value = TRUE)
mat_subp$AIYL50_subp <- rowSums(mat_subp[, cols_yl50], na.rm = TRUE) / 24
cols_yl75 <- grep("^yl75_p\\d+_subp$", names(mat_subp), value = TRUE)
mat_subp$AIYL75_subp <- rowSums(mat_subp[, cols_yl75], na.rm = TRUE) / 24
cols_yl90 <- grep("^yl90_p\\d+_subp$", names(mat_subp), value = TRUE)
mat_subp$AIYL90_subp <- rowSums(mat_subp[, cols_yl90], na.rm = TRUE) / 24

}
##Sauvegarde en geotiff du AIYL
if (1 == 1) {
ref_raster <- raster("lulc_current.tif")
crs_correct <- CRS("+init=EPSG:32633")
crs(ref_raster) <- crs_correct
dir.create("TIF_AIYL_STROMB", showWarnings = FALSE)
dir.create("TIF_AIYL_SUBP", showWarnings = FALSE)
quantiles <- c(10, 25, 50, 75, 90)
# --- Strombolien ---
for (q in quantiles) {
  var_name <- paste0("AIYL", q, "_stromb")
  vals <- rep(NA, nrow(mat_stromb))  # initialise tout à NA
  vals[mat_stromb$lu %in% c(100, 92, 73)] <- mat_stromb_agri[[var_name]]
  
  # Créer le raster
  r <- rasterFromXYZ(cbind(mat_stromb$x, mat_stromb$y, vals),
                     crs = crs_correct,
                     res = res(ref_raster))
  
  # Forcer alignement
  extent(r) <- extent(ref_raster)
  projection(r) <- crs_correct
  
  # Sauver
  filename <- paste0("TIF_AIYL_STROMB/AIYL_stromb_", q, ".tif")
  writeRaster(r, filename, overwrite = TRUE)
}

# --- Sub-plinien ---
for (q in quantiles) {
  var_name <- paste0("AIYL", q, "_subp")
  vals <- rep(NA, nrow(mat_subp))  # initialise tout à NA
  vals[mat_subp$lu %in% c(100, 92, 73)] <- mat_subp_agri[[var_name]]
  
  # Créer le raster
  r <- rasterFromXYZ(cbind(mat_subp$x, mat_subp$y, vals),
                     crs = crs_correct,
                     res = res(ref_raster))
  
  # Forcer alignement
  extent(r) <- extent(ref_raster)
  projection(r) <- crs_correct
  
  # Sauver
  filename <- paste0("TIF_AIYL_SUBP/AIYL_subp_", q, ".tif")
  writeRaster(r, filename, overwrite = TRUE)
}
}
##Violin plot AILY, toutes cultures confondues
if (1 == 1) {
  # Pixels agricoles
  mat_stromb_agri <- mat_stromb[mat_stromb$lu %in% c(100,92,73), ]
  mat_subp_agri   <- mat_subp[mat_subp$lu %in% c(100,92,73), ]
  
  # -------------------------------
  # STOCKAGE SCENARIO STROMBOLIENNE
  # -------------------------------
  list_dfs_stromb <- list()
  for (q in quantiles) {
    aiyl_col <- paste0("AIYL", q, "_stromb")
    df_q <- data.frame(
      AIYL = mat_stromb_agri[[aiyl_col]],
      Quantile = factor(q, levels = c(10,25,50,75,90)),
      Scenario = "Strombolian"
    )
    list_dfs_stromb[[as.character(q)]] <- df_q
  }
  df_stromb <- bind_rows(list_dfs_stromb)
  
  # -------------------------------
  # STOCKAGE SCENARIO SUB-PLINIENNE
  # -------------------------------
  list_dfs_subp <- list()
  for (q in quantiles) {
    aiyl_col <- paste0("AIYL", q, "_subp")
    df_q <- data.frame(
      AIYL = mat_subp_agri[[aiyl_col]],
      Quantile = factor(q, levels = c(10,25,50,75,90)),
      Scenario = "Sub-plinian"
    )
    list_dfs_subp[[as.character(q)]] <- df_q
  }
  df_subp <- bind_rows(list_dfs_subp)
  
  # -------------------------------
  # CONCAT FINAL
  # -------------------------------
  AIYL_combined <- bind_rows(df_stromb, df_subp)
  
  ggplot(AIYL_combined, aes(x = Quantile, y = AIYL, fill = Quantile)) +
    geom_violin(alpha = 0.4, color = "black", scale = "width", position = "identity") +
    scale_fill_manual(values = c("10" = "orange", 
                                 "25" = "gold", 
                                 "50" = "green", 
                                 "75" = "blue", 
                                 "90" = "purple")) +
    labs(x = "", y = "Annualized Index of Yield Loss (AIYL, %)",
         fill = "Ashfall quantile") +
    facet_wrap(~ Scenario, ncol = 2) +
    theme_minimal()
  
  
  
  ggplot(AIYL_combined, aes(x = Scenario, y = AIYL, fill = Quantile)) +
    geom_violin(alpha = 0.4, color = "black", scale = "width", position = "identity") +
    scale_fill_manual(values = c("10" = "orange",
                                 "25" = "gold",
                                 "50" = "green",
                                 "75" = "blue",
                                 "90" = "purple")) +
    labs(x = "", y = "Annualized Index of Yield Loss (AIYL, %)",
         fill = "Ashfall quantile") +
    facet_wrap(~ Scenario, ncol = 2, scales = "free_x") +
    theme_minimal()
  
  
}
##Violin plot AIYL STROMB toutes cultures confondues
if (1 == 1) {
# Sélection des pixels agricoles
mat_stromb_agri <- mat_stromb[mat_stromb$lu %in% c(100, 92, 73), ]

# Définir les quantiles
quantiles <- c(10, 25, 50, 75, 90)

# Palette de couleurs rouge foncé → jaune clair
quantile_colors <- c(
  "10" = "#B2182B",  # rouge foncé
  "25" = "#D6604D",
  "50" = "#F4A582",
  "75" = "#FDDBC7",
  "90" = "#FFFFBF"   # jaune clair
)

# Construction du dataframe pour Strombolian uniquement
list_dfs_stromb <- lapply(quantiles, function(q) {
  aiyl_col <- paste0("AIYL", q, "_stromb")
  data.frame(
    AIYL = mat_stromb_agri[[aiyl_col]],
    Quantile = factor(q, levels = quantiles),
    Scenario = "Strombolian"
  )
})
df_stromb <- dplyr::bind_rows(list_dfs_stromb)

# Violin plot (Strombolian uniquement, couleurs personnalisées)
ggplot(df_stromb, aes(x = Quantile, y = AIYL, fill = Quantile)) +
  geom_violin(
    alpha = 0.9,
    color = "black",
    scale = "width",
    position = "identity"
  ) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2, fill = "white", color = "black") +
  scale_fill_manual(values = quantile_colors) +
  labs(
    x = "Ashfall exceedance probability",
    y = "Annualized Index of Yield Loss (AIYL, %)",
    title = "Strombolian scenario – AIYL distribution by exceedance probability",
    fill = "Quantile"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "right"
  )

}
##Violin plot AIYL SUBP toutes cultures confondues
if (1 == 1) {
mat_subp_agri <- mat_subp[mat_subp$lu %in% c(100, 92, 73), ]

quantiles <- c(10, 25, 50, 75, 90)

quantile_colors <- c(
  "10" = "#B2182B",
  "25" = "#D6604D",
  "50" = "#F4A582",
  "75" = "#FDDBC7",
  "90" = "#FFFFBF"
)

list_dfs_subp <- lapply(quantiles, function(q) {
  aiyl_col <- paste0("AIYL", q, "_subp")
  data.frame(
    AIYL = mat_subp_agri[[aiyl_col]],
    Quantile = factor(q, levels = quantiles),
    Scenario = "Sub-plinian"
  )
})
df_subp <- dplyr::bind_rows(list_dfs_subp)

ggplot(df_subp, aes(x = Quantile, y = AIYL, fill = Quantile)) +
  geom_violin(
    alpha = 0.9,
    color = "black",
    scale = "width",
    position = "identity"
  ) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2, fill = "white", color = "black") +
  scale_fill_manual(values = quantile_colors) +
  labs(
    x = "Ashfall exceedance probability",
    y = "Annualized Index of Yield Loss (AIYL, %)",
    title = "Sub-plinian scenario – AIYL distribution by exceedance probability",
    fill = "Quantile"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "right"
  )
}
##Violin plot AIYL par culture
if (1 == 1) {
# ============================
# PREPARER LES DONNEES STROMBOLIENNE
# ============================
mat_stromb_agri <- mat_stromb[mat_stromb$lu %in% c(100, 92, 73), ]

AIYL_long_stromb <- melt(
  mat_stromb_agri[, c("lu", "AIYL10_stromb", "AIYL25_stromb", "AIYL50_stromb", "AIYL75_stromb", "AIYL90_stromb")],
  id.vars = "lu",
  variable.name = "Quantile",
  value.name = "AIYL"
)
AIYL_long_stromb$Quantile <- sub("AIYL([0-9]+)_stromb", "\\1", AIYL_long_stromb$Quantile)
AIYL_long_stromb$Quantile <- factor(AIYL_long_stromb$Quantile, levels = c("10", "25", "50", "75", "90"))
AIYL_long_stromb$Culture <- NA
AIYL_long_stromb$Culture[AIYL_long_stromb$lu == 100] <- "Olive trees"
AIYL_long_stromb$Culture[AIYL_long_stromb$lu == 92] <- "Orchards"
AIYL_long_stromb$Culture[AIYL_long_stromb$lu == 73] <- "Vineyards"
AIYL_long_stromb$Scenario <- "Strombolian"

# ============================
# PREPARER LES DONNEES SUB-PLINIENNE
# ============================
mat_subp_agri <- mat_subp[mat_subp$lu %in% c(100, 92, 73), ]

AIYL_long_subp <- melt(
  mat_subp_agri[, c("lu", "AIYL10_subp", "AIYL25_subp", "AIYL50_subp", "AIYL75_subp", "AIYL90_subp")],
  id.vars = "lu",
  variable.name = "Quantile",
  value.name = "AIYL"
)
AIYL_long_subp$Quantile <- sub("AIYL([0-9]+)_subp", "\\1", AIYL_long_subp$Quantile)
AIYL_long_subp$Quantile <- factor(AIYL_long_subp$Quantile, levels = c("10", "25", "50", "75", "90"))
AIYL_long_subp$Culture <- NA
AIYL_long_subp$Culture[AIYL_long_subp$lu == 100] <- "Olive trees"
AIYL_long_subp$Culture[AIYL_long_subp$lu == 92] <- "Orchards"
AIYL_long_subp$Culture[AIYL_long_subp$lu == 73] <- "Vineyards"
AIYL_long_subp$Scenario <- "Sub-plinian"

# ============================
# CONCATENER LES DONNEES
# ============================
AIYL_all <- bind_rows(AIYL_long_stromb, AIYL_long_subp)

# ============================
# VIOLIN PLOTS PAR CULTURE
# ============================
ggplot(AIYL_all, aes(x = Quantile, y = AIYL, fill = Quantile)) +
  geom_violin(scale = "width", trim = TRUE) +
  facet_grid(Scenario ~ Culture) +
  scale_fill_manual(values = c("orange", "khaki", "lightgreen", "lightblue", "orchid")) +
  labs(x = "Ashfall quantile",
       y = "Annualized Index of Yield Loss (AIYL, %)",
       fill = "Ashfall quantile") +
  theme_minimal()
}
##CDF plot du AIYL, toutes cultures confondues
if (1 == 1) {
  #Strombolienne
  #Empilement des AIYL par quantile
  mat_stromb_agri <- mat_stromb[mat_stromb$lu %in% c(100, 92, 73), ]
  AIYL_df_stromb <- data.frame(
    AIYL = c(mat_stromb_agri$AIYL10_stromb, mat_stromb_agri$AIYL25_stromb, 
             mat_stromb_agri$AIYL50_stromb, mat_stromb_agri$AIYL75_stromb, mat_stromb_agri$AIYL90_stromb),
    Quantile = factor(rep(c(10,25,50,75,90), each = nrow(mat_stromb_agri)))
  )
  
  AIYL_cdf_stromb <- AIYL_df_stromb %>%
    group_by(Quantile) %>%
    do({
      xvals <- sort(.$AIYL)
      data.frame(
        AIYL = xvals,
        Exceedance = 1 - ecdf(.$AIYL)(xvals)
      )
    })
  ggplot(AIYL_cdf_stromb, aes(x = AIYL, y = Exceedance, color = Quantile)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.10)) +
    labs(x = "Annualized Index of Yield Loss (AIYL, %)",
         y = "Probability of exceeding",
         color = "Exceedance Level (%)") +
    theme_minimal()
  
  #Sub-plinienne
  #Empilement par quantile
  mat_subp_agri <- mat_subp[mat_subp$lu %in% c(100, 92, 73), ]
  
  AIYL_df_subp <- data.frame(
    AIYL = c(mat_subp_agri$AIYL10_subp, mat_subp_agri$AIYL25_subp, 
             mat_subp_agri$AIYL50_subp, mat_subp_agri$AIYL75_subp, mat_subp_agri$AIYL90_subp),
    Quantile = factor(rep(c(10,25,50,75,90), each = nrow(mat_subp_agri)))
  )
  
  AIYL_cdf_subp <- AIYL_df_subp %>%
    group_by(Quantile) %>%
    do({
      xvals <- sort(.$AIYL)
      data.frame(
        AIYL = xvals,
        Exceedance = 1 - ecdf(.$AIYL)(xvals)
      )
    })
  ggplot(AIYL_cdf_subp, aes(x = AIYL, y = Exceedance, color = Quantile)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Annualized Index of Yield Loss (AIYL, %)",
       y = "Probability of exceeding",
       color = "Exceedance Level (%)") +
  theme_minimal()
 #Strombolienne et sub-plinienne sur le même graph
AIYL_cdf_stromb$Scenario <- "Strombolian"
AIYL_cdf_subp$Scenario <- "Sub-plinian"

cdf_all <- bind_rows(AIYL_cdf_stromb, AIYL_cdf_subp)

# Plot direct
ggplot(cdf_all, aes(x = AIYL, y = Exceedance, color = Quantile, linetype = Scenario)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1)) +
  scale_color_manual(values = c("10" = "red", "25" = "goldenrod", 
                                "50" = "green", "75" = "blue", "90" = "purple")) +
  labs(x = "Annualized Index of Yield Loss (AIYL, %)",
       y = "Probability of exceeding",
       color = "Exceedance Level (%)",
       linetype = "Eruption Scenario") +
  theme_minimal()
}
##CDF plot, par culture
if (1 == 1) {
  #Strombolienne
  cols_to_keep <- c("lu", "AIYL10_stromb", "AIYL25_stromb", "AIYL50_stromb", "AIYL75_stromb", "AIYL90_stromb")
  mat_stromb_agri_sub <- mat_stromb_agri[, cols_to_keep]
  
  AIYL_long_stromb <- melt(mat_stromb_agri_sub, id.vars = "lu", 
                           variable.name = "Quantile", value.name = "AIYL")
  
  AIYL_long_stromb$Quantile <- sub("AIYL([0-9]+)_stromb", "\\1", AIYL_long_stromb$Quantile)
  AIYL_long_stromb$Quantile <- factor(AIYL_long_stromb$Quantile, levels = c("10","25","50","75","90"))
  
  AIYL_long_stromb$Culture <- NA
  AIYL_long_stromb$Culture[AIYL_long_stromb$lu == 100] <- "Olive trees"
  AIYL_long_stromb$Culture[AIYL_long_stromb$lu == 92]  <- "Orchards"
  AIYL_long_stromb$Culture[AIYL_long_stromb$lu == 73]  <- "Vineyards"  
#Calcul du CDF stromb
  AIYL_cdf_stromb_culture <- AIYL_long_stromb %>%
  group_by(Quantile, Culture) %>%
  do({
    xvals <- sort(.$AIYL)
    data.frame(
      AIYL = xvals,
      Exceedance = 1 - ecdf(.$AIYL)(xvals)
    )
  })

ggplot(AIYL_cdf_stromb_culture, aes(x = AIYL, y = Exceedance, color = Quantile)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Annualized Index of Yield Loss (AIYL, %)",
       y = "Probability of exceeding",
       color = "Exceedance Level (%)") +
  facet_wrap(~ Culture) +
  theme_minimal()
 
 #Sub-plinienne
cols_to_keep_subp <- c("lu", "AIYL10_subp", "AIYL25_subp", "AIYL50_subp", "AIYL75_subp", "AIYL90_subp")
mat_subp_agri_sub <- mat_subp_agri[, cols_to_keep_subp]

AIYL_long_subp <- melt(mat_subp_agri_sub, id.vars = "lu", 
                       variable.name = "Quantile", value.name = "AIYL")

AIYL_long_subp$Quantile <- sub("AIYL([0-9]+)_subp", "\\1", AIYL_long_subp$Quantile)
AIYL_long_subp$Quantile <- factor(AIYL_long_subp$Quantile, levels = c("10","25","50","75","90"))

AIYL_long_subp$Culture <- NA
AIYL_long_subp$Culture[AIYL_long_subp$lu == 100] <- "Olive trees"
AIYL_long_subp$Culture[AIYL_long_subp$lu == 92]  <- "Orchards"
AIYL_long_subp$Culture[AIYL_long_subp$lu == 73]  <- "Vineyards"  

# Calculer la CDF sub-plinienne
AIYL_cdf_subp_culture <- AIYL_long_subp %>%
  group_by(Quantile, Culture) %>%
  do({
    xvals <- sort(.$AIYL)
    data.frame(
      AIYL = xvals,
      Exceedance = 1 - ecdf(.$AIYL)(xvals)
    )
  })
ggplot(AIYL_cdf_subp_culture, aes(x = AIYL, y = Exceedance, color = Quantile)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Annualized Index of Yield Loss (AIYL, %)",
       y = "Probability of exceeding",
       color = "Exceedance Level (%)") +
  facet_wrap(~ Culture) +
  theme_minimal()

  #Strombolienne et sub-plinienne
# Ajouter la colonne Scenario
AIYL_long_stromb$Scenario <- "Strombolian"
AIYL_long_subp$Scenario <- "Sub-plinian"

# Fusionner les deux dataframes long
AIYL_long_combined <- rbind(AIYL_long_stromb, AIYL_long_subp)

# Calculer la CDF groupée par Scenario, Quantile et Culture
AIYL_cdf_combined <- AIYL_long_combined %>%
  group_by(Scenario, Quantile, Culture) %>%
  do({
    xvals <- sort(.$AIYL)
    data.frame(
      AIYL = xvals,
      Exceedance = 1 - ecdf(.$AIYL)(xvals)
    )
  })

# Graph final
ggplot(AIYL_cdf_combined, aes(x = AIYL, y = Exceedance, color = Quantile, linetype = Scenario)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Annualized Index of Yield Loss (AIYL, %)",
       y = "Probability of exceeding",
       color = "Ashfall quantile",
       linetype = "Eruption scenario") +
  facet_wrap(~ Culture) +
  theme_minimal()

}


## Seasonal Index of Yield Loss stocké dans une dataframe SIYL_season_combined qui comprend SIYL, Season, Quantile et Scenario
if (1 == 1) {

# Pixels agricoles
mat_stromb_agri <- mat_stromb[mat_stromb$lu %in% c(100,92,73), ]
mat_subp_agri <- mat_subp[mat_subp$lu %in% c(100, 92, 73), ]
# -------------------------------
# STOCKAGE SCENARIO STROMBOLIENNE
# -------------------------------
list_dfs_stromb <- list()
for (q in quantiles) {
  
  cols_spring <- grep(paste0("^yl", q, "_p(6|7|8|9|10|11)_stromb$"), names(mat_stromb_agri), value = TRUE)
  cols_summer <- grep(paste0("^yl", q, "_p(12|13|14|15|16|17)_stromb$"), names(mat_stromb_agri), value = TRUE)
  cols_autumn <- grep(paste0("^yl", q, "_p(18|19|20|21|22|23)_stromb$"), names(mat_stromb_agri), value = TRUE)
  cols_winter <- grep(paste0("^yl", q, "_p(1|2|3|4|5|24)_stromb$"), names(mat_stromb_agri), value = TRUE)
  
  df_q <- data.frame(
    SIYL = c(rowSums(as.matrix(mat_stromb_agri[, cols_spring]), na.rm = TRUE) / 6,
             rowSums(as.matrix(mat_stromb_agri[, cols_summer]), na.rm = TRUE) / 6,
             rowSums(as.matrix(mat_stromb_agri[, cols_autumn]), na.rm = TRUE) / 6,
             rowSums(as.matrix(mat_stromb_agri[, cols_winter]), na.rm = TRUE) / 6),
    Season = factor(rep(c("Spring", "Summer", "Autumn", "Winter"),
                        each = nrow(mat_stromb_agri)),
                    levels = c("Winter", "Spring", "Summer", "Autumn")),
    Quantile = factor(q, levels = c(10,25,50,75,90)),
    Scenario = "Strombolian"
  )
  list_dfs_stromb[[as.character(q)]] <- df_q
}
df_stromb <- bind_rows(list_dfs_stromb)

# -------------------------------
# STOCKAGE SCENARIO SUB-PLINIENNE
# -------------------------------
list_dfs_subp <- list()
for (q in quantiles) {
  
  cols_spring <- grep(paste0("^yl", q, "_p(6|7|8|9|10|11)_subp$"), names(mat_subp_agri), value = TRUE)
  cols_summer <- grep(paste0("^yl", q, "_p(12|13|14|15|16|17)_subp$"), names(mat_subp_agri), value = TRUE)
  cols_autumn <- grep(paste0("^yl", q, "_p(18|19|20|21|22|23)_subp$"), names(mat_subp_agri), value = TRUE)
  cols_winter <- grep(paste0("^yl", q, "_p(1|2|3|4|5|24)_subp$"), names(mat_subp_agri), value = TRUE)
  
  df_q <- data.frame(
    SIYL = c(rowSums(as.matrix(mat_subp_agri[, cols_spring]), na.rm = TRUE) / 6,
             rowSums(as.matrix(mat_subp_agri[, cols_summer]), na.rm = TRUE) / 6,
             rowSums(as.matrix(mat_subp_agri[, cols_autumn]), na.rm = TRUE) / 6,
             rowSums(as.matrix(mat_subp_agri[, cols_winter]), na.rm = TRUE) / 6),
    Season = factor(rep(c("Spring", "Summer", "Autumn", "Winter"),
                        each = nrow(mat_subp_agri)),
                    levels = c("Winter", "Spring", "Summer", "Autumn")),
    Quantile = factor(q, levels = c(10,25,50,75,90)),
    Scenario = "Sub-plinian"
  )
  list_dfs_subp[[as.character(q)]] <- df_q
}
df_subp <- bind_rows(list_dfs_subp)

# -------------------------------
# CONCAT FINAL
# -------------------------------
SIYL_season_combined <- bind_rows(df_stromb, df_subp)
}
##Violin plot SIYL, toutes cultures confondues
if (1 == 1) {
ggplot(SIYL_season_combined, aes(x = Quantile, y = SIYL, fill = Quantile)) +
  geom_violin(alpha = 0.4, scale = "width", color = "black") +
  scale_fill_manual(values = c("10" = "orange", 
                               "25" = "gold", 
                               "50" = "green", 
                               "75" = "blue", 
                               "90" = "purple")) +
  labs(x = "", y = "Seasonal Index of Yield Loss (SIYL, %)",
       fill = "Ashfall quantile") +
  facet_grid(Scenario ~ Season) +
  theme_minimal()



##Violin plots des pertes par saison
if (1 == 1) {
ggplot(SIYL_season_stromb_combined, aes(x = Season, y = SIYL, fill = Season)) +
  geom_violin(alpha = 0.5, color = "black", scale = "width") +
  scale_fill_manual(values = c("Winter" = "blue", 
                               "Spring" = "green", 
                               "Summer" = "orange", 
                               "Autumn" = "red")) +
  labs(x = "", y = "Seasonal Index of Yield Loss (SIYL, %)",
       fill = "Season") +
  facet_wrap(~ Quantile, ncol = 3) +
  theme_minimal()
}

#Code modifié pour rassembler les quantiles en 1 violin plot
ggplot(SIYL_season_combined, aes(x = Season, y = SIYL, fill = Quantile)) +
  geom_violin(alpha = 0.4, color = "black", scale = "width", position = "identity") +
  scale_fill_manual(values = c("10" = "orange", 
                               "25" = "gold", 
                               "50" = "green", 
                               "75" = "blue", 
                               "90" = "purple")) +
  labs(x = "", y = "Seasonal Index of Yield Loss (SIYL, %)",
       fill = "Ashfall quantile") +
  facet_wrap(~ Scenario, ncol = 1) +
  theme_minimal()
}
##Ridge plot : une ligne par saison, facetté par quantile
if (1 == 1) {
ggplot(SIYL_season_stromb_combined, 
       aes(x = SIYL, y = Season, fill = Season)) +
  geom_density_ridges(alpha = 0.6, scale = 1) +
  scale_fill_manual(values = c("Winter" = "blue", 
                               "Spring" = "green", 
                               "Summer" = "orange", 
                               "Autumn" = "red")) +
  facet_wrap(~ Quantile, ncol = 3) +
  labs(x = "Seasonal Index of Yield Loss (SIYL, %)",
       y = "Season",
       fill = "Season") +
  theme_minimal()
}
##CDF plot du SIYL, toutes cultures confondues
if (1 == 1) {
SIYL_cdf_all <- SIYL_season_combined %>%
  group_by(Scenario, Quantile, Season) %>%
  do({
    xvals <- sort(.$SIYL)
    data.frame(
      SIYL = xvals,
      Exceedance = 1 - ecdf(.$SIYL)(xvals)
    )
  })

ggplot(SIYL_cdf_all, aes(x = SIYL, y = Exceedance, color = Quantile, linetype = Scenario)) +
  geom_line() +
  facet_wrap(~ Season) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Seasonal Index of Yield Loss (SIYL, %)",
       y = "Probability of exceeding",
       color = "Ashfall quantile",
       linetype = "Eruption scenario") +
  theme_minimal()
}
##Recalcul et stockage de SIYL dans mat_stromb et mat_subp comme l'a été AIYL pour ensuite pouvoir faire des graph par culture
if (1 == 1) {
  
  ## STROMBOLIENNE
  for (q in quantiles) {
    # Colonnes par saison
    cols_spring <- grep(paste0("^yl", q, "_p(6|7|8|9|10|11)_stromb$"), names(mat_stromb), value = TRUE)
    cols_summer <- grep(paste0("^yl", q, "_p(12|13|14|15|16|17)_stromb$"), names(mat_stromb), value = TRUE)
    cols_autumn <- grep(paste0("^yl", q, "_p(18|19|20|21|22|23)_stromb$"), names(mat_stromb), value = TRUE)
    cols_winter <- grep(paste0("^yl", q, "_p(1|2|3|4|5|24)_stromb$"), names(mat_stromb), value = TRUE)
    
    # Calculs par saison
    mat_stromb[[paste0("SIYL", q, "_spring_stromb")]] <- rowSums(mat_stromb[, cols_spring], na.rm = TRUE) / 6
    mat_stromb[[paste0("SIYL", q, "_summer_stromb")]] <- rowSums(mat_stromb[, cols_summer], na.rm = TRUE) / 6
    mat_stromb[[paste0("SIYL", q, "_autumn_stromb")]] <- rowSums(mat_stromb[, cols_autumn], na.rm = TRUE) / 6
    mat_stromb[[paste0("SIYL", q, "_winter_stromb")]] <- rowSums(mat_stromb[, cols_winter], na.rm = TRUE) / 6
  }
  
  ## SUB-PLINIENNE
  for (q in quantiles) {
    # Colonnes par saison
    cols_spring <- grep(paste0("^yl", q, "_p(6|7|8|9|10|11)_subp$"), names(mat_subp), value = TRUE)
    cols_summer <- grep(paste0("^yl", q, "_p(12|13|14|15|16|17)_subp$"), names(mat_subp), value = TRUE)
    cols_autumn <- grep(paste0("^yl", q, "_p(18|19|20|21|22|23)_subp$"), names(mat_subp), value = TRUE)
    cols_winter <- grep(paste0("^yl", q, "_p(1|2|3|4|5|24)_subp$"), names(mat_subp), value = TRUE)
    
    # Calculs par saison
    mat_subp[[paste0("SIYL", q, "_spring_subp")]] <- rowSums(mat_subp[, cols_spring], na.rm = TRUE) / 6
    mat_subp[[paste0("SIYL", q, "_summer_subp")]] <- rowSums(mat_subp[, cols_summer], na.rm = TRUE) / 6
    mat_subp[[paste0("SIYL", q, "_autumn_subp")]] <- rowSums(mat_subp[, cols_autumn], na.rm = TRUE) / 6
    mat_subp[[paste0("SIYL", q, "_winter_subp")]] <- rowSums(mat_subp[, cols_winter], na.rm = TRUE) / 6
  }
}
##Préparation du dataframe long SIYL
if (1 == 1) {
# On extrait la culture directement depuis mat_stromb et mat_subp
mat_stromb_agri <- mat_stromb[mat_stromb$lu %in% c(100,92,73), c("lu", grep("^SIYL", names(mat_stromb), value = TRUE))]
mat_subp_agri   <- mat_subp[mat_subp$lu %in% c(100,92,73), c("lu", grep("^SIYL", names(mat_subp), value = TRUE))]

# Fonction helper pour fondre en long
melt_siyl <- function(mat, scenario_label) {
  df_long <- melt(mat, id.vars = "lu", variable.name = "Var", value.name = "SIYL")
  
  # Récupère Season et Quantile depuis le nom de colonne
  df_long$Season <- sub(".*_(spring|summer|autumn|winter)_.*", "\\1", df_long$Var)
  df_long$Quantile <- sub("SIYL(\\d+)_.*", "\\1", df_long$Var)
  df_long$Scenario <- scenario_label
  
  # Récupère Culture
  df_long$Culture <- NA
  df_long$Culture[df_long$lu == 100] <- "Olive trees"
  df_long$Culture[df_long$lu == 92]  <- "Orchards"
  df_long$Culture[df_long$lu == 73]  <- "Vineyards"
  
  df_long$Quantile <- factor(df_long$Quantile, levels = c("10","25","50","75","90"))
  df_long$Season <- factor(df_long$Season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  
  return(df_long %>% filter(!is.na(Culture)))
}

# On applique aux deux matrices
SIYL_long_stromb <- melt_siyl(mat_stromb_agri, "Strombolian")
SIYL_long_subp   <- melt_siyl(mat_subp_agri,   "Sub-plinian")

# On combine le tout
SIYL_long <- bind_rows(SIYL_long_stromb, SIYL_long_subp)
}
##CDF plot du SIYL par culture --> pas réussi jusqu'ici



