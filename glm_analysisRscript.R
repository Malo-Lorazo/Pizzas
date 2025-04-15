# Chargement des bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(reshape2)
library(ComplexHeatmap)

setwd("C:/Users/loraz/Documents/Stage_Nez_Blanc/04_data/02_scripts/06_results_GLM/04_concat_results_NoTransfo/")
glm_results <- read.table("combined_results.txt", header = TRUE)
df <- glm_results

# Création des données de lignée
lineage_data <- list(
  # Pd1 (Lignée 1)
  "Gd_00442-ba" = 1, 
  "Gd_02407-aa" = 1,
  "Gd_04985-ea" = 1,
  "Gd_01111-aaa" = 1,
  "Gd_00293-aad" = 1,
  "Gd_00994-aaa" = 1,
  "Gd_01144-ba" = 1,
  "Gd_01882-ad" = 1,
  "Gd_03020-aa" = 1,
  "Gd_00194-a2ab" = 1,
  
  # Pd2 (Lignée 2)
  "Gd_00614-ba" = 2,
  "Gd_00708-ba" = 2,
  "Gd_04986-cc" = 2,
  "Gd_00045-a2ab" = 2,
  "Gd_02185-ab" = 2
)
# D'abord, on crée une fonction qui récupère la valeur de lignée à partir de lineage_data
get_lineage_value <- function(id) {
  # Retourne la valeur correspondante si l'identifiant existe dans lineage_data, sinon NA
  if(id %in% names(lineage_data)) {
    return(lineage_data[[id]])
  } else {
    return(NA)
  }
}

# On applique cette fonction sur les colonnes query et target
df$query_value <- sapply(df$query, get_lineage_value)
df$target_value <- sapply(df$target, get_lineage_value)

# Maintenant on peut multiplier les deux valeurs
df$multiplication <- df$query_value * df$target_value

# ------------------------ Importation des données



df_sort <- df[df$term == "SV1", ]
data <- df_sort[, c(1,2,5,8)] 

# Créer une matrice vide pour stocker les estimates
individuals <- unique(c(data$query, data$target))  # Liste des individus
estimate_matrix <- matrix(NA, nrow = length(individuals), ncol = length(individuals))
rownames(estimate_matrix) <- individuals
colnames(estimate_matrix) <- individuals

# Ajouter les estimates dans la matrice
for(i in 1:nrow(data)) {
  query <- data$query[i]
  target <- data$target[i]
  estimate <- data$estimate[i]
  estimate_matrix[query, target] <- estimate
  estimate_matrix[target, query] <- estimate  # matrice symétrique
}

# Préparer les données
melted_matrix <- melt(estimate_matrix)
colnames(melted_matrix) <- c("Individu1", "Individu2", "Estimate")

# Ajouter les p-values au dataframe
melted_matrix$p_value <- NA
for(i in 1:nrow(melted_matrix)) {
  row <- data[
    (data$query == melted_matrix$Individu1[i] & data$target == melted_matrix$Individu2[i]) |
      (data$target == melted_matrix$Individu1[i] & data$query == melted_matrix$Individu2[i]),
  ]
  if(nrow(row) > 0) {
    melted_matrix$p_value[i] <- row$p_value
  }
}

# Fonction de formatage scientifique personnalisé
format_scientific <- function(x) {
  ifelse(abs(x) < 1e-3, 
         sprintf("%.2e", x),  # Notation scientifique pour petites valeurs
         sprintf("%.2f", x)   # 2 décimales pour valeurs plus grandes
  )
}

# Ne garder que la moitié inférieure de la matrice mais inversée
melted_matrix_lower <- melted_matrix[lower.tri(estimate_matrix, diag = FALSE), ]
melted_matrix_lower$Individu1 <- factor(melted_matrix_lower$Individu1, 
                                        levels = rev(unique(melted_matrix_lower$Individu1)))
melted_matrix_lower$Individu2 <- factor(melted_matrix_lower$Individu2, 
                                        levels = unique(melted_matrix_lower$Individu2))

# Créer une colonne pour les étoiles
melted_matrix_lower$significance <- ifelse(melted_matrix_lower$p_value <= 0.01, "**", 
                                           ifelse(melted_matrix_lower$p_value <= 0.05, "*", ""))

# Créer la heatmap
ggplot(melted_matrix_lower, aes(x = Individu2, y = Individu1, fill = Estimate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(format_scientific(Estimate), significance)), 
            color = "black", 
            size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, 
                       name = "Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Heatmap des Estimates (Moitié inférieure)")

