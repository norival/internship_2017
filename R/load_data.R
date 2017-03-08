# ------------------------------------------------------------------------------
# file        : load_data.R
# author      : Xavier Laviron
# object      : This file loads, merge and subset the datasets
# ------------------------------------------------------------------------------

# -- packages ------------------------------------------------------------------
library(tidyverse)

# -- data management -----------------------------------------------------------
# loads and converts the data
# This has to be run only once and, when databases will be clean, it should not
# be run again.

# loading
cat("Chargement des données...\n")
if (!(file.exists("data/generated/converted_data.csv"))) {
  # if not done already
  source("db_convert.R")
}

converted_data <- read.csv("data/generated/converted_data.csv",
                           stringsAsFactors = FALSE,
                           encoding = "utf8")
other_data <- read.csv("data/raw/BDD_dec2016_culture.csv",
                       stringsAsFactors = FALSE,
                       dec = ",",
                       encoding = "utf8")

# récupération de la dose d'azote depuis la base de Robin
other_data$dose_n <- NA

# merge the 2 databases into one data.frame
data_full_tmp <- rbind.data.frame(other_data, converted_data)

# remove trailing and leading whitespace from dataframe
data_full_tmp <-
  data_full_tmp %>%
  mutate_all(.funs = trimws)

# write it to a file
write.csv(data_full_tmp, "data/generated/BDD_full_dirty.csv", row.names = FALSE)

cat("Nettoyage des valeurs...\n")
source("clean_values.R")

data_full <- read.csv("data/generated/BDD_full.csv", stringsAsFactors = FALSE)

# -- subsetting du jeu de données ----------------------------------------------
cat("Subsetting...\n")
# récupération des plots contenant des relevés de flore
data_sub <-
  data_full %>%
  filter(ID_Enquêteur %in% c("inchausti", "grison"),
         Année_Enquête %in% c(2007, 2011))

data_sub$Produit_Ferti <-
  data_sub$Produit_Ferti %>%
  stri_replace_all(fixed = "/", repl = "-")

data_sub$id_parc_tri <- paste(data_sub$ID_Exploitation,
                              data_sub$ID_Parcelle,
                              data_sub$Année_SuiviParcelle,
                              sep = "_")

# reads the reference table for fertilizers
fert <- read.csv("data/raw/BD_Fertilizers_dec2016.csv", stringsAsFactors = FALSE)
fert$Fertilisant  <- toupper(trimws(fert$Fertilisant))
colnames(fert)[2] <- "kg_n_100kg"

# calcul de la dose de fertilisants à partir de la concentration lue dans le
# tableau de référence
# compute reference dose from concentration found in the ref table
n_ha <- numeric(nrow(data_sub))
n_ha[n_ha == 0] <- NA

for (i in 1:nrow(data_sub)) {
  if (is.na(data_sub$Unité_dose[i])) {
    next
  }
  if (data_sub$Unité_dose[i] == "Unité/HA") {
    # 1 U/HA = 1 Kg/HA
    n_ha[i] <- data_sub$Dose_Ferti[i]
  }
  else if (data_sub$Unité_dose[i] == "Kg/HA") {
    if (toupper(data_sub$Produit_Ferti[i]) %in% fert$Fertilisant) {
      # m_n_100 is the nitrogen mass for 100 Kg of product, found in the
      # reference table
      m_n_100 <- fert$kg_n_100kg[fert$Fertilisant == toupper(data_sub$Produit_Ferti[i])] 
      n_ha[i] <- data_sub$Dose_Ferti[i] * m_n_100 / 100
    }
  }
}

# adds the new column to the df
data_sub$kg_n_ha <- n_ha

# adds column with the yield in qtx / Ha
data_sub$Rdt_Qtx <- as.numeric(data_sub$Rdt_Qtx)
data_sub$qtx_ha <- data_sub$Rdt_Qtx / as.numeric(data_sub$Surface_ha)

write.csv(data_sub, "data/generated/BDD_sub.csv", row.names = FALSE)

# ------------------------------------------------------------------------------

# formatting and subsetting the data.frame with Sabrina's scripts
# cat("Conversion et subsetting du fichier enquêtes...\n")
# source("util/load_enquete.R", encoding = "latin1")

# source("util/R_pesticides.R", encoding = "latin1")
# ift_herbi <- Intensite_Traitement()

# -- flora ---------------------------------------------------------------------
# adds scripts to compute estimates of abundance

library(doParallel)
registerDoParallel(cores=4)
foreach (i = 2006:2010) %dopar% {
  print(i)
  filename <- paste("util/code", i, ".R", sep = "")
  source(filename, encoding = "latin1")
}
