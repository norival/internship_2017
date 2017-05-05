# ------------------------------------------------------------------------------
# file        : load_data.R
# author      : Xavier Laviron
# object      : This file loads, merge and subset the datasets
# ------------------------------------------------------------------------------

# -- packages and functions ----------------------------------------------------
library(tidyverse)
source("functions/db_convert.R")
source("functions/clean_df.R")
source("functions/doseN.R")


# -- data management -----------------------------------------------------------
# loads and converts the data: there are two databases that must be converted
# into the same format

cat("Chargement des données...\n")

# read the tables
R <- read.csv("data/raw/BDD_robin_full.csv",
              stringsAsFactors = FALSE)
B <- read.csv("data/raw/BDD_dec2016_culture.csv",
              stringsAsFactors = FALSE)

# convert the data
converted_data <- convert_db(R, B, verbose = F)

# merge the 2 databases into one data.frame
data_full_dirty <- rbind.data.frame(B, converted_data)

cat("Nettoyage des valeurs...\n")
data_full <- clean_df(data_full_dirty)

write.csv(data_full, "data/generated/BDD_full.csv", row.names = FALSE)


# -- sormatting and  subsetting df ---------------------------------------------
# formatting and subsetting the data.frame with Sabrina's scripts

cat("Formating and subsetting datasets...\n")
source("util/load_enquete.R", encoding = "latin1")

source("util/R_pesticides.R", encoding = "latin1")
ift_herbi <- Intensite_Traitement()

# reads the reference table for fertilizers

# -- nitrogen dose computation -------------------------------------------------
# Compute nitrogen dose from values in reference table

# get the reference table
fert <- read.csv("data/raw/Engrais_DB_Rui280317.csv", dec = ",",
                 stringsAsFactors = FALSE)
# we need only the name and the notrogen dose
fert <- fert[, c("Nom.dans.BDD.Enquetes", "N")]
colnames(fert) <- c("product", "doseN")

# just a little cleaning to be sure (removes trailing whitespaces):
fert$product <- trimws(fert$product)

# make an unique identifier for the data
bdd <- read.csv("data/generated/BDD_full.csv", stringsAsFactors = FALSE)

bdd$id_parc_tri <- paste(bdd$ID_Parcelle, bdd$ID_Exploitation,
                         bdd$Année_SuiviParcelle, sep = "_")

# we need only the informations about fertilization in the database:
kept  <- c("id_parc_tri", "Produit_Ferti", "Dose_Ferti", "Unité_dose")
tab   <- bdd[, kept]

# compute the dose. Results is given in Kg/HA
nitrogen_doses <- n_dose(tab, fert)

# ------------------------------------------------------------------------------
data_full$id_parc_tri <- paste(data_full$ID_Parcelle, data_full$ID_Exploitation,
                               data_full$Année_SuiviParcelle, sep = "_")
kept  <- c("id_parc_tri", "Produit_Ferti", "Dose_Ferti", "Unité_dose")
tab   <- data_full[, kept]
n_doses <- n_dose(tab, fert)

# -- flora ---------------------------------------------------------------------
# adds scripts to compute estimates of abundance
source("util/code_annee_tot.R", encoding = "latin1")

for (i in c(2006:2011, 2013:2016)) {
  print(paste("year", i))
  filename <- paste("util/code", i, ".R", sep = "")
  source(filename, encoding = "latin1")
}
