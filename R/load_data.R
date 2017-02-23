# ------------------------------------------------------------------------------
# file        : load_data.R
# author      : Xavier Laviron
# object      : This file loads, merge and subset the datasets
# ------------------------------------------------------------------------------

# -- packages ------------------------------------------------------------------
library(plyr)
library(dplyr)

# -- data management -----------------------------------------------------------
# loads and converts the data
# This has to be run only once and, when databases will be clean, it should not
# be run again.

# loading
if (!(file.exists("data/converted_data.csv"))) {
  # if not done already
  source("db_convert.R")
}

converted_data <- read.csv("data/converted_data.csv",
                           stringsAsFactors = FALSE,
                           dec = ",",
                           encoding = "utf8")
other_data <- read.csv("data/BDD_dec2016_culture.csv",
                       stringsAsFactors = FALSE,
                       dec = ",",
                       encoding = "utf8")

# merge the 2 databases into one data.frame
data_full <- rbind.data.frame(other_data, converted_data)

# remove trailing and leading whitespace from dataframe
data_full <-
  data_full %>%
  mutate_all(.funs = trimws)

data_full$Protection_visée[data_full$Produit_phyto == "KARATE K"] <- "insecticide"
data_full$Produit_phyto[data_full$Produit_phyto == "U46D"] <- "U 46 D"

# write it to a file
write.csv(data_full, "data/BDD_full.csv", row.names = FALSE)

# ------------------------------------------------------------------------------

# formatting and subsetting the data.frame with Sabrina's scripts
cat("Conversion et subsetting du fichier enquêtes...\n")
source("util/load_enquete.R", encoding = "latin1")

source("util/R_pesticides.R", encoding = "latin1")
ift_herbi <- Intensite_Traitement()
