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
cat("Chargement des données...\n")
if (!(file.exists("data/converted_data.csv"))) {
  # if not done already
  source("db_convert.R")
}

converted_data <- read.csv("data/converted_data.csv",
                           stringsAsFactors = FALSE,
                           encoding = "utf8")
other_data <- read.csv("data/BDD_dec2016_culture.csv",
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
write.csv(data_full_tmp, "data/BDD_full_tmp.csv", row.names = FALSE)

cat("Nettoyage des valeurs...\n")
source("clean_values.R")

data_full <- read.csv("data/BDD_full.csv", stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------

# formatting and subsetting the data.frame with Sabrina's scripts
cat("Conversion et subsetting du fichier enquêtes...\n")
source("util/load_enquete.R", encoding = "latin1")

# source("util/R_pesticides.R", encoding = "latin1")
# ift_herbi <- Intensite_Traitement()
