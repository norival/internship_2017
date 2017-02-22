# ------------------------------------------------------------------------------
# file        : load_data.R
# author      : Xavier Laviron
# object      : This file loads, merge and subset the datasets
# ------------------------------------------------------------------------------

# -- packages ------------------------------------------------------------------
library(plyr)

# -- data management -----------------------------------------------------------
# loads and converts the data

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

data_full <- rbind.data.frame(other_data, converted_data)

# merging the 2 databases into one data.frame

write.csv(data_full, "data/BDD_full.csv", row.names = FALSE)

# formatting and subsetting the data.frame with Sabrina's scripts
cat("Conversion et subsetting du fichier enquêtes...\n")
source("util/load_enquete.R", encoding = "latin1")

source("util/R_pesticides.R", encoding = "latin1")
ift_herbi <- Intensite_Traitement()
