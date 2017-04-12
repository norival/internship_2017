# -- packages and functions ----------------------------------------------------
library(tidyverse)
source("functions/transpose_flora.R", encoding = "utf8")
source("functions/abundance.R", encoding = "utf8")


# -- data ----------------------------------------------------------------------
flore <- read.csv("data/raw/flore_tot_per1.csv", encoding = "latin1", sep = ";",
                  stringsAsFactors = FALSE)

# remove the "Transect" quadrat
flore2013 <- flore[flore$Annee == 2013 & flore$Quadrat != "Transect", ]

# remove all NAs lines
flore2013 <-
  flore2013[which(apply(flore2013[, 16:ncol(flore2013)], 1, function(x) sum(is.na(x)))
                  != ncol(flore2013) - 15),]


# -- check estimations ---------------------------------------------------------
# Check the estimates against the real values
# The values are converted whether in base 0 (0/1) or base2 or base10 and then
# the abundance is estimated like for the other samples.
surf <- 1

# transpose and convert the dataframe
transposed <- transpose_flora_tot(flore2013)

## test correlation with geometric mean
gmean <- transposed[["base10"]][, -(1:4)]
gmean[gmean == 2] <- exp(mean(log(c(2, 9))))
gmean[gmean == 3] <- exp(mean(log(c(10, 99))))
gmean[gmean == 4] <- exp(mean(log(c(100, 999))))
gmean[gmean == 5] <- exp(mean(log(c(1000, 9999))))

cor_gmean <- estim_summary_gm(tab = transposed[["orig"]], gmean, surf = 1)

## test on base2
# compute estimtions
estim2 <- estim_abundance(x = transposed[["base2"]], surf = 1, n_cores = 4, addpos = FALSE)

cor_base2 <- estim_summary(transposed[["orig"]], estim2, surf = 1)

save.image('/tmp/data_verif.RData')
