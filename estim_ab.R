# ------------------------------------------------------------------------------
# file        : estim_ab.R
# author      : Xavier Laviron
# object      : This file contain code to estimate abundances
# note        : The tables from which abundances are estimated need to be built
#               from scripts 'util/codes*.R
# ------------------------------------------------------------------------------

# -- packages and functions ----------------------------------------------------
source("functions/abundance.R")

# -- estimations ---------------------------------------------------------------
# as this is quite long, each year is estimated one by one

## year 2006 -------------------------------------------------------------------
data2006 <- read.csv("data/generated/transpose_abondance_per_quadrat2006.csv",
                     sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

abond_per_plot <- estim_abundance(data2006, surf = 4, n_cores = 4,
                                  fun = "gammapoisson")

write.csv(abond_per_plot, "data/generated/abondt_per_plot_2006.csv",
          row.names = TRUE)

rm(c("abond_per_plot", "data2006"))


## year 2007 -------------------------------------------------------------------
data2007 <- read.csv("data/generated/transpose_abondance_per_quadrat2007.csv",
                     sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

abond_per_plot <- estim_abundance(data2007, surf = 4, n_cores = 4,
                                  fun = "gammapoisson")

write.csv(abond_per_plot, "data/generated/abond_per_plot_2007.csv",
          row.names = TRUE)
rm(c("abond_per_plot", "data2007"))


## year 2008 -------------------------------------------------------------------
data2008 <- read.csv("data/generated/transpose_abondance_per_quadrat2008.csv",
                     sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

abond_per_plot <- estim_abundance(data2008, surf = 4, n_cores = 4,
                                  fun = "gammapoisson")

write.csv(abond_per_plot, "data/generated/abond_per_plot_2008.csv",
          row.names = TRUE)
rm(c("abond_per_plot", "data2008"))


## year 2009 -------------------------------------------------------------------
data2009 <- read.csv("data/generated/transpose_abondance_per_quadrat2009.csv",
                     sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

abond_per_plot <- estim_abundance(data2009, surf = 4, n_cores = 4,
                                  fun = "gammapoisson")

write.csv(abond_per_plot, "data/generated/abond_per_plot_2009.csv",
          row.names = TRUE)
rm(c("abond_per_plot", "data2009"))


## year 2010 -------------------------------------------------------------------
data2010 <- read.csv("data/generated/transpose_abondance_per_quadrat2010.csv",
                     sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

abond_per_plot <- estim_abundance(data2010, surf = 4, n_cores = 4,
                                  fun = "gammapoisson")

write.csv(abond_per_plot, "data/generated/abond_per_plot_2010.csv",
          row.names = TRUE)
rm(c("abond_per_plot", "data2010"))


## year 2011 -------------------------------------------------------------------
data2011 <- read.csv("data/generated/transpose_abondance_per_quadrat2011.csv",
                     sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

abond_per_plot <- estim_abundance(data2011, surf = 4, n_cores = 4,
                                  fun = "gammapoisson")

write.csv(abond_per_plot, "data/generated/abond_per_plot_2011.csv",
          row.names = TRUE)
rm(c("abond_per_plot", "data2011"))


## year 2013 -------------------------------------------------------------------
data2013 <- read.csv("data/generated/transpose_abondance_per_quadrat2013.csv",
                     sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

abond_per_plot <- estim_abundance(data2013, surf = 4, n_cores = 4,
                                  fun = "gammapoisson")

write.csv(abond_per_plot, "data/generated/abond_per_plot_2013.csv",
          row.names = TRUE)
rm(c("abond_per_plot", "data2013"))


## year 2014 -------------------------------------------------------------------
# Passage des notes en log2
# On considère ici qu'il ne peut y avoir plus d'une plante par sous-quadra. On
# regroupe donc les sous-quadras en faisant la somme des indices d'abondances et
# si une note est > 2, on la remplace par 2.
data2014 <- read.csv("data/generated/transpose_abondance_per_sousquadrat2014.csv",
                  sep = ";", stringsAsFactors = FALSE)

# grouping
data2014 <- group_subqd(data2014, base2 = TRUE, n.subqd = 4)

abond_per_plot <- estim_abundance01(data2014, surf = 1, gp.subquadra = T, base2 = T)

write.csv(abond_per_plot, "data/generated/abond_per_plot_2014.csv",
          row.names = TRUE)
rm(c("abond_per_plot", "data2014"))


## year2015 --------------------------------------------------------------------
# Sub quadrates are grouped, as for 2014
data2015 <- read.csv("data/generated/transpose_abondance_per_sousquadrat2015.csv",
                  sep = ";", stringsAsFactors = FALSE)

abond_per_plot <- estim_abundance01(data2015, surf = 1, gp.subquadra = T, base2 = T)

write.csv(abond_per_plot, "data/generated/abond_per_plot_2015.csv",
          row.names = TRUE)
rm(c("abond_per_plot", "data2015"))


## year2016 --------------------------------------------------------------------
# Sub quadrates are grouped, as for 2014
data2016 <- read.csv("data/generated/transpose_abondance_per_sousquadrat2016.csv",
                  sep = ";", stringsAsFactors = FALSE)

abond_per_plot <- estim_abundance01(data2016, surf = 1, gp.subquadra = T, base2 = T)

write.csv(abond_per_plot, "data/generated/abond_per_plot_2016.csv",
          row.names = TRUE)
rm(c("abond_per_plot", "data2016"))
