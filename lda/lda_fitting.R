# -- packages and functions ----------------------------------------------------
library(topicmodels)
library(doSNOW)
source("functions/lda.R")

dat <- as.matrix(read.csv("data/generated/abond_per_plot_all_years.csv",
                          row.names = 1))

# -- compute the lda models ----------------------------------------------------
# lda model on all years grouped together

control <- list(seed = 42, burnin = 10000, thin = 500, iter = 50000)

ldas_all_years <-
  par_lda(tab = dat, groups = 2:15, control = control, ncores = 3,
          datname = "lda_all_years_K_groups",
          path    = "data/generated")


# lda model year by year
ldas_by_year <- list()

for (year in 2006:2016) {
  if (year == 2012) next

  cat("year ", year, "...\n", sep = "")
  file <- paste0("abond_per_plot_", year, ".csv")
  dat_year <- dat[grepl(paste0("^", year), rownames(dat)),]

  ldas_by_year[[as.character(year)]] <-
    par_lda(tab = dat_year, groups = 2:15, control = control, ncores = 3,
            datname = paste0("lda_year_", year, "_K_groups"),
            path    = "data/generated")
}
