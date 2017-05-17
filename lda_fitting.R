# -- packages and functions ----------------------------------------------------
library(topicmodels)
library(doSNOW)
source("functions/lda.R")

# -- read the data -------------------------------------------------------------
files <- list.files("data/generated", pattern = "abond_per_plot", full.names = TRUE)

surf <- 20
all_years <- numeric()
for (file in files) {
  year      <- strsplit(file, "[_\\.]")[[1]][4]
  all_years <- c(all_years, year)
  var_name  <- paste("weeds", year, sep = "")

  assign(var_name, read.csv(file, stringsAsFactor = FALSE, row.names = 1) * surf)
}


# -- clean values --------------------------------------------------------------
# group all years together

allsp <- character()
allpc <- character()

for (year in all_years) {
  tmp <- get(paste("weeds", year, sep = ""))
  allsp <- c(allsp, colnames(tmp))
  allpc <- c(allpc, paste(year, rownames(tmp), sep = "_"))
}

allsp <- unique(allsp)
allpc <- unique(allpc)

mat <- matrix(0, nrow = length(allpc), ncol = length(allsp))
colnames(mat) <- allsp
rownames(mat) <- allpc

for (pc in rownames(mat)) {
  year   <- substr(pc, 1, 4)
  pcorig <- substr(pc, 6, nchar(pc))
  tmp    <- get(paste("weeds", year, sep = ""))

  # set minimum values to 0
  tmp[tmp == min(tmp)] <- 0

  for (sp in colnames(tmp)) {
    mat[pc, sp] <- tmp[pcorig, sp]
  }
}

# remove interface
dat <- mat[!(grepl("In", rownames(mat))),]

# truncate the values to the smaller greater integer
dat <- ceiling(dat)

# remove plots with no plant because LDA function cannot handle it
dat <- dat[rowSums(dat) != 0,]


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
  dat_year <- as.matrix(get(paste0("weeds", year)))
  dat_year <- dat_year[!(grepl("In", rownames(dat_year))),]
  dat_year[dat_year == min(dat_year)] <- 0
  dat_year <- ceiling(dat_year)
  dat_year <- dat_year[rowSums(dat_year) != 0,]

  ldas_by_year[[as.character(year)]] <-
    par_lda(tab = dat_year, groups = 2:15, control = control, ncores = 3,
            datname = paste0("lda_year_", year, "_K_groups"),
            path    = "data/generated")
}
