# -- packages and functions ----------------------------------------------------
library(tidyverse)
library(topicmodels)
source("functions/lda.R")

# -- read the data -------------------------------------------------------------
files <- list.files("data/generated", pattern = "abondt_per_quadra", full.names = TRUE)
files <- files[!grepl("binomiale", files)]

surf <- 20
all_years <- numeric()
for (file in files) {
  year <- strsplit(file, "[_\\.]")[[1]][4]
  all_years <- c(all_years, year)
  var_name <- paste("weeds", year, sep = "")

  assign(var_name, read.csv(file, stringsAsFactor = FALSE, row.names = 1) * surf)
}


# -- clean values --------------------------------------------------------------
# group 2014:2016
allsp <- character()
allpc <- character()
for (i in 2014:2016) {
  tmp <- get(paste("weeds", i, sep = ""))
  allsp <- c(allsp, colnames(tmp))
  allpc <- c(allpc, paste(i, rownames(tmp), sep = "_"))
}
allsp <- unique(allsp)
allpc <- unique(allpc)

mat <- matrix(0, nrow = length(allpc), ncol = length(allsp))
colnames(mat) <- allsp
rownames(mat) <- allpc

for (pc in rownames(mat)) {
  year <- substr(pc, 1, 4)
  pcorig <- substr(pc, 6, nchar(pc))
  tmp <- get(paste("weeds", year, sep = ""))
  # set minimum values to 0
  tmp[tmp == min(tmp)] <- 0

  for (sp in colnames(mat)) {
    if (!(sp %in% colnames(tmp))) next
    mat[pc, sp] <- tmp[pcorig, sp]
  }
}

dat <- mat

# remove interface
dat <- dat[!(grepl("In", rownames(dat))),]

# truncate the values to the smaller greater integer
dat <- ceiling(dat)

# remove plots with no plant because LDA function cannot handle it
dat <- dat[rowSums(dat) != 0,]


# -- compute the lda models ----------------------------------------------------
aic <- numeric()

control <- list(seed = 42, burnin = 10000, thin = 500, iter = 50000)
for (k in 2:3) {
  Gibbs <- LDA(dat, k = k, method = "Gibbs", control = control)

  ll <- logLik(Gibbs)
  dfree <- Gibbs@k * length(Gibbs@terms)

  aic <- c(aic, (-2 * ll + 2 * dfree)[][1])
}
