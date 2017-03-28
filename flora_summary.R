# -- packages and functions ----------------------------------------------------
library(tidyverse)

# summary of flora data
files <- list.files("data/generated", pattern = "abondt_per_quadra", full.names = TRUE)
files <- files[!grepl("binomiale", files)]

surf <- 20
for (file in files) {
  year <- strsplit(file, "[_\\.]")[[1]][4]
  var_name <- paste("weeds", year, sep = "")

  assign(var_name, read.csv(file, stringsAsFactor = FALSE, row.names = 1) * surf)
}


# -- all plants ----------------------------------------------------------------
sum_plots <- data.frame(year        = numeric(0),
                        n_plots     = numeric(0),
                        n_sp        = numeric(0),
                        n_sp_pplot  = numeric(0),
                        mab_pplot   = numeric(0))
sum_flora <- data.frame(year      = numeric(0),
                        sp        = character(0),
                        total     = numeric(0))


for (year in (2006:2016)[-which(2006:2016 == 2012)]) {
  a <- get(paste("weeds", year, sep = ""))

  if (year <= 2011) {
    # removes interface for which abundance have not been estimated
    a <- a[!grepl("-In", rownames(a)),]
  }

  # replace very small values by 0
  a[a <= 3.5e-09] <- 0

  # remove species that are not present
  a <- a[, apply(a, 2, sum) != 0]

  # number of species
  n_sp <- ncol(a)

  # nb plots
  n_plots <- length(unique(rownames(a)))

  # nb species per plot
  n_sp_pplot <- mean(apply(a, 1, function(x) sum(x != 0)))

  # mean abundance per plot
  mab_pplot <- mean(apply(a, 1, mean))

  # present species
  sp <- colnames(a)

  # sum per species
  total <- as.numeric(apply(a, 2, sum))

  # fill the dataframe
  tab <- cbind(year, n_plots, n_sp, n_sp_pplot, mab_pplot)
  sum_plots <- rbind.data.frame(sum_plots, tab)

  tab <- cbind.data.frame(year, sp, total)
  sum_flora <- rbind.data.frame(sum_flora, tab)
}

sum_flora$total <- as.numeric(as.character(sum_flora$total))

sum_flora <-
  sum_flora %>%
  group_by(sp) %>%
  summarise(total = sum(total))

species <- character(0)
parc    <- character(0)
for (year in (2006:2016)[-which(2006:2016 == 2012)]) {
  a <- get(paste("weeds", year, sep = ""))

  if (year <= 2011) {
    # removes interface for which abundance have not been estimated
    a <- a[!grepl("-In", rownames(a)),]
  }

 species <- c(species, colnames(a))
 parc <- c(parc, rownames(a))
}

mat <-
  as.data.frame(matrix(0, nrow = length(unique(parc)), ncol = length(unique(species))),
                stringsAsFactor = FALSE)
rownames(mat) <- unique(parc)
colnames(mat) <- unique(species)

for (year in (2006:2016)[-which(2006:2016 == 2012)]) {
  a <- get(paste("weeds", year, sep = ""))

  if (year <= 2011) {
    # removes interface for which abundance have not been estimated
    a <- a[!grepl("-In", rownames(a)),]
  }

  for (i in 1:nrow(a)) {
    for (ii in 1:ncol(a)) {
      parc <- rownames(a)[i]
      sp <- colnames(a)[ii]
      mat[parc, sp] <- a[i, ii]
    }
  }
}

# replace very small values by 0
mat[mat <= 3.5e-09] <- 0

# compute occurence index as the percentage of plots where the species is present
occurence <- apply(mat, 2, function(x) sum(x != 0) / length(x) * 100)

sum_flora$occurence <- NA
for (i in 1:nrow(sum_flora)) {
  sum_flora$occurence[i] <- occurence[sum_flora$sp[i]]
}

# adds 'status' based on the occurence of the species
sum_flora$status <- NA
sum_flora$status[sum_flora$occurence >= 25] <- "abundant"
sum_flora$status[sum_flora$occurence < 25 & sum_flora$occurence >= 5] <- "intermediate"
sum_flora$status[sum_flora$occurence < 5] <- "rare"
sum_flora <- sum_flora[order(sum_flora$total, decreasing = TRUE),]
