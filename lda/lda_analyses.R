library(topicmodels)
library(ggplot2)
source("functions/lda.R")


# -- get all files -------------------------------------------------------------
files <- list.files("data/generated", "lda", full.names = TRUE)

## load all files
for (file in files) {
  load(file)
}

rm("file", "files")

# -- compute and compare AIC ---------------------------------------------------
## by year

ldas <- ls(pattern = "lda_year")

aics <- matrix(0, 0, 3)
for (lda in ldas) {
  year <- strsplit(lda, "_")[[1]][3]
  k    <- strsplit(lda, "_")[[1]][4]

  aic <- AIC(get(lda))
  aics <- rbind(aics, cbind(year, k, aic))
}

aics <- as.data.frame(aics, stringsAsFactors = FALSE)
aics$aic <- as.numeric(aics$aic)
aics$k   <- as.numeric(aics$k)

ggplot(aics, aes(x = k, y = aic, colour = year)) +
  geom_point()


## all years grouped
ldas <- ls(pattern = "lda_all_years")

aics <- matrix(0, 0, 2)
for (lda in ldas) {
  k <- strsplit(lda, "_")[[1]][4]

  aic <- AIC(get(lda))
  aics <- rbind(aics, cbind(k, aic))
}

aics <- as.data.frame(aics, stringsAsFactors = FALSE)
aics$aic <- as.numeric(aics$aic)
aics$k   <- as.numeric(aics$k)

ggplot(aics, aes(x = k, y = aic)) +
  geom_point()


## all years grouped by threshold
ldas <- ls(pattern = "lda_[[:digit:]]*_percent")

aics <- matrix(0, 0, 3)
for (lda in ldas) {
  percent <- strsplit(lda, "_")[[1]][2]
  k       <- strsplit(lda, "_")[[1]][4]

  aic <- AIC(get(lda))
  aics <- rbind(aics, cbind(percent, k, aic))
}

aics <- as.data.frame(aics, stringsAsFactors = FALSE)
aics$aic <- as.numeric(aics$aic)
aics$k   <- as.numeric(aics$k)

ggplot(aics, aes(x = k, y = aic)) +
  geom_point() +
  facet_wrap(~ percent, scales = "free_y")
