library(topicmodels)
library(ggplot2)
source("functions/lda.R")


# -- get all files -------------------------------------------------------------
files <- list.files("data/generated", "^lda", full.names = TRUE)
files <- files[!grepl("VEM", files)]

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

aics_by_year <- as.data.frame(aics, stringsAsFactors = FALSE)
aics_by_year$aic <- as.numeric(aics_by_year$aic)
aics_by_year$k   <- as.numeric(aics_by_year$k)


## all years grouped
ldas <- ls(pattern = "lda_all_years")

aics <- matrix(0, 0, 2)
for (lda in ldas) {
  k <- strsplit(lda, "_")[[1]][4]

  aic <- AIC(get(lda))
  aics <- rbind(aics, cbind(k, aic))
}

aics_all_years <- as.data.frame(aics, stringsAsFactors = FALSE)
aics_all_years$aic <- as.numeric(aics_all_years$aic)
aics_all_years$k   <- as.numeric(aics_all_years$k)
aics_all_years_delta <- abs(diff(aics_all_years$aic[order(aics_all_years$k)]))

## all years grouped by threshold
ldas <- ls(pattern = "lda_[[:digit:]]*_percent")

aics <- matrix(0, 0, 4)
for (lda in ldas) {
  percent <- strsplit(lda, "_")[[1]][2]
  k       <- strsplit(lda, "_")[[1]][4]

  nsp  <- length(get(lda)@terms)
  aic  <- AIC(get(lda))
  aics <- rbind(aics, cbind(percent, k, aic, nsp))
}

aics_by_thrs <- as.data.frame(aics, stringsAsFactors = FALSE)
aics_by_thrs$aic <- as.numeric(aics_by_thrs$aic)
aics_by_thrs$k   <- as.numeric(aics_by_thrs$k)
aics_by_thrs$percent <- paste(aics_by_thrs$percent, "%")


# -- cross-validation based selection ------------------------------------------
files <- list.files("data/generated", "cv_lda_1", full.names = TRUE)

cv_results <- data.frame(matrix(0, 0, 2))
for (file in files) {
  cv_results <- rbind(cv_results, read.csv(file))
}

cv_means <- aggregate(perplexity ~ k, cv_results, mean)
cv_delta <- abs(diff(cv_means$perplexity))

# -- posterior analyses --------------------------------------------------------
# here we choose 9 groups as a good number of groups based on AIC and CV
k <- 9
mod <- get(paste0("lda_all_years_", k, "_groups"))
post <- posterior(mod)

# tidy posterior analyses in a friendly way
tidy_post  <- tidy_lda_post(mod)
tidy_plots <- tidy_post[["plots"]]
tidy_plots$group <- gsub("gp", "", tidy_plots$group)
tidy_plots$year <- sapply(strsplit(tidy_plots$parc, "_"), "[", 1)

# get the crop informations
cor_crops <- read.csv("data/generated/corres_parc_crop.csv",
                      stringsAsFactors = FALSE)

for (parc in unique(tidy_plots$parc)) {
  num_parc <- strsplit(parc, "_")[[1]][2]
  num_parc <- gsub("-Pa$", "", num_parc)

  tidy_plots$crop[tidy_plots$parc == parc] <-
    cor_crops$crop.analyses[grepl(num_parc, cor_crops$carre.parc)]
}

# dataframes for representing the most likely group according to year and crop
## get the most likely topic for each plot
mostlik <- topics(mod)
mostlik <- cbind.data.frame(parc = names(mostlik), ml = as.numeric(mostlik),
                            stringsAsFactors = FALSE)
## add teh year and the crop
mostlik$year <- sapply(strsplit(mostlik$parc, "_"), "[", 1)
mostlik$crop <- tidy_plots$crop[1:1888]

## count by year
### count plots with each most likely topic
tab <- as.data.frame(table(mostlik$year, mostlik$ml))
colnames(tab) <- c("year", "mostlik", "count")
### transform into percentage for comparisons
b <- aggregate(count ~ year, data = tab, function(x) x/sum(x))
a <- cbind.data.frame(year = b$year, mostlik = rep(1:k, rep(10, k)),
                      prop = as.numeric(b$count) * 100)
mostlik_by_year <- a

## count by crop
### count plots with each most likely topic
tab <- as.data.frame(table(mostlik$crop, mostlik$ml))
colnames(tab) <- c("crop", "mostlik", "count")
### transform into percentage for comparisons
b <- aggregate(count ~ crop, data = tab, function(x) x/sum(x))
a <- cbind.data.frame(crop = b$crop, mostlik = rep(1:k, rep(length(b$crop), k)),
                      prop = as.numeric(b$count) * 100)
mostlik_by_crop <- a

# dataframes with groups
comp_com <- tidy_post[["spp"]]

thrs <- 0.1
ml_comp <- data.frame(matrix(0, 0, 3))
ml_comp_reporting <- data.frame(matrix(0, 0, 3))
for (group in unique(comp_com$group)) {
  ml <- comp_com[comp_com$group == group & comp_com$rel_ab > thrs,]
  ml <- ml[order(ml$rel_ab, decreasing = TRUE),]
  ml_ <- paste("\textit{", ml$sp, "} (", round(ml$rel_ab, 2), ")", sep = "", collapse = ", ")
  ml_reporting <-
    paste(ml$sp, " (", round(ml$rel_ab, 2), ")", sep = "", collapse = ", ")
  ml_comp <- rbind(ml_comp, cbind.data.frame(group, ml_, stringsAsFactors = FALSE),
                   stringsAsFactors = FALSE)
  ml_comp_reporting <-
    rbind(ml_comp_reporting,
          cbind.data.frame(group, ml_reporting, stringsAsFactors = FALSE),
          stringsAsFactors = FALSE)
}

colnames(ml_comp) <- c("Groupe", "Espèces les plus probables")
colnames(ml_comp_reporting) <- c("Groupe", "Espèces les plus probables")

# -- cleaning and saving environment -------------------------------------------
keep <- c("lda_all_years_9_groups")
notkeep <- ls(pattern = "lda")
notkeep <- notkeep[!notkeep %in% keep]

rm(list = notkeep)

save.image("data/generated/envir_lda.Rdata")
