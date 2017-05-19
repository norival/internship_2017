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

aics_by_year <- as.data.frame(aics, stringsAsFactors = FALSE)
aics_by_year$aic <- as.numeric(aics_by_year$aic)
aics_by_year$k   <- as.numeric(aics_by_year$k)

p_aics_by_year <-
  ggplot(aics_by_year, aes(x = k, y = aic, colour = year)) +
  geom_point() +
  scale_x_continuous(breaks = 2:15) +
  theme_bw()


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

p_aics_all_years <-
  ggplot(aics_all_years, aes(x = k, y = aic)) +
  geom_point() +
  scale_x_continuous(breaks = 2:15) +
  theme_bw()


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

p_aics_by_thrs <-
  ggplot(aics_by_thrs, aes(x = k, y = aic)) +
  geom_point() +
  scale_x_continuous(breaks = 2:15) +
  facet_wrap(~ percent, scales = "free_y") +
  theme_bw()


# -- posterior analyses --------------------------------------------------------
# groups per year
mod <- lda_5_percent_5_groups
tidy_post  <- tidy_lda_post(mod)
tidy_plots <- tidy_post[["plots"]]
tidy_plots$year <- sapply(strsplit(tidy_plots$parc, "_"), function(x) x[1])

# group by year
a <- aggregate(percent ~ year + group, data = tidy_plots,
               mean)

ggplot(a, aes(x = year, y = percent, fill = group)) +
  geom_bar(stat = 'identity')

# group by crop
## get the crop
cor_crops <- read.csv("data/generated/corres_parc_crop.csv",
                      stringsAsFactors = FALSE)

tidy_plots$crop <- ""
for (parc in unique(tidy_plots$parc)) {
  num_parc <- strsplit(parc, "_")[[1]][2]
  num_parc <- gsub("-Pa$", "", num_parc)

  tidy_plots$crop[tidy_plots$parc == parc] <-
    cor_crops$crop.analyses[grepl(num_parc, cor_crops$carre.parc)]
}

a <- aggregate(percent ~ crop + group, data = tidy_plots,
               mean)
a <- a[a$crop %in% c("cereal", "osr", "sunflower", "maize"),]

ggplot(a, aes(x = crop, y = percent, fill = group)) +
  geom_bar(stat = 'identity')
