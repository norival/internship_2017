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
# currently, for 2016 only
dat <- as.matrix(weeds2016)

# remove interface
dat <- dat[!(grepl("In", rownames(dat))),]

# set minimum values to 0
dat[dat == min(dat)] <- 0

# truncate the values to the smaller greater integer
dat <- ceiling(dat)

# remove plots with no plant because LDA function cannot handle it
dat <- dat[rowSums(dat) != 0,]


# -- compute the lda models ----------------------------------------------------
# currently, for 2016 only

# set the seed beacause method rely on random estmations, then compute the model
seed  <- "42"
mod   <- LDA(x = dat, k = 4, control = list(seed = seed))

# get the posterior probabilities from the model
post <- posterior(mod)

# tidy the post datasets into a form easily 'plotable'
tidy_post <- tidy_lda_post(post)

ggplot(tidy_post$plots, aes(x = x, y = val, colour = gp)) +
  geom_line()

ggplot(tidy_post$spp, aes(x = x, y = rel_ab, colour = gp)) +
  geom_line() +
  facet_grid(gp ~ .)
