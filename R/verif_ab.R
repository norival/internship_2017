# -- packages and functions ----------------------------------------------------
library(tidyverse)
source("functions/transpose_flora.R", encoding = "utf8")
source("functions/abundance.R", encoding = "utf8")


# -- data ----------------------------------------------------------------------
flore1 <- read.csv("data/raw/manip_ble_2014_p1.csv",
                   encoding = "utf8", stringsAsFactor = FALSE)
flore2 <- read.csv("data/raw/manip_ble_2014_p2.csv",
                   encoding = "utf8", stringsAsFactor = FALSE)
flore3 <- read.csv("data/raw/manip_ble_2014_p3.csv",
                   encoding = "utf8", stringsAsFactor = FALSE)

flore <- rbind(flore1, flore2, flore3)


# -- check estimations ---------------------------------------------------------
# Check the estimates against the real values
# The values are converted whether in base 0 (0/1) or base2 and then the
# abundance is estimated like for the other samples.
surf <- 0.25

dat <- as.data.frame(matrix(nrow = 0, ncol = 8, 0))
for (per in 1:3) {
  cat("Computing period ", per, "...\n", sep = "")
  tab <- flore[flore$Periode == per,]

  for (base in c(0, 2)) {
    cat(" Computing base ", base, "...\n", sep = "")
    # transpose the dataframe into the same format than the monitoring
    # dataframes so the functions to estimate abundances can be used
    tabt    <- transpose_flora(tab, base = base)
    origin  <- tabt[["origin"]]
    conv    <- tabt[["converted"]]

    # use the appropriate function to estimate the abundance of converted
    # dataframes, according to 'base'
    if (base == 0) {
      ab <- estim_abundance01(conv, surf, progress = FALSE, addpos = FALSE)
    } else {
      ab <- estim_abundance(conv, surf, n_cores = 4, progress = FALSE, addpos = FALSE)
    }

    estim <- estim_summary(origin, ab, surf)
    estim$periode <- paste("period", per, sep = "")
    estim$base    <- paste("base", base, sep = "")

    # removes infinite values to compute a linear model on the data
    estim_noinf <- estim[!(is.infinite(estim$esti)),]

    # fit a linear model on the data, and get the R^2 and the equation of the
    # model to print those on the graphics
    mod <- lm(estimate ~ real, data = estim_noinf)
    estim$r2 <- summary(mod)$adj.r.squared
    mod$coef <- round(mod$coef, 3)
    estim$eqn <- paste("Y = ", mod$coef[2], "X + ", mod$coef[1], sep = "")

    dat <- rbind.data.frame(dat, estim)
  }
}
dat_noinf <- dat[!(is.infinite(dat$esti)),]

# aggregate labels to be displayed on the plot
f_labels <- aggregate(data.frame(r2 = dat$r2, eqn = dat$eqn),
                      list(periode = dat$periode, base = dat$base),
                      unique)
f_labels$r2 <- paste("R2=", round(f_labels$r2, 3), sep = "")

# plot dat!
p <- ggplot(dat, aes(x = real, y = estimate)) +
  geom_point(size = 1) +
  geom_abline(slope = 1, intercept = 0) +
  xlim(0, 40) +
  ylim(0, 40) +
  xlab("Real values") +
  ylab("Estimates") +
  facet_grid(periode ~ base) +
  geom_text(x = 32, y = 20, aes(label = eqn), data = f_labels,
            colour = "red", fontface = "bold") +
  geom_text(x = 32, y = 15, aes(label = r2), data = f_labels,
            colour = "red", fontface = "bold")

# plots that are not good
notgood <- dat[dat$estimate > 300 & dat$base == "base2" & dat$periode == "period2",
               -seq(from = length(dat), to = length(dat) - 4, by = -1)]

save.image('/tmp/data_verif.RData')


# ------------------------------------------------------------------------------
# with complete abundance

flore <- read.csv("data/raw/flore_tot_per1.csv", encoding = "latin1", sep = ";",
                  stringsAsFactors = FALSE)

# remove the "Transect" quadrat
flore2013 <- flore[flore$Annee == 2013 & flore$Quadrat != "Transect", ]

# remove all NAs lines
flore2013 <-
  flore2013[which(apply(flore2013[, 16:ncol(flore2013)], 1, function(x) sum(is.na(x)))
                  != ncol(flore2013) - 15),]

# transpose and convert the dataframe
transposed <- transpose_flora_tot(flore2013)

## test correlation with geometric mean
gmean <- transposed[["base10"]][, -(1:4)]
gmean[gmean == 2] <- exp(mean(log(c(2, 9))))
gmean[gmean == 3] <- exp(mean(log(c(10, 99))))
gmean[gmean == 4] <- exp(mean(log(c(100, 999))))
gmean[gmean == 5] <- exp(mean(log(c(1000, 9999))))

cor_gmean <- estim_summary_tot_gm(tab = transposed[["orig"]], gmean, surf = 1)

## test on base2
# compute estimtions
estim2 <- estim_abundance(x = transposed[["base2"]], surf = 1, n_cores = 4, addpos = FALSE)

cor_base2 <- estim_summary_tot(transposed[["orig"]], estim2, surf = 1)

save.image('/tmp/data_verif.RData')
