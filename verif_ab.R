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

dat <- as.data.frame(matrix(nrow = 0, ncol = 6, 0))
for (per in 1:3) {
  # per <- 1
  # base <- 0
  cat("Computing period ", per, "...\n", sep = "")
  tab <- flore[flore$Periode == per,]

  for (base in c(0, 2)) {
    cat(" Computing base ", base, "...\n", sep = "")
    tabt <- transpose_flora(tab, base = base)
    origin <- tabt[["origin"]]
    conv   <- tabt[["converted"]]

    if (base == 0) {
      ab <- estim_abundance01(conv, surf, progress = FALSE)
    } else {
      ab <- estim_abundance(conv, surf, n_cores = 4, progress = FALSE)
    }

    estim <- estim_summary(origin, ab, surf)
    estim$periode <- paste("period", per, sep = "")
    estim$base    <- paste("base", base, sep = "")

    estim_noinf <- estim[!(is.infinite(estim$esti)),]
    mod <- lm(esti ~ real,
              data = estim_noinf[estim_noinf$periode == paste("period", per, sep = "") &
                                 estim_noinf$base == paste("base", base, sep = ""),])
    estim$r2 <- summary(mod)$adj.r.squared
    mod$coef <- round(mod$coef, 3)
    estim$eqn <- paste("Y = ", mod$coef[2], "X + ", mod$coef[1], sep = "")

    dat <- rbind.data.frame(dat, estim)
  }
}
dat_noinf <- dat[!(is.infinite(dat$esti)),]

f_labels <- aggregate(data.frame(r2 = dat$r2, eqn = dat$eqn),
                      list(periode = dat$periode, base = dat$base),
                      unique)
f_labels$r2 <- paste("R2=", round(f_labels$r2, 3), sep = "")

p <- ggplot(dat_noinf, aes(x = real, y = esti)) +
  geom_point(size = 2) +
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
