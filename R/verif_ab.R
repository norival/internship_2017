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

tab <- flore[flore$Periode == 2,]
surf = 0.25

# Check when 0/1 notes
tab0        <- transpose_flora(tab, base = 0)
tab0_estim  <- estim_abundance01(tab0, surf = 0.25, progress = FALSE)
dat0        <- estim_summary(tab0, tab0_estim, surf)

ggplot(dat0, aes(x = real, y = esti)) +
  geom_point(size = 2) +
  xlim(0, 2) +
  ylim(0, 2) +
  geom_abline(slope = 1, intercept = 0)
ggsave("~/desktop/graphbase0.pdf")


# en base 2
tab2        <- transpose_flora(tab, base = 2)
tab2_estim  <- estim_abundance(tab2, surf = 0.25, n_cores = 4, progress = FALSE)
dat2        <- estim_summary(tab2, tab2_estim, surf)

ggplot(dat2, aes(x = real, y = esti)) +
  geom_point(size = 2) +
  xlim(0, 2) +
  ylim(0, 2) +
  geom_abline(slope = 1, intercept = 0)
ggsave("~/desktop/graphbase2.pdf")


dat02 <- dat0[!(is.infinite(dat0$esti)),]
dat22 <- dat2[!(is.infinite(dat0$esti)),]

cor.test(dat22$esti, dat22$real)
cor.test(dat02$esti, dat02$real)
