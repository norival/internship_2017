# -- packages and functions ----------------------------------------------------
library(ggplot2)
library(magrittr)
source("functions/verif_ab.R", encoding = "utf8")
source("functions/abundance.R", encoding = "utf8")


# -- data ----------------------------------------------------------------------
flore <- read.csv("data/raw/flore_tot_per1.csv", encoding = "latin1", sep = ";",
                  stringsAsFactors = FALSE)

# remove the "Transect" quadrat
flore2013 <- flore[flore$Annee == 2013 & flore$Quadrat != "Transect", ]


# -- check estimations ---------------------------------------------------------
# Check the estimates against the real values
# The values are converted whether in base 0 (0/1) or base2 or base10 and then
# the abundance is estimated like for the other samples.
surf <- 1

# transpose and convert the dataframe
transposed <- transpose_flora_tot(flore2013)


# ------------------------------------------------------------------------------
# all estimations methods

## test correlation with geometric mean
gmean <- transposed[["base10"]][, -(1:4)]
gmean[gmean == 2] <- exp(mean(log(c(2, 9))))
gmean[gmean == 3] <- exp(mean(log(c(10, 99))))
gmean[gmean == 4] <- exp(mean(log(c(100, 999))))
gmean[gmean == 5] <- exp(mean(log(c(1000, 9999))))

cor_gmean <- estim_summary_gm(tab = transposed[["orig"]], gmean, surf = 1)

## estimate with Poisson distribution
estim01 <- estim_abundance01(x = transposed[["base0"]], surf = 1, addpos = FALSE)
cor_base0 <- estim_summary(transposed[["orig"]], estim01, surf = 1)

## estimate with COM-Poisson distribution
estim2 <- estim_abundance(x = transposed[["base2"]], surf = 1, n_cores = 4, addpos = FALSE)
cor_base2 <- estim_summary(transposed[["orig"]], estim2, surf = 1)

## estimate with negative binomiale distribution
# The distribution of plants within plots follows a Poisson distribution whith
# lambda paramater being a random parameter drawn from a gamme distribution.
# Then we are looking for P(k|lambda), k being the number of plants obsevred in
# the quadrate and lambda the random parameter drawn from the gamma
# distribution.
gpoisson <- estim_abundance(x = transposed[["base2"]], surf = 1, n_cores = 3,
                            fun = "gammapoisson", addpos = FALSE, maxtheta = 20)
cor_gpoisson <- estim_summary(transposed[["orig"]], gpoisson, surf = 1)


# ------------------------------------------------------------------------------
# check distribution of abundances

orig <- transposed[["orig"]]
allsp <- data.frame(character(), numeric())
for (sp in unique(orig$sp)) {
  a <-
    cbind.data.frame(sp, as.numeric(as.matrix(orig[orig$sp == sp, 4:length(orig)])))
  allsp <- rbind.data.frame(allsp, a)
}
colnames(allsp) <- c("sp", "ab")
allsp$sp <- as.character(allsp$sp)

p <-
  allsp[allsp$sp %in% unique(allsp$sp)[1:16],] %>%
  ggplot(aes(ab)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ sp)
# ggsave("~/desktop/distributions_abondances.png")
# write.csv(allsp, "~/desktop/distributions_abondances.csv")

a <- colSums(orig[, 4:length(orig)])
p <-
  allsp[allsp$sp %in% sample(unique(allsp$sp), 20), ] %>%
  ggplot(aes(ab)) +
  geom_histogram(bins = 15) +
  facet_wrap(~ sp)


# ------------------------------------------------------------------------------
# summary table
aa <-
  rbind.data.frame(cbind.data.frame(base = "base0", cor_base0[,1:4]),
                   cbind.data.frame(base = "base2", cor_base2[,1:4]),
                   cbind.data.frame(base = "geom",  cor_gmean[,1:4]),
                   cbind.data.frame(base = "binom", cor_gpoisson[,1:4]))


# ------------------------------------------------------------------------------
# bootstraps on models coefficients
# the models to check estimations are bootstraped
nboot <- 10000

# bootstraps on models
## the regression must be done on log-log values so the variance is homogeneous,
## we convert the values first
lcor_base0 <-
  cbind.data.frame(real = log(cor_base0$real), estimate = log(cor_base0$estimate))
bootbase0 <- bootstrap(nboot, lcor_base0)

lcor_gmean <-
  cbind.data.frame(real = log(cor_gmean$real), estimate = log(cor_gmean$estimate))
bootgmean <- bootstrap(nboot, lcor_gmean)

lcor_cpoisson <-
  cbind.data.frame(real = log(cor_base2$real), estimate = log(cor_base2$estimate))
bootcpoisson <- bootstrap(nboot, lcor_cpoisson)

lcor_gpoisson <-
  cbind.data.frame(real = log(cor_gpoisson$real), estimate = log(cor_gpoisson$estimate))
bootgpoisson <- bootstrap(nboot, lcor_gpoisson)

# compute predictd values with IC95
predbootbase0    <- bootpred(0:30, bootbase0)
predbootgmean    <- bootpred(0:30, bootgmean)
predbootcpoisson <- bootpred(0:30, bootcpoisson)
predbootgpoisson <- bootpred(0:30, bootgpoisson)

tab_boot <-
  rbind.data.frame(predbootbase0, predbootgmean, predbootcpoisson, predbootgpoisson)
tab_boot$estimation <-
  rep(c("0/1", "gmean", "base2", "gpoisson"), rep(nrow(predbootgmean), 4))

# summary table for bootstraps
bootsum <-
  cbind.data.frame(estim   = c("Poisson", "Moyenne géométrique", "COM-Poisson", "Gamma-Poisson"),
                   r_icinf = rbind(quantile(bootbase0$r.squared,    0.025),
                                   quantile(bootgmean$r.squared,    0.025),
                                   quantile(bootcpoisson$r.squared, 0.025),
                                   quantile(bootgpoisson$r.squared, 0.025)),
                   r_mean  = rbind(mean(bootbase0$r.squared),
                                   mean(bootgmean$r.squared),
                                   mean(bootcpoisson$r.squared),
                                   mean(bootgpoisson$r.squared)),
                   r_icinf = rbind(quantile(bootbase0$r.squared,    0.975),
                                   quantile(bootgmean$r.squared,    0.975),
                                   quantile(bootcpoisson$r.squared, 0.975),
                                   quantile(bootgpoisson$r.squared, 0.975)))
colnames(bootsum) <- c("Estimation", "R2 Inf", "R2 moy", "R2 sup")

save.image('data/generated/data_verif.RData')


# ------------------------------------------------------------------------------
# optimisation of minimisation parameters
# this is commented because it is quite long to run...

# test different values of maxthetat for the optimisation function. maxtheta is
# the threshold value from which estimated parameter is considered too high.
# maxtheta <- 15:35

# results <- optim_maxtheta(transposed, maxtheta, fun = "gammapoisson", surf = 1,
#                           nboot = 2500)

# some plots to check that
# a <- data.frame(x = numeric(), moy = numeric(), icinf = numeric(), icsup = numeric())
# for (i in 1:length(results)) {
#   a <- rbind.data.frame(a, results[[i]])
# }
# a$maxtheta <- as.character(rep(10:50, rep(nrow(results[[1]]), length(10:50))))

# a[a$maxtheta %in% c(15:25, 30),] %>%
#   ggplot(aes(x = x, y = moy, colour = maxtheta)) +
#   geom_line() +
#   geom_abline(intercept = 0, slope = 1) +
#   theme(axis.title   = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         axis.text  = element_text(size = 14),
#         legend.text  = element_text(size = 14)) +
#   # geom_ribbon(aes(ymin = icinf, ymax = icsup, fill = maxtheta), alpha = 0.2) +
#   theme_bw()
# # ggsave("~/desktop/graphs/binom_maxtheta.png")

# a <- rbind.data.frame(cor_gpoisson, cor_gpoisson30)
# a$maxtheta <- as.character(rep(c(20, 30), rep(nrow(cor_gpoisson), 2)))

# ggplot(a, aes(x = estimate, y = real, colour = maxtheta)) +
#   geom_point(size = 2, shape = 1, position = "jitter") +
#   geom_abline(intercept = 0, slope = 1) +
#   theme(axis.title   = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         axis.text  = element_text(size = 14),
#         legend.text  = element_text(size = 14))
# # ggsave("~/desktop/graphs/binom_nolog.png")

# ggplot(a, aes(x = log(estimate), y = log(real), colour = maxtheta)) +
#   geom_point(size = 2, shape = 1, position = "jitter") +
#   geom_abline(intercept = 0, slope = 1) +
#   theme(axis.title   = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         axis.text  = element_text(size = 14),
#         legend.text  = element_text(size = 14))
# # ggsave("~/desktop/graphs/binom_log.png")
