# -- packages and functions ----------------------------------------------------
library(ggplot2)
library(magrittr)
library(dplyr)
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
gmean <- transposed[["base10"]][, -(1:3)]
gmean[gmean == 1] <- exp(mean(log(c(1, 9))))
gmean[gmean == 2] <- exp(mean(log(c(10, 99))))
gmean[gmean == 3] <- exp(mean(log(c(100, 999))))
gmean[gmean == 4] <- exp(mean(log(c(1000, 9999))))
gmean[gmean == 5] <- exp(mean(log(c(10000, 99999))))

cor_gmean <- estim_summary_gm(tab = transposed[["orig"]], gmean, surf = 1)

## estimate with Poisson distribution
estim_poisson <- estim_abundance(x = transposed[["base0"]],
                                 surf = 1, fun = "poisson")
cor_poisson <- estim_summary(transposed[["orig"]], estim_poisson, surf = 1)

## estimate with COM-Poisson distribution
estim_cpoisson <- estim_abundance(x = transposed[["base2"]],
                                  surf = 1, fun = "compoisson")
cor_cpoisson <- estim_summary(transposed[["orig"]], estim_cpoisson, surf = 1)

## estimate with negative binomiale distribution
estim_gpoisson <- estim_abundance(x = transposed[["base2"]],
                                  surf = 1, fun = "gammapoisson", maxtheta = 20)
cor_gpoisson <- estim_summary(transposed[["orig"]], estim_gpoisson, surf = 1)


# ------------------------------------------------------------------------------
# summary table
cor_summary <-
  rbind.data.frame(cbind.data.frame(base = "base0", cor_poisson),
                   cbind.data.frame(base = "base2", cor_cpoisson),
                   cbind.data.frame(base = "geom",  cor_gmean),
                   cbind.data.frame(base = "binom", cor_gpoisson))


# ------------------------------------------------------------------------------
# bootstraps on models coefficients
# the models to check estimations are bootstraped
nboot <- 10000

# bootstraps on models
## the regression must be done on log-log values so the variance is homogeneous,
## we convert the values first
lcor_poisson <- cbind.data.frame(observed = log(cor_poisson$observed),
                                 estimate = log(cor_poisson$estimate))
bootpoisson <- bootstrap(nboot, lcor_poisson[!is.infinite(lcor_poisson$observed) &
                         !is.infinite(lcor_poisson$estimate),])

lcor_gmean <- cbind.data.frame(observed = log(cor_gmean$observed),
                               estimate = log(cor_gmean$estimate))
bootgmean <- bootstrap(nboot, lcor_gmean[!is.infinite(lcor_gmean$observed),])

lcor_cpoisson <- cbind.data.frame(observed = log(cor_cpoisson$observed),
                                  estimate = log(cor_cpoisson$estimate))
bootcpoisson <- bootstrap(nboot, lcor_cpoisson[!is.infinite(lcor_cpoisson$observed),])

lcor_gpoisson <- cbind.data.frame(observed = log(cor_gpoisson$observed),
                                  estimate = log(cor_gpoisson$estimate))
bootgpoisson <- bootstrap(nboot, lcor_gpoisson[!is.infinite(lcor_gpoisson$observed),])

# compute predictd values with IC95
predbootpoisson  <- bootpred(log(1:1000), bootpoisson)
predbootgmean    <- bootpred(log(1:1000), bootgmean)
predbootcpoisson <- bootpred(log(1:1000), bootcpoisson)
predbootgpoisson <- bootpred(log(1:1000), bootgpoisson)

tab_boot <-
  rbind.data.frame(predbootgmean, predbootpoisson, predbootcpoisson, predbootgpoisson)
tab_boot$estimation <-
  rep(c("Moyenne Géométrique", "Loi de Poisson", "Loi de COM-Poisson",
        "Loi Binomiale Négative"), rep(nrow(predbootgmean), 4))

# summary table for bootstraps
bootsum <-
  cbind.data.frame(estim   = c("Poisson", "Moyenne géométrique", "COM-Poisson", "Gamma-Poisson"),
                   r_icinf = rbind(quantile(bootpoisson$r.squared,    0.025),
                                   quantile(bootgmean$r.squared,    0.025),
                                   quantile(bootcpoisson$r.squared, 0.025),
                                   quantile(bootgpoisson$r.squared, 0.025)),
                   r_mean  = rbind(mean(bootpoisson$r.squared),
                                   mean(bootgmean$r.squared),
                                   mean(bootcpoisson$r.squared),
                                   mean(bootgpoisson$r.squared)),
                   r_icinf = rbind(quantile(bootpoisson$r.squared,    0.975),
                                   quantile(bootgmean$r.squared,    0.975),
                                   quantile(bootcpoisson$r.squared, 0.975),
                                   quantile(bootgpoisson$r.squared, 0.975)))
colnames(bootsum) <- c("Estimation", "R2 Inf", "R2 moy", "R2 sup")

mod_gpoisson_sum <-
  data.frame(val = c("mean", "inf", "sup"),
             r = c(mean(bootgpoisson$r.squared),
                   quantile(bootgpoisson$r.squared, 0.025),
                   quantile(bootgpoisson$r.squared, 0.975)),
             int = c(mean(bootgpoisson$interc),
                     quantile(bootgpoisson$interc, 0.025),
                     quantile(bootgpoisson$interc, 0.975)),

             est = c(mean(bootgpoisson$estimate),
                     quantile(bootgpoisson$estimate, 0.025),
                     quantile(bootgpoisson$estimate, 0.975)))


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

# ------------------------------------------------------------------------------
# Check the minimum number of quadra to have a good estimation

# boot_quadras <- min_quadras(transposed[["orig"]], min = 10, nboot = 2500, n_cores = 3)
# write.csv(a, "data/generated/smoothed_bootstraps.csv")

boot_quadras <- read.csv("data/generated/smoothed_bootstraps.csv")

tab <- rbind.data.frame(cor_gmean, cor_poisson, cor_cpoisson, cor_gpoisson)
tab$method <- rep(c("Moyenne géométrique", "Loi de Poisson", "Loi de COM-Poisson",
                    "Loi Binomiale Négative"), rep(nrow(tab) / 4, 4))

tabsum <-
  tab %>%
  mutate(error = estimate - observed) %>%
  group_by(method) %>%
  summarise(sous = sum(error >= 40) / length(error) * 100,
            sur  = sum(error <= -40) / length(error) * 100,
            errmoy = mean(error[!is.infinite(error)], na.rm = TRUE),
            errinf = quantile(error, 0.025),
            errsup = quantile(error, 0.975),
            errmax = max(error),
            errmin = min(error))
# print(xtable::xtable(tabsum, digits = 2), include.rownames = FALSE)

save.image('data/generated/data_verif.RData')