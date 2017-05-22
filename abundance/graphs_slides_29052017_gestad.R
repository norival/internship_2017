# ------------------------------------------------------------------------------
# file        : graphs_slides_29052017_gestad.R
# description : generates graphics for gestad presentation
# ------------------------------------------------------------------------------

# -- packages ------------------------------------------------------------------
library(ggplot2)
library(compoisson)
library(magrittr)
library(tikzDevice)

# little function to create the names of files
prefixs <- "../../slides/29052017_gestad/img/"
pafs <- function(string) paste0(prefixs, string)

# colorblind friendly palette
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")

# load data
load("data/generated/data_verif.RData")


# -- estimation ----------------------------------------------------------------

aa <-
  rbind.data.frame(cbind.data.frame(method = "Poisson", cor_poisson[,1:4]),
                   cbind.data.frame(method = "COM-Poisson", cor_cpoisson[,1:4]),
                   cbind.data.frame(method = "Moyenne géométrique",  cor_gmean[,1:4]),
                   cbind.data.frame(method = "Binomiale négative", cor_gpoisson[,1:4]))

aa$estim <- "Bon"
aa$diff <- aa$estimate - aa$observed
aa$estim[aa$diff > 40] <- "Sur-estimé"
aa$estim[aa$diff < -40] <- "Sous-estimé"

p <- ggplot(aa, aes(x = log(estimate), y = log(observed), colour = estim)) +
  geom_point(size = 1.2, shape = 1, position = "jitter") +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~ method, scales = "fixed") +
  xlab("log(Abondance estimée)") +
  ylab("log(Abondance observée)") +
  theme_bw() +
  labs(colour = "Estimation") +
  guides(colour = guide_legend(override.aes = list(size = 3, shape = 19))) +
  theme(strip.background = element_rect(fill = "white", size = rel(1))) +
  theme(strip.text = element_text(size = rel(1.1))) +
  theme(axis.title = element_text(size = rel(1.2))) +
  theme(axis.text = element_text(size = rel(1))) +
  scale_colour_manual(values = cb_palette)
  # scale_colour_manual(values = c("darkgreen", "cyan4", "darkred"))

pdf(pafs("estimations_log.pdf"), height = 4, width = 6)
plot(p)
dev.off()

# -- estimations ---------------------------------------------------------------
# scatter plot
aa$method  <-  as.character(aa$method)

for (method in unique(aa$method)) {
  filename <- switch(method,
                     "Poisson" = "poisson",
                     "Moyenne géométrique" = "gmean",
                     "COM-Poisson" = "cpoisson",
                     "Binomiale négative" = "nbino")
  filename <- paste0("estimations_", filename, ".pdf")

  pdf(pafs(filename), height = 3, width = 4)

  p <-
    aa[aa$method == method,] %>%
    ggplot(aes(x = log(estimate), y = log(observed), colour = estim)) +
    geom_point(size = 1.2, shape = 1, position = "jitter") +
    geom_abline(slope = 1, intercept = 0) +
    xlab("log(Abondance estimée)") +
    ylab("log(Abondance observée)") +
    theme_bw() +
    labs(colour = "Estimation") +
    guides(colour = guide_legend(override.aes = list(size = 3, shape = 19))) +
    theme(axis.title = element_text(size = rel(1.2))) +
    theme(axis.text = element_text(size = rel(1))) +
    scale_colour_manual(values = cb_palette)

  plot(p)
  dev.off()
}

# bootstrap
for (method in unique(tab_boot$estimation)) {
  filename <- switch(method,
                     "Loi de Poisson" = "poisson",
                     "Moyenne Géométrique" = "gmean",
                     "Loi de COM-Poisson" = "cpoisson",
                     "Loi Binomiale Négative" = "nbino")
  filename <- paste0("bootstrap_", filename, ".pdf")

  pdf(pafs(filename), height = 3, width = 3)

  p <-
    tab_boot[tab_boot$estimation == method,] %>%
    ggplot(aes(x = x, y = moy)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = icinf, ymax = icsup), alpha = 0.3) +
    geom_abline(slope = 1, intercept = 0, col = "red", size = 0.7,
                linetype = "dashed") +
    xlim(c(0, 1000)) +
    coord_cartesian(ylim = c(0, 1000)) +
    xlab("Abondance estimée") +
    ylab("Abondance observée") +
    theme_bw() +
    theme(axis.title = element_text(size = rel(1.2))) +
    theme(axis.text = element_text(size = rel(1)))

  plot(p)
  dev.off()
}

# -- bootstraps ----------------------------------------------------------------

p <-
  ggplot(tab_boot, aes(x = x, y = moy, fill = estimation)) +
  geom_line(aes(colour = estimation), size = 1) +
  geom_ribbon(aes(ymin = icinf, ymax = icsup), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, col = "red", size = 0.7,
              linetype = "dashed") +
  xlim(c(0, 1000)) +
  coord_cartesian(ylim = c(0, 1000)) +
  xlab("Estimated density") +
  ylab("Observed density") +
  labs(fill = "Method") +
  labs(colour = "Method") +
  theme_bw() +
  theme(axis.title = element_text(size = rel(1.2))) +
  theme(axis.text = element_text(size = rel(1))) +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette)
  # scale_colour_manual(values = c("darkgreen", "cyan4", "greenyellow", "lightseagreen")) +
  # scale_fill_manual(values = c("darkgreen", "cyan4", "greenyellow", "lightseagreen"))

pdf(pafs("bootstrap_en.pdf"), height = 3.4, width = 5.4)
plot(p)
dev.off()


# -- Binomial negative estimation bootstrap ------------------------------------

p <-
  tab_boot[tab_boot$estimation ==  "Negative binomiale",] %>%
  ggplot(aes(x = x, y = moy)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = icinf, ymax = icsup), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, col = "red", size = 0.7,
              linetype = "dashed") +
  xlim(c(0, 30)) +
  coord_cartesian(ylim = c(0, 35)) +
  xlab("Estimated density") +
  ylab("Observed density") +
  theme_bw() +
  theme(axis.title = element_text(size = rel(1.2))) +
  theme(axis.text = element_text(size = rel(1))) +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette)

pdf(pafs("bootstrap_gpoiss.pdf"), height = 3.4, width = 4.4)
plot(p)
dev.off()


# -- litle graphs to show distributions shapes ---------------------------------
gpoisson <- function(k, r, theta) {
  p <- 1 / (theta + 1)
  q <- 1 - p

  pk <-
    gamma(k + r) / (gamma(r) * factorial(k)) * p^r * q^k

  return(pk)
}

dat <- data.frame(x = 0:10)
dat$y <- dpois(dat$x, lambda = 3)

pdf(pafs("poisson.pdf"), height = 0.9, width = 0.9)
ggplot(dat, aes(x = x, y = y)) +
  geom_point(size = 0.6) +
  theme_bw() +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(panel.grid = element_blank())
dev.off()

dat <- data.frame(x = 0:9)
dat$y <- exp(com.log.density(dat$x, lambda = 3, nu = 2))

pdf(pafs("compoisson.pdf"), height = 0.9, width = 0.9)
ggplot(dat, aes(x = x, y = y)) +
  geom_point(size = 0.6) +
  theme_bw() +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(panel.grid = element_blank())
dev.off()

dat <- data.frame(x = 0:10)
dat$y <- gpoisson(dat$x, r = 8, theta = 0.3)

pdf(pafs("negbino.pdf"), height = 0.9, width = 0.9)
ggplot(dat, aes(x = x, y = y)) +
  geom_point(size = 0.6) +
  theme_bw() +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(panel.grid = element_blank())
dev.off()
