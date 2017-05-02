library(ggplot2)
library(magrittr)
library(tikzDevice)

load('data/generated/data_verif.RData')

prefix <- "~/documents/master/m2/stage_report/img/"
paf <- function(string) paste0(prefix, string)

# colorblind friendly palette
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")

# ------------------------------------------------------------------------------
# verification abundances

aa <-
  rbind.data.frame(cbind.data.frame(method = "Loi de Poisson", cor_base0[,1:4]),
                   cbind.data.frame(method = "Loi de COM-Poisson", cor_base2[,1:4]),
                   cbind.data.frame(method = "Moyenne Géométrique",  cor_gmean[,1:4]),
                   cbind.data.frame(method = "Loi Binomiale Négative", cor_gpoisson[,1:4]))

aa$estim <- "Bon"
aa$diff <- aa$estimate - aa$real
aa$estim[aa$diff > 1] <- "Sur-estimé"
aa$estim[aa$diff < -1] <- "Sous-estimé"

p <- ggplot(aa, aes(x = log(estimate), y = log(real), colour = estim)) +
# p <- ggplot(aa, aes(x = estimate, y = real, colour = estim)) +
  geom_point(size = 1.2, shape = 1, position = "jitter") +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~ method, scales = "fixed") +
  xlab("log(Abondance estimation)") +
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

pdf(paf("estimations_log.pdf"), height = 4, width = 6)
plot(p)
dev.off()

# ------------------------------------------------------------------------------
# bootstraps

tab_boot$estimation <- as.factor(tab_boot$estimation)
levels(tab_boot$estimation) <- unique(aa$method)

p <- ggplot(tab_boot, aes(x = x, y = moy, fill = estimation)) +
  geom_line(aes(colour = estimation), size = 1) +
  geom_ribbon(aes(ymin = icinf, ymax = icsup), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, col = "red", size = 0.7,
              linetype = "dashed") +
  xlim(c(0, 30)) +
  coord_cartesian(ylim = c(0, 35)) +
  xlab("Abondance estimée") +
  ylab("Abondance observée") +
  labs(fill = "Méthode") +
  labs(colour = "Méthode") +
  theme_bw() +
  theme(axis.title = element_text(size = rel(1.2))) +
  theme(axis.text = element_text(size = rel(1))) +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette)
  # scale_colour_manual(values = c("darkgreen", "cyan4", "greenyellow", "lightseagreen")) +
  # scale_fill_manual(values = c("darkgreen", "cyan4", "greenyellow", "lightseagreen"))

pdf(paf("bootstrap.pdf"), height = 3.4, width = 5.4)
plot(p)
dev.off()
