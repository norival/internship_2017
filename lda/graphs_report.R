# ------------------------------------------------------------------------------
# file        : graphs_report.R
# description : generates graphics for report
# ------------------------------------------------------------------------------

# -- packages ------------------------------------------------------------------
library(ggplot2)
library(magrittr)
library(tikzDevice)
library(xtable)

# little function to create the names of files
prefix  <- "~/documents/master/m2/stage/report/img/"
prefixt  <- "~/documents/master/m2/stage/report/tables/"
paf <- function(string) paste0(prefix, string)
paft <- function(string) paste0(prefixt, string)

# colorblind friendly palette
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")

load("data/generated/envir_lda.Rdata")

# ------------------------------------------------------------------------------
# graphics for report
# ------------------------------------------------------------------------------

# variations of delta(AIC)
p <-
  data.frame(k = 3:(length(aics_all_years_delta) + 2),
             delta = aics_all_years_delta) %>%
  ggplot(aes(x = k, y = delta)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Nombre de groupes") +
  ylab("delta(AIC)") +
  scale_x_continuous(breaks = 3:15) +
  theme_bw()

pdf(paf("delta_aic.pdf"), height = 3, width = 6)
plot(p)
dev.off()

# variations of delta(perplexity)
p <-
  data.frame(k = sort(unique(cv_results$k))[-1], delta = cv_delta) %>%
  .[.$k < 40,] %>%
  ggplot(aes(x = k, y = delta)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Nombre de groupes") +
  ylab("delta(perpexlité)") +
  scale_x_continuous(breaks = c(3:30)) +
  theme_bw()

pdf(paf("delta_perp.pdf"), height = 3, width = 6)
plot(p)
dev.off()

# 9 groups table
bold_names <- function(x) paste0("\\textbf{", x, "}")
print.xtable(xtable(ml_comp, align = "llX"),
             width = "\\textwidth",
             floating = FALSE,
             sanitize.colnames.function = bold_names,
             sanitize.text.function = identity,
             tabular.environment = "tabularx",
             include.rownames = FALSE,
             comment = FALSE,
             booktabs = TRUE,
             file = paft("comp_com.tex"))

# most likely group by year
mostlik_by_year$mostlik <- paste0("G", mostlik_by_year$mostlik)
p <- ggplot(mostlik_by_year, aes(x = year, y = mostlik, fill = prop)) +
  geom_raster() +
  # geom_text(aes(label = round(prop, 0)), colour = "blue", size = 3.5) +
  scale_fill_gradient(low = "grey95", high = "black", limits = c(0, 100)) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.5)) +
  guides(fill = FALSE) +
  xlab("Année") +
  ylab("Groupe le plus probable") +
  labs(fill = "Pourcentage")

pdf(paf("mostlikgp_by_year.pdf"), height = 3.5, width = 3)
plot(p)
dev.off()

p <- ggplot(mostlik_by_year, aes(x = mostlik, y = prop)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ year) +
  theme_bw()

pdf(paf("mostlikgp_by_year2.pdf"), height = 4.5, width = 6)
plot(p)
dev.off()

# most likely group by crop
mostlik_by_crop$mostlik <- paste0("G", mostlik_by_crop$mostlik)
p <- ggplot(mostlik_by_crop, aes(x = crop, y = mostlik, fill = prop)) +
  geom_raster() +
  # geom_text(aes(label = round(prop, 1)), colour = "white") +
  # scale_fill_gradient2(low = muted("green3"), mid = "orange", high = muted("red"),
  #                      midpoint = 50) +
  scale_fill_gradient(low = "grey95", high = "black") +
  # scale_y_continuous(breaks = unique(mostlik_by_crop$mostlik)) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.2)) +
  xlab("Culture") +
  ylab("Groupe le plus probable") +
  theme(legend.position = "top") +
  labs(fill = "Pourcentage")

pdf(paf("mostlikgp_by_crop.pdf"), height = 3.5, width = 3)
plot(p)
dev.off()
