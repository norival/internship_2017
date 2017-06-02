# ------------------------------------------------------------------------------
# file        : graphs_report.R
# description : generates graphics for report
# ------------------------------------------------------------------------------

# -- packages ------------------------------------------------------------------
library(ggplot2)
library(magrittr)
library(tikzDevice)

# little function to create the names of files
prefix  <- "~/documents/master/m2/stage/report/img/"
paf <- function(string) paste0(prefix, string)

# colorblind friendly palette
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")


# ------------------------------------------------------------------------------
# graphics for report
# ------------------------------------------------------------------------------

# most likely group by year
p <- ggplot(mostlik_by_year, aes(x = year, y = mostlik, fill = prop)) +
  geom_raster() +
  # geom_text(aes(label = round(prop, 1)), colour = "white") +
  scale_fill_gradient2(low = muted("green"), mid = "orange", high = muted("red"),
                       midpoint = 50, limits = c(0, 100)) +
  scale_y_continuous(breaks = unique(mostlik_by_year$mostlik)) +
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

pdf(paf("mostlikgp_by_year.pdf"), height = 4, width = 3)
plot(p)
dev.off()

p <- ggplot(mostlik_by_crop, aes(x = crop, y = mostlik, fill = prop)) +
  geom_raster() +
  # geom_text(aes(label = round(prop, 1)), colour = "white") +
  scale_fill_gradient2(low = muted("green3"), mid = "orange", high = muted("red"),
                       midpoint = 50) +
  scale_y_continuous(breaks = unique(mostlik_by_crop$mostlik)) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1.5)) +
  xlab("Culture") +
  ylab("Groupe le plus probable") +
  theme(legend.position = "top") +
  labs(fill = "Pourcentage")

pdf(paf("mostlikgp_by_crop.pdf"), height = 4, width = 3)
plot(p)
dev.off()
