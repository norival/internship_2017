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

# some renaming and translating
tidy_plots$crop[tidy_plots$crop == "ce/leg"]  <- "Cér./Lég."
tidy_plots$crop[tidy_plots$crop == "cereal"]  <- "Céréales"
tidy_plots$crop[tidy_plots$crop == "osr"]     <- "Colza"
tidy_plots$crop[tidy_plots$crop == "maize"]   <- "Maïs"
tidy_plots$crop[tidy_plots$crop == "pois"]    <- "Pois"
tidy_plots$crop[tidy_plots$crop == "sunflower"] <- "Tournesol"
tidy_plots$crop[tidy_plots$crop == "grassl"]    <- "Prairies"
tidy_plots$crop[tidy_plots$crop == "lucerne"]   <- "Luzerne"
tidy_plots$crop[tidy_plots$crop == "trefle"]    <- "Trèfle"
tidy_plots$crop[tidy_plots$crop == "lin/lentille"]  <- "Lin/Lentille"
tidy_plots$crop[tidy_plots$crop == "lin"]           <- "Lin"
tidy_plots$crop[tidy_plots$crop == "pavot"]         <- "Pavot"

mostlik_by_crop$crop <- as.character(mostlik_by_crop$crop)
mostlik_by_crop$crop[mostlik_by_crop$crop == "ce/leg"]  <- "Cér./Lég."
mostlik_by_crop$crop[mostlik_by_crop$crop == "cereal"]  <- "Céréales"
mostlik_by_crop$crop[mostlik_by_crop$crop == "osr"]     <- "Colza"
mostlik_by_crop$crop[mostlik_by_crop$crop == "maize"]   <- "Maïs"
mostlik_by_crop$crop[mostlik_by_crop$crop == "pois"]    <- "Pois"
mostlik_by_crop$crop[mostlik_by_crop$crop == "sunflower"] <- "Tournesol"
mostlik_by_crop$crop[mostlik_by_crop$crop == "grassl"]    <- "Prairies"
mostlik_by_crop$crop[mostlik_by_crop$crop == "lucerne"]   <- "Luzerne"
mostlik_by_crop$crop[mostlik_by_crop$crop == "trefle"]    <- "Trèfle"
mostlik_by_crop$crop[mostlik_by_crop$crop == "lin/lentille"]  <- "Lin/Lentille"
mostlik_by_crop$crop[mostlik_by_crop$crop == "lin"]           <- "Lin"
mostlik_by_crop$crop[mostlik_by_crop$crop == "pavot"]         <- "Pavot"


# distribution des cultures par année
tab <- tidy_plots[tidy_plots$group == 1,]
tt <- table(tab$crop, tab$year)
tt <- tt[order(rowSums(tt), decreasing = TRUE),]
tt <- rbind(tt, colSums(tt))
tt <- cbind(tt, rowSums(tt))
rownames(tt) <- c(rownames(tt)[1:(nrow(tt) - 1)], 'Total')
colnames(tt) <- c(colnames(tt)[1:(ncol(tt) - 1)], 'Total')
print.xtable(xtable(tt, digits = 0, align = c("l", rep("R", ncol(tt)))),
             hline.after = c(-1, 0, nrow(tt) - 1, nrow(tt)),
             tabular.environment = "tabularx",
             width = "\\textwidth",
             booktabs = TRUE,
             sanitize.colnames.function = bold_names,
             file = "../../report/tables/crops_dist.tex",
             floating = FALSE,
             comment = FALSE)

# variations of delta(AIC)
delta_aic <- data.frame(k = 3:(length(aics_all_years_delta) + 2),
                        delta = aics_all_years_delta)
p <-
  aics_all_years %>%
  ggplot(aes(x = k, y = aic)) +
  geom_point() +
  geom_line(data = delta_aic, aes(x = k, y = delta), colour = 'darkgray') +
  # geom_smooth(method = "loess", se = FALSE) +
  xlab("Nombre de groupes") +
  ylab("AIC") +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
  theme_bw()

pdf(paf("delta_aic.pdf"), height = 2.7, width = 3)
plot(p)
dev.off()

# variations of delta(perplexity)
delta_cv <- data.frame(k = sort(unique(cv_results$k))[-1], delta = cv_delta)
delta_cv <- delta_cv[delta_cv$k < 40,]
p <-
  cv_results %>%
  .[.$k < 40,] %>%
  ggplot(aes(x = k, y = perplexity)) +
  geom_point() +
  geom_line(data = delta_cv, aes(x = k, y = delta), colour = "darkgrey") +
  xlab("Nombre de groupes") +
  ylab("Perpexlité") +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
  theme_bw()

pdf(paf("delta_perp.pdf"), height = 2.7, width = 3)
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


# most likely group by crop
mostlik_by_crop$mostlik <- paste0("G", mostlik_by_crop$mostlik)
# mostlik_by_crop <- mostlik_by_crop[order(tt["Total"], decreasing = TRUE),]
mostlik_by_crop$crop <- factor(mostlik_by_crop$crop,
                               levels = rownames(tt))
# tt[1:(nrow(tt) - 1),"Total"]
p <- ggplot(mostlik_by_crop, aes(x = crop, y = mostlik, fill = prop)) +
  geom_raster() +
  scale_fill_gradient(low = "grey95", high = "black", limits = c(0, 100)) +
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
