# -- packages and functions ----------------------------------------------------
library(tidyverse)
source("functions/transpose_flora.R", encoding = "utf8")
source("functions/abundance.R", encoding = "utf8")


# -- data ----------------------------------------------------------------------
flore <- read.csv("data/raw/flore_tot_per1.csv", encoding = "latin1", sep = ";",
                  stringsAsFactors = FALSE)

# remove the "Transect" quadrat
flore2013 <- flore[flore$Annee == 2013 & flore$Quadrat != "Transect", ]

# remove all NAs lines
flore2013 <-
  flore2013[which(apply(flore2013[, 16:ncol(flore2013)], 1, function(x) sum(is.na(x)))
                  != ncol(flore2013) - 15),]


# -- check estimations ---------------------------------------------------------
# Check the estimates against the real values
# The values are converted whether in base 0 (0/1) or base2 or base10 and then
# the abundance is estimated like for the other samples.
surf <- 1

# transpose and convert the dataframe
transposed <- transpose_flora_tot(flore2013)

## test correlation with geometric mean
gmean <- transposed[["base10"]][, -(1:4)]
gmean[gmean == 2] <- exp(mean(log(c(2, 9))))
gmean[gmean == 3] <- exp(mean(log(c(10, 99))))
gmean[gmean == 4] <- exp(mean(log(c(100, 999))))
gmean[gmean == 5] <- exp(mean(log(c(1000, 9999))))

cor_gmean <- estim_summary_gm(tab = transposed[["orig"]], gmean, surf = 1)

## test on base2
# compute estimtions
estim2 <- estim_abundance(x = transposed[["base2"]], surf = 1, n_cores = 4, addpos = FALSE)

cor_base2 <- estim_summary(transposed[["orig"]], estim2, surf = 1)

# ------------------------------------------------------------------------------

orig  <- transposed[["orig"]]
base2 <- transposed[["base2"]]
bad21 <- cor_base2[abs(cor_base2$estimate - cor_base2$real) > 5,]
badid <- paste0(bad21$sp, bad21$parc)
id    <- paste0(orig$sp, orig$carre.parc)
bad2 <- base2[id %in% badid, -3]
n2 <- apply(bad2[,3:length(bad2)], 1, function(x) sum(x == 2))
n1 <- apply(bad2[,3:length(bad2)], 1, function(x) sum(x == 1))
n0 <- apply(bad2[,3:length(bad2)], 1, function(x) sum(x == 0))
bad2sum <- cbind.data.frame(bad2[, 1:2], real = bad21$real, estimate =
                            bad21$estimate, n0, n1, n2)
badorig <- orig[id %in% badid, -3]
write.csv(badorig, "~/desktop/data_stage2/badplots.csv", row.names = FALSE)
write.csv(bad2sum, "~/desktop/data_stage2/badsum.csv", row.names = FALSE)

bb <- cbind()
for (i in 1:nrow(badorig)) {
  b <- cbind(i, as.numeric(badorig[i, 3:length(badorig)]))
  bb <- rbind(bb, b)
}
bb <- as.data.frame(bb)
colnames(bb) <- c("parc", "ab")

# ------------------------------------------------------------------------------

estim10 <- estim_abundance01(x = transposed[["base10"]], surf = 1, addpos = FALSE)

cor_base10 <- estim_summary(transposed[["orig"]], estim10, surf = 1)

cor_base10 %>%
  ggplot(aes(x = real, y = estimate)) +
  geom_point(size = 2, shape = 1) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(0, 30)) +
  xlab("Densité réelle") +
  ylab("Estimation sur base10") +
  geom_abline(slope = 1, intercept = 0)

save.image('/tmp/data_verif.RData')
