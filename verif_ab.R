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
# flore2013 <-
#   flore2013[which(apply(flore2013[, 16:ncol(flore2013)], 1, function(x) sum(is.na(x)))
#                   != ncol(flore2013) - 15),]


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

estim01 <- estim_abundance01(x = transposed[["base0"]], surf = 1, addpos = FALSE)

cor_base0 <- estim_summary(transposed[["orig"]], estim01, surf = 1)

cor_base0 %>%
  ggplot(aes(x = real, y = estimate)) +
  geom_point(size = 2, shape = 1) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(0, 30)) +
  xlab("Densité réelle") +
  ylab("Estimation sur base10") +
  geom_abline(slope = 1, intercept = 0)

# ------------------------------------------------------------------------------
# empirical

a <- as.numeric(as.matrix(flore2013[, 16:length(flore2013)]))
b0 <- a
b0[b0 > 1] <- 1
b2 <- a
b2[b2 > 2] <- 2
b10 <- a
b10[b10 >= 2 &    b10 < 10]     <- 2
b10[b10 >= 10 &   b10 < 100]    <- 3
b10[b10 >= 100 &  b10 < 1000]   <- 4
b10[b10 >= 1000 & b10 < 10000]  <- 5
plot(b10, log(a))
plot(b2, a)

save.image('/tmp/data_verif.RData')

# ------------------------------------------------------------------------------
# distribution of abundances

orig <- transposed[["orig"]]
allsp <- data.frame(character(), numeric())
for (sp in unique(orig$sp)) {
  a <-
    cbind.data.frame(sp, as.numeric(as.matrix(orig[orig$sp == sp, 4:length(orig)])))
  allsp <- rbind.data.frame(allsp, a)
}
colnames(allsp) <- c("sp", "ab")
allsp$sp <- as.character(allsp$sp)

allsp[allsp$sp %in% unique(allsp$sp)[1:16],] %>%
  ggplot(aes(ab)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ sp)
ggsave("~/desktop/distributions_abondances.png")
write.csv(allsp, "~/desktop/distributions_abondances.csv")

# ------------------------------------------------------------------------------
# graphs pour présentation cesab

allsp0 <- allsp
allsp0$ab[allsp0$ab > 1] <- 1
allsp2 <- allsp
allsp2$ab[allsp2$ab > 2] <- 2
allsp10 <- allsp
allsp10$ab[allsp10$ab >= 2 &    allsp10$ab < 10]     <- 2
allsp10$ab[allsp10$ab >= 10 &   allsp10$ab < 100]    <- 3
allsp10$ab[allsp10$ab >= 100 &  allsp10$ab < 1000]   <- 4
allsp10$ab[allsp10$ab >= 1000 & allsp10$ab < 10000]  <- 5
a <- cbind.data.frame(allsp$sp, allsp$ab, allsp0$ab, allsp2$ab, allsp10$ab)
colnames(a) <- c("sp", "ab", "ab0", "ab2", "ab10")
a$sp <- as.character(a$sp)
suma <- matrix(ncol = 4, nrow = 0)
for (sp in unique(a$sp)) {
  datsp <- a[a$sp == sp,]
  b <- cbind(mean(datsp$ab), mean(datsp$ab0),
             mean(datsp$ab2), mean(datsp$ab10))
  suma <- rbind(suma, b)
}

bb <- as.data.frame(matrix(ncol = 3, nrow = 0))
for (i in 2:ncol(suma)) {
  b <- cbind.data.frame(paste0("ab", i), suma[, 1], suma[, i])
  bb <- rbind.data.frame(bb, b)
}
colnames(bb) <- c("base", "real", "meanab")
ggplot(bb, aes(x = log(real), y = log(meanab), colour = base)) +
  geom_point(size = 2) +
  # geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  xlab("log(Abondance réelle)") +
  ylab("log(Note moyenne)") +
  labs(colour = "Dégradation") +
  scale_color_discrete(labels = c("0/1", "base2", "base10")) +
  theme_bw()
ggsave("~/desktop/graphs/note_moyenne_vs_abondance.png")

# ------------------------------------------------------------------------------

aa <-
  rbind.data.frame(cbind.data.frame(base = "base0", cor_base0[,1:4]),
                   cbind.data.frame(base = "base2", cor_base2[,1:4]),
                   cbind.data.frame(base = "geom", cor_gmean[,1:4]))
ggplot(aa, aes(x = log(estimate), y = log(real), colour = base)) +
  geom_point(size = 2, shape = 1, position = "jitter") +
  geom_abline(slope = 1, intercept = 0) +
  labs(colour = "Estimation") +
  xlab("log(Estimation)") +
  ylab("log(Abondance réelle)") +
  scale_color_discrete(labels = c("0/1", "base2", "Géometrique")) +
  theme_bw()
ggsave("~/desktop/graphs/estimation_vs_abondance.png")
