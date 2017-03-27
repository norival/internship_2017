# -- packages and functions ----------------------------------------------------

library(tidyverse)
source("functions/transpose_flora.R")
source("functions/abundance.R")


# -- data ----------------------------------------------------------------------
flore1 <- read.csv("data/raw/manip_ble_2014_p1.csv",
                   encoding = "utf8", stringsAsFactor = FALSE)
flore2 <- read.csv("data/raw/manip_ble_2014_p2.csv",
                   encoding = "utf8", stringsAsFactor = FALSE)
flore3 <- read.csv("data/raw/manip_ble_2014_p3.csv",
                   encoding = "utf8", stringsAsFactor = FALSE)

flore <- rbind(flore1, flore2, flore3)

# keep only when culture is present
# keep_lines <- (flore$Culture == "C1" &
#                # flore$Quadrat == "Q1" &
#                flore$Herbicide == "H1" &
#                flore$Periode == 2 &
#                flore$Azote == "N1")
# tab <- flore[keep_lines,]

# convert abundance indices by taking the geometric mean for each class
tab$Note_mean <- numeric(nrow(tab))
tab$Note_mean[tab$Note == 2] <- (2+9)/2
tab$Note_mean[tab$Note == 3] <- (10+99)/2
tab$Note_mean[tab$Note == 4] <- (100+999)/2
tab$Note_mean[tab$Note == 5] <- (1000+9999)/2

# compute densities
surf <- 0.25
summary(factor(tab$Zone))
tab_summary <-
  tab %>%
  mutate(density = Note_mean / surf) %>%
  group_by(Taxon) %>%
  summarise(density_mean = mean(Note_mean))

# estimate densities by different mean
# I need a table with species in lines

# get estimations for 2014
estim2014 <- read.csv("data/generated/abondt_per_subquadra_2014_binomiale.csv", row.names = 1)
# estim20142 <- read.csv("data/generated/abondt_per_quadra_2014_binomiale.csv", row.names = 1)
# estim20143 <- read.csv("data/generated/abondt_per_quadra_2014_base2.csv", row.names = 1)
# apply(estim20143, 2, mean)
source("functions/eppo_api.R")
token <- "f014e95874996d1da00c34723d98fdbf"

codes <- tab_summary$Taxon
sp <- eppo_api(codes, token)
sp <- gsub(" ", "\\.", sp)
tab_summary$sp <- sp

# get columns with names in tab_summary
estim_ab <- estim2014[, colnames(estim2014) %in% tab_summary$sp]

# estim_ab <- apply(estim2014, 2, mean)
ab_summary <- data.frame(A = character(),
                         B = numeric(),
                         C  = numeric())

for (sp in colnames(estim_ab)) {
  if (sp %in% tab_summary$sp) {
    estim_sp <- estim_ab[, sp]
    mean_estim <- mean(estim_sp[estim_sp != 0])
    tmp <-
      cbind.data.frame(sp,
                       mean_estim,
                       tab_summary$density_mean[tab_summary$sp == sp])
    ab_summary <- rbind.data.frame(ab_summary, tmp)
  }
}
colnames(ab_summary) <- c("sp", "estimate", "actual")
plot(y = ab_summary$estimate, x = ab_summary$actual, xlab = "Densité", ylab = "Estimation")
lines(x = c(0, 45), y = c(0, 45))
ggplot(ab_summary, aes(x = actual, y = estimate)) +
  geom_point(size = 2) +
  ylim(0, max(ab_summary$actual)) +
  geom_abline(slope = 1, intercept = 0)


# -- check estimations ---------------------------------------------------------

tab <- flore[flore$Periode == 2,]

# Check when 0/1 notes
tab0 <- transpose_flora(tab, base = 0)
ab   <- estim_abundance01(tab0, surf = 0.25)

tab0sum <-
  tab0 %>%
  dplyr::select(-carre.parc) %>%
  group_by(sp) %>%
  summarise_all(mean) %>%
  as.data.frame()
rownames(tab0sum) <- tab0sum$sp
real <- apply(tab0sum[, 2:length(tab0sum)], 1, mean)
esti <- apply(ab, 2, mean)
esti <- esti[order(names(esti))]
dat0 <- cbind.data.frame(real, esti)

ggplot(dat0, aes(x = real, y = esti)) +
  geom_point(size = 2) +
  xlim(0, 2) +
  ylim(0, 2) +
  geom_abline(slope = 1, intercept = 0)
ggsave("~/desktop/graphbase0.pdf")


# en base 2
tab2 <- transpose_flora(tab, base = 2)
ab2 <- estim_abundance(tab2, surf = 0.25, n_cores = 4)

tab2sum <-
  tab2 %>%
  dplyr::select(-carre.parc) %>%
  group_by(sp) %>%
  summarise_all(mean) %>%
  as.data.frame()
rownames(tab2sum) <- tab2sum$sp
real <- apply(tab2sum[, 2:length(tab2sum)], 1, mean)
esti <- apply(ab2, 2, mean)
esti <- esti[order(names(esti))]
dat2 <- cbind.data.frame(real, esti)

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

