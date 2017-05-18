# -- packages and functions ----------------------------------------------------
library(tidyverse)

# summary of flora data
files <- list.files("data/generated", pattern = "abondt_per_quadra", full.names = TRUE)
files <- files[!grepl("binomiale", files)]

surf <- 20
all_years <- numeric()
for (file in files) {
  year <- strsplit(file, "[_\\.]")[[1]][4]
  all_years <- c(all_years, year)
  var_name <- paste("weeds", year, sep = "")

  assign(var_name, read.csv(file, stringsAsFactor = FALSE, row.names = 1) * surf)
}


# -- all plants ----------------------------------------------------------------
sum_plots <- data.frame(year        = numeric(0),
                        n_plots     = numeric(0),
                        n_sp        = numeric(0),
                        n_sp_pplot  = numeric(0),
                        mab_pplot   = numeric(0))
sum_flora <- data.frame(year      = numeric(0),
                        sp        = character(0),
                        total     = numeric(0))


for (year in all_years) {
  a <- get(paste("weeds", year, sep = ""))

  # replace very small values by 0
  a[a == min(a)] <- 0

  # always remove interface
  a <- a[!(grepl("In", rownames(a))),]

  # remove species that are not present
  a <- a[, apply(a, 2, sum) != 0]

  # number of species
  n_sp <- ncol(a)

  # nb plots
  n_plots <- length(unique(rownames(a)))

  # nb species per plot
  n_sp_pplot <- mean(apply(a, 1, function(x) sum(x != 0)))

  # mean abundance per plot
  mab_pplot <- mean(apply(a, 1, sum))

  # present species
  sp <- colnames(a)

  # sum per species
  total <- as.numeric(apply(a, 2, sum))

  # fill the dataframe
  tab <- cbind.data.frame(year, n_plots, n_sp, n_sp_pplot, mab_pplot,
                          stringsAsFactors = FALSE)
  sum_plots <- rbind.data.frame(sum_plots, tab, stringsAsFactors=F)

  tab <- cbind.data.frame(year, sp, total)
  sum_flora <- rbind.data.frame(sum_flora, tab)
}

sum_flora$total <- as.numeric(as.character(sum_flora$total))

sum_flora <-
  sum_flora %>%
  group_by(sp) %>%
  summarise(total = sum(total)) %>%
  arrange(desc(total))

species <- character(0)
parc    <- character(0)

for (year in all_years) {
  a <- get(paste("weeds", year, sep = ""))

  # always remove interface
  a <- a[!grepl("-In", rownames(a)),]

 species <- c(species, colnames(a))
 parc <- c(parc, rownames(a))
}

mat <- matrix(0, nrow = length(unique(parc)), ncol = length(unique(species)))
rownames(mat) <- unique(parc)
colnames(mat) <- unique(species)

for (year in all_years) {
  a <- as.matrix(get(paste("weeds", year, sep = "")))

  # always remove interface
  a <- a[!grepl("-In", rownames(a)),]

  a[a == min(a)] <- 0

  k <- nrow(a)
  for (z in 1:length(a)) {
    # get row and column indices from the cell's index
    j <- ifelse(z %% k == 0, z/k, floor(z/k) + 1)
    i <- z - ((j - 1) * k)
    parc <- dimnames(a)[[1]][i]
    sp   <- dimnames(a)[[2]][j]
    mat[parc, sp] <- sum(mat[parc, sp], a[z])
  }
}

# compute occurence index as the percentage of plots where the species is present
occurence <- apply(mat, 2, function(x) sum(x != 0) / length(x) * 100)

sum_flora$occurence <- NA
sum_flora$sp <- as.character(sum_flora$sp)
for (i in 1:nrow(sum_flora)) {
  sum_flora$occurence[i] <- occurence[sum_flora$sp[i]]
}
# sum_flora$occurence <- occurence[sum_flora$sp 
sum_flora$occurence[sum_flora$sp=="Anagallis.foemina"]
# as.data.frame(head(occurence[order(decreasing = T, occurence)]))
# sum_flora$sp[23]

# adds 'status' based on the occurence of the species
sum_flora$status <- NA
sum_flora$status[sum_flora$occurence >= 25] <- "abundant"
sum_flora$status[sum_flora$occurence < 25 & sum_flora$occurence >= 5] <- "intermediate"
sum_flora$status[sum_flora$occurence < 5] <- "rare"
sum_flora <- sum_flora[order(sum_flora$total, decreasing = TRUE),]

# espèces les plus fréquentes
aa <- sum_flora[order(sum_flora$occurence, decreasing = T),]
aa %>%
  head(10) %>%
  knitr::kable(format="markdown")

# nombre total de parcelles
nrow(mat)

# nombre total d'espèces
ncol(mat)

# distribution des parcelles par année
apply(weeds2010, 1, sum)
sum_plots %>%
  ggplot(aes(x = year, y = n_plots)) +
  geom_bar(stat = 'identity')
ggsave("~/desktop/graphs/nplot.png")

# évolution de la richesse par année
sum_plots %>%
  ggplot(aes(x = year, y = n_sp_pplot)) +
  geom_bar(stat = "identity")
ggsave("~/desktop/graphs/rich_pplot.png")

sum_plots %>%
  ggplot(aes(x = year, y = mab_pplot)) +
  geom_bar(stat = "identity")
ggsave("~/desktop/graphs/ab_pplot.png")

# most frequent
aa$sp[1:6]
mostfreq <- aa$sp[1:6]

tabtot <- data.frame(year = character(), sp = character(), ab=numeric())
for (year in all_years) {
  a <- get(paste("weeds", year, sep = ""))
  a[a == min(a)] <- 0

  a <- a[,colnames(a) %in% mostfreq]
  ee <- apply(a, 2, sum)
  tab <- cbind(year, names(ee), as.numeric(ee))
  tabtot <- rbind.data.frame(tabtot, tab)
}
colnames(tabtot) <- c("year", "sp", "ab")
tabtot[,1] <- as.numeric(as.character(tabtot[,1]))
tabtot[,2] <- as.character(tabtot[,2])
tabtot[,3] <- as.numeric(as.character(tabtot[,3]))

tabtot %>%
  ggplot(aes(x = year, y = ab)) +
  geom_line() +
  facet_wrap(~ sp)

save.image('/tmp/data_flore.RData')
