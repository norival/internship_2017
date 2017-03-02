library(ggplot2)
# analyses

data_full <- read.csv("data/BDD_full.csv", stringsAsFactors = FALSE)

# subsetting
data_sub <-
  data_full %>%
  filter(ID_Enquêteur %in% c("inchausti", "grison"),
         Année_Enquête %in% c(2007, 2011))

data_sub$Produit_Ferti <-
  data_sub$Produit_Ferti %>%
  stri_replace_all(fixed = "/", repl = "-")

data_sub$id_parc_tri <- paste(data_sub$ID_Exploitation,
                              data_sub$ID_Parcelle,
                              data_sub$Année_SuiviParcelle,
                              sep = "_")


# -- fertilisation -------------------------------------------------------------
fert <- read.csv("data/BD_Fertilizers_dec2016.csv", stringsAsFactors = FALSE)
fert$Fertilisant <- trimws(fert$Fertilisant)

# Récupérer les doses où j'ai les doses en Unité/HA
n_ha <- numeric(nrow(data_sub))
n_ha[n_ha == 0] <- NA
for (i in 1:nrow(data_sub)) {
  if (is.na(data_sub$Unité_dose[i])) {
    next
  }
  if (data_sub$Unité_dose[i] == "Unité/HA") {
    n_ha[i] <- data_sub$Dose_Ferti[i]
  }
  else if (data_sub$Unité_dose[i] == "Kg/HA") {
    if (toupper(data_sub$Produit_Ferti[i]) %in% toupper(fert$Fertilisant)) {
      n_ha[i]  <-
        fert$Ferti.N...kg.100.kg.ou.l.[toupper(fert$Fertilisant) ==
                                       toupper(data_sub$Produit_Ferti[i])] * data_sub$Dose_Ferti[i] / 100
    }
  }
}
data_sub$n_kg_ha <- n_ha
data_sub$Rdt_Qtx <- as.numeric(data_sub$Rdt_Qtx)

# calcul dose d'azote / ha
data_sub$qtx_ha <- data_sub$Rdt_Qtx / as.numeric(data_sub$Surface_ha)

# graphiques
data_sub %>%
  filter(!is.na(n_kg_ha), toupper(Type_CultureSimplifiée) == "MAIS",
         !is.na(qtx_ha)) %>%
  ggplot(aes(x = n_kg_ha, y = qtx_ha)) +
  geom_point(size = 4) +
  geom_smooth()
# cbind.data.frame(data_sub$id_parc_tri, data_sub$n_kg_ha, data_sub$Rdt_Qtx)
# length(n_ha[!is.na(n_ha)]) / length(n_ha[is.na(n_ha)]) * 100

a <- 
  data_sub %>%
  filter(!is.na(n_kg_ha), !is.na(qtx_ha)) %>%
  filter(!is.na(qtx_ha)) %>%
  filter(Type_CultureSimplifiée == "mais") #%>%
  group_by(Type_CultureSimplifiée) %>%
  summarise(n = length(unique(id_parc_tri)))

plot(a$n_kg_ha, log(a$qtx_ha), cex = 3)
lines(lowess(a$n_kg_ha, a$qtx_ha, f = 0.5))

# ------------------------------------------------------------------------------
# travail du sol
unique(data_sub$Type_Tsol)

data_sub %>%
  # filter(!is.na(n_kg_ha), !is.na(qtx_ha)) %>%
  filter(!is.na(Type_Tsol), !is.na(qtx_ha),
         !is.na(kg_n_ha)) %>%
  filter(Type_CultureSimplifiée == "ble") %>%
  ggplot(aes(x = kg_n_ha, y = qtx_ha)) +
  geom_point(size = 4) +
  facet_wrap(~ Type_Tsol, scales = "free_y")

data_sub %>%
  # filter(!is.na(n_kg_ha), !is.na(qtx_ha)) %>%
  filter(!is.na(Type_Tsol), !is.na(qtx_ha)) %>%
  filter(Type_CultureSimplifiée == "mais") %>%
  ggplot(aes(x = kg_n_ha, y = qtx_ha)) +
  geom_point(size = 4) +
  facet_wrap(~ Type_Tsol, scales = "free_y")

data_sub %>%
  filter(Type_CultureSimplifiée %in% c("ble", "mais")) %>%
  filter(!is.na(qtx_ha)) %>%
  ggplot(aes(x = Type_Tsol, y = qtx_ha, fill = Type_CultureSimplifiée)) +
  geom_boxplot() +
  stat_summary(fun.data = give.n, geom = "text")

give.n <- function(x){
   return(c(y = mean(x), label = length(x)))
}
# aucune valeur pour la profondeur de travail
# data_sub %>%
#   filter(Type_CultureSimplifiée %in% c("ble", "mais")) %>%
#   filter(!is.na(qtx_ha)) %>%
#   ggplot(aes(x = Profondeur_Tsol_.cm, y = qtx_ha, fill = Type_CultureSimplifiée)) +
#   geom_point()

# -- flore ---------------------------------------------------------------------

flore_esf <- read.csv("data/raw/ESFplantes_verfi.csv", sep = ";",
                      stringsAsFactors = FALSE)
flore_grison <- read.csv("data/raw/GrisonAL_2011_releve_flore.csv", sep = ";",
                         stringsAsFactors = FALSE)

a <- flore_esf[3,]
sum(a[6:length(a)])

# rdt ~ flore
data_sub$richesse <- NA
data_sub$abondance <- NA
aa$ID_Enquêteur
aa <-
  data_sub %>%
  filter(Année_SuiviParcelle == 2007, ID_Enquêteur == "inchausti")

data_sub$richesse <- NA
data_sub$abondance <- NA
for (i in levels(as.factor(aa$ID_Parcelle))) {
  if (i %in% flore_esf$No_parcelle) {
    aa$richesse[aa$ID_Parcelle == i] <-
      flore_esf$Richesse[flore_esf$No_parcelle == i]
    aa$abondance[aa$ID_Parcelle == i] <-
      flore_esf$Abondance[flore_esf$No_parcelle == i]
  }
}
aa %>%
  filter(!is.na(richesse),
         !is.na(Rdt_Qtx)) %>%
  group_by(Type_CultureSimplifiée) %>%
  summarise(n = length(unique(id_parc_tri)))
data_sub %>%
  filter(!is.na(richesse), Type_CultureSimplifiée == "ble") %>%
  nrow()
  ggplot(aes(x = richesse, y = Rdt_Qtx)) +
  geom_point(size = 3)

data_sub %>%
  filter(!is.na(abondance)) %>%
  ggplot(aes(x = abondance, y = Rdt_Qtx)) +
  geom_point(size = 3)

data_sub %>%
  select(Rdt_Qtx, richesse, id_parc_tri)

# engrais ~ flore

# ------------------------------------------------------------------------------
install.packages()
library(topicmodels)
test <- as.matrix(flore_esf[, 6:length(flore_esf)])
head(test)
dimnames(test) <- NULL
a <- test/rowSums(test)
a <- apply(test, 2, FUN = function(x) x / sum(x))
a <- scale(a, center = FALSE, scale = colSums(a))
colSums(a)
head(a)
sum(a[1,])

LDA()

data("AssociatedPress", package = "topicmodels")
lda <- LDA(AssociatedPress[1:20,], control = list(alpha = 0.1), k = 4)
lda_inf <- posterior(lda, AssociatedPress[21:30,])
