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
