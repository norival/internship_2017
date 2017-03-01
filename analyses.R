# analyses

data_full <- read.csv("data/BDD_full.csv", stringsAsFactors = FALSE)

# subsetting
data_sub <-
  data_full %>%
  filter(ID_Enquêteur %in% c("inchausti", "grison"),
         Année_SuiviParcelle %in% c(2007, 2011),
         Type_CultureSimplifiée == "ble")

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

# graphiques
data_sub %>%
  ggplot(aes(x = n_kg_ha, y = Rdt_Qtx)) +
  geom_point()
cbind(data_sub$id_parc_tri, data_sub$n_kg_ha, data_sub$Rdt_Qtx)
