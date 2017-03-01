library(stringi)

# -- stuff ok ------------------------------------------------------------------
data_full_tmp <- read.csv("data/BDD_full_tmp.csv", stringsAsFactors = FALSE)

data_full_tmp$Protection_visée[data_full_tmp$Produit_phyto == "KARATE K"] <- "insecticide"
data_full_tmp$Produit_phyto[data_full_tmp$Produit_phyto == "U46D"] <- "U 46 D"

# Sélection des années > 2005
data_full_tmp <-
  data_full_tmp %>%
  filter(Année_SuiviParcelle > 2005) %>%
  droplevels()

# -- fertilisation -------------------------------------------------------------
data_full_tmp[data_full_tmp$Fertilisation %in% c("", " "),] <- NA

data_full_tmp$Dose_Ferti <-
  data_full_tmp$Dose_Ferti %>%
  stri_replace_all(regex = c(" ", ","), repl = c("", "."),
                   vectorize_all = FALSE) %>%
  as.numeric()

# conversion des m^3 en litres
data_full_tmp$Dose_Ferti <- ifelse(data_full_tmp$Unité_dose == "m3/ha",
                               data_full_tmp$Dose_Ferti * 1000,
                               data_full_tmp$Dose_Ferti)
data_full_tmp$Dose_Ferti <- ifelse(data_full_tmp$Unité_dose == "qtx/ha",
                               data_full_tmp$Dose_Ferti * 100,
                               data_full_tmp$Dose_Ferti)
data_full_tmp$Dose_Ferti <- ifelse(grepl("[Tt]/[Hh][Aa]", data_full_tmp$Unité_dose),
                               data_full_tmp$Dose_Ferti * 1000,
                               data_full_tmp$Dose_Ferti)
data_full_tmp$Unité_dose <-
  data_full_tmp$Unité_dose %>%
  stri_replace_all(regex = c("m3/ha", "qtx/ha", "[Tt]/[Hh][Aa]"),
                   repl = c("L/HA", "Kg/HA", "Kg/HA"),
                   vectorize_all = FALSE)

# unités doses ferti
data_full_tmp$Unité_dose <-
  data_full_tmp$Unité_dose %>%
  stri_replace_all(regex = c("[uU]{1}.*/", "[Kk][Gg].*/", "[Ll].*/")%s+%"[Hh][aA]",
                   repl = c("Unité/HA", "Kg/HA", "L/HA"),
                   vectorize_all = FALSE)

write.csv(data_full_tmp, "data/BDD_full.csv", row.names = FALSE)
