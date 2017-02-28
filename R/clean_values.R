library(stringi)

# -- stuff ok ------------------------------------------------------------------
data_full <- read.csv("data/BDD_full.csv", stringsAsFactors = FALSE)

# Sélection des années > 2005
data_full <-
  data_full %>%
  filter(Année_SuiviParcelle > 2005) %>%
  droplevels()

# fertilisation
data_full[data_full$Fertilisation %in% c("", " "),] <- NA

data_full$Dose_Ferti <-
  data_full$Dose_Ferti %>%
  stri_replace_all(regex = c(" ", ","), repl = c("", "."),
                   vectorize_all = FALSE) %>%
  as.numeric()

# conversion des m^3 en litres
data_full$Dose_Ferti <- ifelse(data_full$Unité_dose == "m3/ha",
                               data_full$Dose_Ferti * 1000,
                               data_full$Dose_Ferti)
data_full$Dose_Ferti <- ifelse(data_full$Unité_dose == "qtx/ha",
                               data_full$Dose_Ferti * 100,
                               data_full$Dose_Ferti)
data_full$Dose_Ferti <- ifelse(grepl("[Tt]/[Hh][Aa]", data_full$Unité_dose),
                               data_full$Dose_Ferti * 1000,
                               data_full$Dose_Ferti)
data_full$Unité_dose <-
  data_full$Unité_dose %>%
  stri_replace_all(regex = c("m3/ha", "qtx/ha", "[Tt]/[Hh][Aa]"),
                   repl = c("L/HA", "Kg/HA", "Kg/HA"),
                   vectorize_all = FALSE)

# unités doses ferti
data_full$Unité_dose <-
  data_full$Unité_dose %>%
  stri_replace_all(regex = c("[uU]{1}.*/", "[Kk][Gg].*/", "[Ll].*/")%s+%"[Hh][aA]",
                   repl = c("Unité/HA", "Kg/HA", "L/HA"),
                   vectorize_all = FALSE)

