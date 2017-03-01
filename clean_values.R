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

# clean 'Type_Culture'
data_full_tmp$Type_Culture <-
  data_full_tmp$Type_Culture %>%
  stri_replace_all(fixed = "_", replacement = " ") %>%
  stri_replace_all(fixed = "-", replacement = " ") %>%
  stri_replace_all(fixed = "d'", replacement = " ") %>%
  stri_trans_totitle() %>%
  stri_replace_all(regex = "[ ]?\\+[ ]?", replacement = "+") %>%
  stri_replace_all(regex = "[_ ]{1}[Aa]{1}[Bb]{1}", replacement = "_AB") %>%
  stri_replace_all(regex = "\\s*", replacement = "") %>%
  stri_replace_all(fixed = "Mais", replacement = "Maïs") %>%
  stri_replace_all(fixed = "TournesolMais", replacement = "Tournesol+Maïs") %>%
  stri_replace_all(regex = "RayGra[s]{1,2}.*$", replacement = "Raygrass") %>%
  stri_replace_all(fixed = "Rg", replacement = "Raygrass") %>%
  stri_replace_all(regex = "F[èe]?vero[l]{1,2}e[s]?", replacement = "Féverolle") %>%
  stri_replace_all(regex = "^1/2.*$", replacement = "Ble+Orge")

# clean 'Culture_précédente'
data_full_tmp$Culture_précédente <-
  data_full_tmp$Culture_précédente %>%
  stri_replace_all(fixed = "_", replacement = " ") %>%
  stri_replace_all(fixed = "-", replacement = " ") %>%
  stri_replace_all(fixed = "d'", replacement = " ") %>%
  stri_replace_all(fixed = "0", replacement = "NA") %>%
  stri_trans_totitle() %>%
  stri_replace_all(regex = "[ ]?\\+[ ]?", replacement = "+") %>%
  stri_replace_all(regex = "[_ ]{1}[Aa]{1}[Bb]{1}", replacement = "_AB") %>%
  stri_replace_all(regex = "\\s*", replacement = "") %>%
  stri_replace_all(fixed = "Mais", replacement = "Maïs") %>%
  stri_replace_all(fixed = "TournesolMais", replacement = "Tournesol+Maïs") %>%
  stri_replace_all(regex = "RayGra[s]{1,2}.*$", replacement = "Raygrass") %>%
  stri_replace_all(fixed = "Rg", replacement = "Raygrass") %>%
  stri_replace_all(regex = "F[èe]?vero[l]{1,2}e[s]?", replacement = "Féverolle") %>%
  stri_replace_all(regex = "^1/2.*$", replacement = "Ble+Orge") %>%
  stri_replace_all(regex = "\\?/", replacement = "+")

# clean 'Culture_suivante'
data_full_tmp$Culture_précédente <-
  data_full_tmp$Culture_précédente %>%
  stri_replace_all(fixed = "_", replacement = " ") %>%
  stri_replace_all(fixed = "-", replacement = " ") %>%
  stri_replace_all(fixed = "d'", replacement = " ") %>%
  stri_replace_all(fixed = "0", replacement = "NA") %>%
  stri_trans_totitle() %>%
  stri_replace_all(regex = "[ ]?\\+[ ]?", replacement = "+") %>%
  stri_replace_all(regex = "[_ ]{1}[Aa]{1}[Bb]{1}", replacement = "_AB") %>%
  stri_replace_all(regex = "\\s*", replacement = "") %>%
  stri_replace_all(fixed = "Mais", replacement = "Maïs") %>%
  stri_replace_all(fixed = "TournesolMais", replacement = "Tournesol+Maïs") %>%
  stri_replace_all(regex = "RayGra[s]{1,2}.*$", replacement = "Raygrass") %>%
  stri_replace_all(fixed = "Rg", replacement = "Raygrass") %>%
  stri_replace_all(regex = "F[èe]?vero[l]{1,2}e[s]?", replacement = "Féverolle") %>%
  stri_replace_all(regex = "^1/2.*$", replacement = "Ble+Orge") %>%
  stri_replace_all(regex = "\\?/", replacement = "+")

# clean 'Type_de_sol'
data_full_tmp$Type_de_sol <-
  data_full_tmp$Type_de_sol %>%
  stri_replace_all(regex = "\\s?,\\s?", replacement = "+") %>%
  stri_replace_all(fixed = "_", replacement = " ") %>%
  stri_trans_totitle() %>%
  stri_replace_all(fixed = " ", replacement = "") %>%
  stri_replace_all(fixed = "Et", replacement = "+") %>%
  stri_replace_all(fixed = "/", replacement = "+") %>%
  stri_replace_all(regex = ".{0,}Argil.*", replacement = "Argileux") %>%
  stri_replace_all(regex = "Limon.*", replacement = "Limoneux") %>%
  stri_replace_all(regex = "Alluv.*", replacement = "Alluvions") %>%
  stri_replace_all(regex = ".{0,}Groie.*", replacement = "Groie") %>%
  stri_replace_all(regex = "Calcaire.*", replacement = "Calcaire") %>%
  stri_replace_all(regex = "Terre[s]?Profond[es]{0,2}", replacement = "TerreProfonde") %>%
  stri_replace_all(fixed = "Goie", replacement = "Groie") %>%
  stri_replace_all(fixed = "TerresDeVarennes", replacement = "Varennes")

# clean 'Interculture'
data_full_tmp$Interculture <-
  data_full_tmp$Interculture %>%
  stri_replace_all(fixed = "d'", replacement = " ") %>%
  stri_trans_totitle() %>%
  stri_replace_all(regex = "[ ]?\\+[ ]?", replacement = "+") %>%
  stri_replace_all(regex = "-", replacement = "+") %>%
  stri_replace_all(regex = "[Rr]ay[\\+ -]?[Gg]ras", replacement = "Raygrass") %>%
  stri_replace_all(regex = " \\(.*\\)", replacement = "") %>%
  stri_replace_all(regex = "[Rr]epousse[s]?.*", replacement = "Repousse") %>%
  stri_replace_all(fixed = "Melange", replacement = "") %>%
  stri_replace_all(fixed = "0", replacement = "Aucune") %>%
  stri_replace_all(fixed = " ", replacement = "") %>%
  stri_replace_all(fixed = ",", replacement = "+") %>%
  stri_replace_all(fixed = "VesceAvoineDiploïde", replacement = "Vesce+Avoine") %>%
  stri_replace_all(fixed = "AvoineVesce", replacement = "Vesce+Avoine")

# clean 'Produit_Enrobage'
data_full_tmp$Produit_Enrobage <-
  data_full_tmp$Produit_Enrobage %>%
  stri_trans_totitle() %>%
  stri_replace_all(regex = "[ ]?\\+[ ]?", replacement = "+") %>%
  stri_replace_all(regex = "Thiram[e ]{0,}[[:alnum:]]*", replacement = "Thirame") %>%
  stri_replace_all(fixed = "Celest", replacement = "Célest") %>%
  stri_replace_all(fixed = "Tt", replacement = "Trt")

# clean 'Obj_Enrobage'
data_full_tmp$Obj_Enrobage <-
  data_full_tmp$Obj_Enrobage %>%
  stri_replace_all(regex = "C\\.?", replacement = "Corvicide") %>%
  stri_replace_all(fixed = "F", replacement = "Fongicide") %>%
  stri_replace_all(fixed = "contre_pourriture", replacement = "Fongicide") %>%
  stri_replace_all(fixed = "&", replacement = "+") %>%
  stri_replace_all(regex = "\\b[Ii]\\b", replacement = "Insecticide")

# clean 'Type_Tsol'
data_full_tmp$Type_Tsol <-
  data_full_tmp$Type_Tsol %>%
  stri_replace_all(regex = "bin.*", replacement = "Binage") %>%
  stri_replace_all(regex = "broy.*", replacement = "Broyage") %>%
  stri_replace_all(regex = "cover.*", replacement = "Cover crop") %>%
  stri_replace_all(regex = "dechaum.*dent", replacement = "Déchaumage_dents") %>%
  stri_replace_all(regex = "dechaum.*disque", replacement = "Déchaumage_disques") %>%
  stri_replace_all(regex = ".*disque.*", replacement = "Déchaumage_disques") %>%
  stri_replace_all(regex = "dechaum.*", replacement = "Déchaumage") %>%
  stri_replace_all(regex = "roul.*", replacement = "Roulage") %>%
  stri_replace_all(regex = ".*vibro.*", replacement = "Vibro") %>%
  stri_replace_all(regex = "herse_rotative", replacement = "Herse_rotative") %>%
  stri_replace_all(regex = "herse.*", replacement = "Herse") %>%
  stri_replace_all(regex = "interculture.*", replacement = "Interculture") %>%
  stri_replace_all(regex = "desherbage.*", replacement = "Désherbage") %>%
  stri_replace_all(regex = "labour.*", replacement = "Labour") %>%
  stri_replace_all(regex = "travail.*", replacement = "Travail") %>%
  stri_trans_totitle()

# clean 'Protection_visée'
data_full_tmp$Protection_visée %>%
  stri_replace_all(fixed = "molluscide", replacement = "Molluscicide") %>%
  stri_replace_all(fixed = "repulsif", replacement = "répulsif") %>%
  stri_replace_all(fixed = "regulateur", replacement = "Régulateur") %>%
  stri_trans_totitle()

# removes plot 2768 for 2011 because ambiguous units in 'Dose_Ferti'
data_full_tmp <-
  data_full_tmp[-which(data_full_tmp$ID_Parcelle == "2768" &
                   data_full_tmp$Année_SuiviParcelle == "2011"),]

# clean units
data_full_tmp$Unité_dose <-
  data_full_tmp$Unité_dose %>%
  stri_replace_all(regex = "[uU]{1}.*/[Hh][aA]", repl = "Unité/HA") %>%
  stri_replace_all(regex = "[Kk][Gg].*/[Hh][aA]", repl = "Kg/HA")

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
