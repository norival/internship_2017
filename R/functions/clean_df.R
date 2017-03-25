# ------------------------------------------------------------------------------
# file        : functions/clean_df.R
# author      : Xavier Laviron
# object      : Clean the dataset
# ------------------------------------------------------------------------------


clean_df <- function(x) {
  library(stringi)

  # trim leading and trailing whitespaces from character vectors
  x <- data.frame(lapply(x, function(.x) if (class(.x) == "character") trimws(.x) else(.x)),
                  stringsAsFactors = FALSE)

  x$Protection_visée[x$Produit_phyto == "KARATE K"] <- "insecticide"
  x$Produit_phyto[x$Produit_phyto == "U46D"] <- "U 46 D"

  # Sélection des années > 2005
  x <-
    x %>%
    filter(Année_SuiviParcelle > 2005) %>%
    droplevels()

  # clean 'Type_Culture'
  x$Type_Culture <-
    x$Type_Culture %>%
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
  x$Culture_précédente <-
    x$Culture_précédente %>%
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
  x$Culture_précédente <-
    x$Culture_précédente %>%
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
  x$Type_de_sol <-
    x$Type_de_sol %>%
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
  x$Interculture <-
    x$Interculture %>%
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
  x$Produit_Enrobage <-
    x$Produit_Enrobage %>%
    stri_trans_totitle() %>%
    stri_replace_all(regex = "[ ]?\\+[ ]?", replacement = "+") %>%
    stri_replace_all(regex = "Thiram[e ]{0,}[[:alnum:]]*", replacement = "Thirame") %>%
    stri_replace_all(fixed = "Celest", replacement = "Célest") %>%
    stri_replace_all(fixed = "Tt", replacement = "Trt")

  # clean 'Obj_Enrobage'
  x$Obj_Enrobage <-
    x$Obj_Enrobage %>%
    stri_replace_all(regex = "C\\.?", replacement = "Corvicide") %>%
    stri_replace_all(fixed = "F", replacement = "Fongicide") %>%
    stri_replace_all(fixed = "contre_pourriture", replacement = "Fongicide") %>%
    stri_replace_all(fixed = "&", replacement = "+") %>%
    stri_replace_all(regex = "\\b[Ii]\\b", replacement = "Insecticide")

  # clean 'Type_Tsol'
  x$Type_Tsol <-
    x$Type_Tsol %>%
    stri_replace_all(regex = "bin.*", replacement = "Binage") %>%
    stri_replace_all(regex = "broy.*", replacement = "Broyage") %>%
    stri_replace_all(regex = "[Cc]over.*", replacement = "Cover_crop") %>%
    stri_replace_all(regex = "[Dd][ée]chaum.*[Dd]ent", replacement = "Déchaumage_dents") %>%
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
  x$Protection_visée <-
    x$Protection_visée %>%
    stri_replace_all(fixed = "molluscide", replacement = "Molluscicide") %>%
    stri_replace_all(fixed = "repulsif", replacement = "répulsif") %>%
    stri_replace_all(fixed = "regulateur", replacement = "Régulateur") %>%
    stri_trans_totitle()

  # removes plot 2768 for 2011 because ambiguous units in 'Dose_Ferti'
  x <- x[-which(x$ID_Parcelle == "2768" & x$Année_SuiviParcelle == "2011"),]

  # clean units
  x$Unité_dose <-
    x$Unité_dose %>%
    stri_replace_all(regex = "[uU]{1}.*/[Hh][aA]", repl = "Unité/HA") %>%
    stri_replace_all(regex = "[Kk][Gg].*/[Hh][aA]", repl = "Kg/HA")

  # -- fertilisation -------------------------------------------------------------
  x[x$Fertilisation %in% c("", " "),] <- NA

  x$Dose_Ferti <-
    x$Dose_Ferti %>%
    stri_replace_all(regex = c(" ", ","), repl = c("", "."),
                     vectorize_all = FALSE) %>%
    as.numeric()

  # conversion des m^3 en litres
  x$Dose_Ferti <- ifelse(x$Unité_dose == "m3/ha",
                         x$Dose_Ferti * 1000,
                         x$Dose_Ferti)
  x$Dose_Ferti <- ifelse(x$Unité_dose == "qtx/ha",
                         x$Dose_Ferti * 100,
                         x$Dose_Ferti)
  x$Dose_Ferti <- ifelse(grepl("[Tt]/[Hh][Aa]", x$Unité_dose),
                         x$Dose_Ferti * 1000,
                         x$Dose_Ferti)
  x$Unité_dose <-
    x$Unité_dose %>%
    stri_replace_all(regex = c("m3/ha", "qtx/ha", "[Tt]/[Hh][Aa]"),
                     repl = c("L/HA", "Kg/HA", "Kg/HA"),
                     vectorize_all = FALSE)

  # unités doses ferti
  x$Unité_dose <-
    x$Unité_dose %>%
    stri_replace_all(regex = c("[uU]{1}.*/", "[Kk][Gg].*/", "[Ll].*/")%s+%"[Hh][aA]",
                     repl = c("Unité/HA", "Kg/HA", "L/HA"),
                     vectorize_all = FALSE)

  return(x)
}
