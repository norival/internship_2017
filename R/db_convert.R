# ------------------------------------------------------------------------------
# file        : db_convert.R
# author      : Xavier Laviron
# object      : convert the databases in the good format
# description : the databases must be in the format of
#               'data/BDD_dec2016_culture.csv' in order to have single database
#               for the analyses
# ------------------------------------------------------------------------------

# packages
library(tidyverse)
library(stringi)

# ------------------------------------------------------------------------------
# converting data/BDD_enquetes_2011_robin_pr_xavier.csv

# the functions who do the job, used only once but put into a function for
# practical reasons
complete_db1 <- function(R, B, verbose = FALSE) {

  # empty data.frame to store  the results
  B2 <- 
    data.frame(matrix(NA, nrow = 1, ncol = length(B)))
  colnames(B2) <- colnames(B)

  # do the conversion for each plot from R
  for (parcelle in levels(as.factor(R$ID.parcelle.ZA))) {

    # subset the R data.frame with only the plot 'parcelle'
    r0 <- filter(R, ID.parcelle.ZA == parcelle)

    for (year in levels(as.factor(r0$annee_enquetee))) {

      # subset the R data.frame with only the year 'year'
      r <- filter(r0, annee_enquetee == year)

      # -- initialization of loop ----------------------------------------------
      # check if the ID of the plot is in the good format
      if (!(grepl("^[[:digit:]]+$", parcelle))) {
        # if not, skip it and return a warning message
        warning(paste("\"", parcelle, "\"", " n'est pas un nom de parcelle valide", sep = ""),
                call. = FALSE)
        next
      } else if (verbose) {
        # echo the plot being processed
        cat("Processing plot ", parcelle, " for year ", year, "...\n", sep = "")
      }


      # empty data.frame to store the conversion from this plot
      b <- data.frame(matrix(NA, nrow = 30, ncol = length(B)))
      colnames(b) <- colnames(B)

      # -- general informations ------------------------------------------------
      # fill the data.frame b with unique values from r, ie values that are
      # always present only once per plot
      b$ID_Parcelle               <- r$ID.parcelle.ZA[1]
      b$ID_Exploitation           <- r$ID.exploitation[1]
      b$Année_SuiviParcelle       <- r$annee_enquetee[1]
      b$Année_Enquête[1]          <- r$annee.de.l.enquete[1]
      b$point_X[1]                <- r$point_X[1]
      b$point_Y[1]                <- r$point_Y[1]
      b$Type_Culture[1]           <- r$culture[1]
      b$Type_CultureSimplifiée[1] <- r$classe.culture[1]
      b$Surface_ha[1]             <- r$surface.parcelle.ha.[1]
      b$Type_Exploitation[1]      <- r$type.exploitation[1]
      b$Type_de_sol[1]            <- r$type.de.sol[1]
      b$Interculture[1]           <- r$interculture.annee.N[1]
      b$Densité_Semis[1]          <- r$densite.semis.Kg.ha[1]
      b$Unité_DensitéSemis[1]     <- "KG/HA"
      b$Rdt_Qtx[1]                <- r$rdt.qtx[1]
      b$Culture_précédente[1]     <- r$culture.annee.N.1[1]
      b$Culture_suivante[1]       <- r$Culture.N.1[1]
      b$ID_Enquêteur[1]           <- r$nom.enqueteur[1]
      b$Syst_Prod[1]              <- r$Conduite.exploit[1]
      b$Surface_traitée_ha[1]     <- r$surface.traitee..ha.[1]

      # these informations are a bit more tricky to get
      ## sowing date of crop
      b$Date_Semis[1] <-
        r %>%
        filter(grepl("^[Ss]emis[_ ]?", Intervention)) %>%
        filter(!(grepl("interculture", Intervention))) %>%
        .[1, "date.intervention.V0"]

      ## intercrop sowing date
      if (!(is.na(b$Interculture[1])) && b$Interculture[1] == 1) {
        b$Date_Semis_Inter[1] <-
          r %>%
          filter(grepl("^[Ss]emis[_ ]?", Intervention)) %>%
          filter(grepl("interculture", Intervention)) %>%
          .[1, "date.intervention.V0"]
      }

      ## intercrop harvest
      b$Date_Récolte_Inter[1] <-
        r %>%
        filter(Intervention == "autre_recolte_interculture") %>%
        .[1, "date.intervention.V0"]

      ## melange
      if (grepl("\\+", paste(r$culture, collapse = ""))) {
        b$Mélange[1] <- "Monovariété"
      } else {
        b$Mélange[1] <- "Multivariété"
      }

      ## MAE plot
      b$Parcelle_MAE[1] <- r$Contrat.MAE.parcelle[1]

      ## harvest date
      b$Date_Récolte_Inter[1] <-
        r %>%
        filter(Intervention == "autre_recolte") %>%
        filter(grepl("interculture", Intervention)) %>%
        .[1, "date.intervention.V0"]

      ## cutting date
      b$Date_Récolte_Inter[1] <-
        r %>%
        filter(Intervention == "autre_fauch") %>%
        filter(!(grepl("interculture", Intervention))) %>%
        .[1, "date.intervention.V0"]


      # -- infos about fetilization --------------------------------------------
      # subset the data.frame
      rtmp <- filter(r, Intervention %in% c("ferti_inorg", "ferti_orga"))

      # do this only if fertilization is mentionned in 'r$intervention'
      if (nrow(rtmp) > 0) {
        # then get informations from each line
        for (j in 1:nrow(rtmp)) {
          b$Produit_Ferti[j]  <- rtmp$produit[j]
          b$Dose_Ferti[j]     <- rtmp$Dose[j]
          b$Unité_dose[j]     <- rtmp$unite.Dose[j]
          b$Date_Ferti[j]     <- rtmp$date.intervention.V0[j]
        }
      }

      ## irrigation
      R$irrigation.bolleen
      levels(as.factor(R$irrigation.mm.ha.))

      # -- infos about phyto ---------------------------------------------------
      rtmp <-
        filter(r, Intervention %in%
               c("trt_fongicide", "trt_herbicide", "trt_insecticide",
                 "trt_molluscide", "trt_raticide", "trt_regulateur",
                 "trt_repulsif"))

      # do this only if there is information about phyto products
      if (nrow(rtmp) > 0) {
        for (j in 1:nrow(rtmp)) {
          b$Protection_visée[j] <-
            stri_split_fixed(rtmp$intervention[j], "_", simplify = TRUE)[2]
          b$Produit_phyto[j]    <- rtmp$produit[j]
          b$Dose_Phyto[j]       <- rtmp$Dose[j]
          b$Unité_dose[j]       <- rtmp$unite.Dose[j]
          b$Date_Phyto[j]       <- rtmp$date.intervention.V0[j]
        }
      }

      # -- infos about soil work -----------------------------------------------
      # informations are stored on rows wit hIntervention begining with "sol"
      rtmp <-
        filter(r, grepl("^sol", Intervention))

      for (j in 1:nrow(rtmp)) {
        b$Type_Tsol[j] <-
          paste(stri_split_fixed(rtmp$Intervention[j], "_", simplify = TRUE)[-1],
                collapse = "_")
        b$Profondeur_Tsol_.cm[j]  <- rtmp$pronf_travail[j]
        b$Date_Tsol[j]            <- rtmp$date.intervention.V0[j]
      }

      # -- infos about caoting -------------------------------------------------
      # information stored in 'Intervention'
      rtmp <-
        filter(r, grepl("enrobage", Intervention))

      # do this only if there is information about enrobage
      if (nrow(rtmp) > 0) {
        b$Enrobage[1] <- "oui"

        pdt_enrobage <- character(0)

        for (j in 1:nrow(rtmp)) {
          pdt_enrobage <- c(pdt_enrobage, rtmp$produit[j])

          # get the objective of coating by looking at what is between
          # parentheses
          b$Obj_Enrobage[1] <-
            rtmp$Intervention[j] %>%
            stri_extract(regex = "\\([[:alpha:][:punct:]]*\\)") %>%
            stri_sub(from = 2, to = nchar(.) - 1)
        }
        b$Produit_Enrobage[1] <- paste(pdt_enrobage, collapse = "+")

      } else {
        b$Enrobage[1] <- "non"
      }

      # -- format the table ----------------------------------------------------
      # strip the dataframe to remove lines with only NA
      b <- b[rowSums(is.na(b[, 5:length(b)])) != length(b[, 5:length(b)]),]

      # bind the dataframe with the big dataframe
      B2 <- rbind.data.frame(B2, b)
    }
  }
  return(B2[2:nrow(B2),])
}

# read the tables
R <- read.csv("data/BDD_robin_full.csv",
              stringsAsFactors = FALSE)
B <- read.csv("data/BDD_dec2016_culture.csv",
              stringsAsFactors = FALSE)

# convert the data
resultat <- complete_db1(R, B, verbose = TRUE)

# write the converted data to a file
write.csv(resultat, "data/converted_data.csv", row.names = FALSE)
