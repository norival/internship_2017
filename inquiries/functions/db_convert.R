# ------------------------------------------------------------------------------
# file        : R/functions/db_convert.R
# author      : Xavier Laviron
# object      : convert the databases in the good format
# description : the databases must be in the format of
#               'data/BDD_dec2016_culture.csv' in order to have single database
#               for the analyses
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# converting data/BDD_enquetes_2011_robin_pr_xavier.csv

# the functions who do the job, used only once but put into a function for
# practical reasons
convert_db <- function(R, B, verbose = FALSE, progress = TRUE) {

  # packages
  library(tidyverse)
  library(stringi)

  # empty data.frame to store  the results
  B2 <-
    data.frame(matrix(NA, nrow = 1, ncol = length(B)))
  colnames(B2) <- colnames(B)

  cat("Converting data.frame. This might take a while...\n")

  # set a progress bar
  i   <- 0
  if (progress & !verbose) {
    pb  <-
      txtProgressBar(min = 0,
                     max = length(levels(as.factor(R$ID.parcelle.ZA))),
                     style = 3, width = 80)
  }

  # do the conversion for each plot from R
  for (parcelle in levels(as.factor(R$ID.parcelle.ZA))) {

    # subset the R data.frame with only the plot 'parcelle'
    r0 <- filter(R, ID.parcelle.ZA == parcelle)

    for (year in levels(as.factor(r0$annee_enquetee))) {

      if (progress & !verbose) {
        # print the progress bar
        setTxtProgressBar(pb, i)
      }

      # subset the R data.frame with only the year 'year'
      r <- filter(r0, annee_enquetee == year)

      # -- initialization of loop ----------------------------------------------
      if (verbose) {
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
      b$Année_Enquête             <- r$annee.de.l.enquete[1]
      b$point_X[1]                <- r$point_X[1]
      b$point_Y[1]                <- r$point_Y[1]
      b$Type_Culture[1]           <- r$culture[1]
      b$Type_CultureSimplifiée[1] <- r$classe.culture[1]
      b$Surface_ha[1]             <- r$surface.2.ha[1]
      b$Type_Exploitation[1]      <- r$type.exploitation[1]
      b$Type_de_sol[1]            <- r$type.de.sol[1]
      b$Interculture[1]           <- r$interculture.annee.N[1]
      b$Densité_Semis[1]          <- r$densite.semis.Kg.ha[1]
      b$Unité_DensitéSemis[1]     <- "KG/HA"
      b$Rdt_Qtx[1]                <- r$rdt.qtx[!is.na(r$rdt.qtx)][1]
      b$Culture_précédente[1]     <- r$culture.annee.N.1[1]
      b$Culture_suivante[1]       <- r$Culture.N.1[1]
      b$ID_Enquêteur              <- r$nom.enqueteur[1]
      b$Syst_Prod[1]              <- r$Conduite.exploit[1]
      b$Surface_traitée_ha[1]     <- r$surface.traitee..ha.[1]
      b$Irrigation[1] <-
        ifelse(!is.na(r$irrigation.bolleen[1]) && r$irrigation.bolleen != 0,
               "oui",
               "non")

      # these informations are a bit more tricky to get
      ## sowing date of crop
      b$Date_Semis[1] <-
        r %>%
        filter(grepl("^[Ss]emis[_ ]?", Intervention)) %>%
        filter(!(grepl("interculture", Intervention))) %>%
        .[1, "dateV05"]

      ## intercrop sowing date
      if (!(is.na(b$Interculture[1])) && b$Interculture[1] == 1) {
        b$Date_Semis_Inter[1] <-
          r %>%
          filter(grepl("^[Ss]emis[_ ]?", Intervention)) %>%
          filter(grepl("interculture", Intervention)) %>%
          .[1, "dateV05"]
      }

      ## intercrop harvest
      b$Date_Récolte_Inter[1] <-
        r %>%
        filter(Intervention == "autre_recolte_interculture") %>%
        .[1, "dateV05"]

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
        .[1, "dateV05"]

      ## cutting date
      b$Date_Récolte_Inter[1] <-
        r %>%
        filter(Intervention == "autre_fauch") %>%
        filter(!(grepl("interculture", Intervention))) %>%
        .[1, "dateV05"]


      # -- infos about fetilization --------------------------------------------
      # subset the data.frame
      rtmp <- filter(r, Intervention %in% c("ferti_inorg", "ferti_orga"))

      # do this only if fertilization is mentionned in 'r$intervention'
      if (nrow(rtmp) > 0) {
        # then get informations from each line
        for (j in 1:nrow(rtmp)) {
          b$Dose_Ferti[j] <-
            ifelse(is.na(rtmp$Dose.convertie.trt[j]),
                   rtmp$Dose[j],
                   rtmp$Dose.convertie.trt[j])
          b$Unité_dose[j] <-
            ifelse(is.na(rtmp$unite.Dose.convertie[j]),
                   rtmp$unite.Dose[j],
                   rtmp$unite.Dose.convertie[j])

          b$Produit_Ferti[j]  <- rtmp$produit[j]
          b$Date_Ferti[j]     <- rtmp$dateV05[j]

          # convert the unit
          if (!is.na(b$Unité_dose[j])) {
            if (!grepl("/[Hh][Aa]", b$Unité_dose[j])) {
              if (toupper(b$Unité_dose[j]) == "KG") {
                b$Dose_Ferti[j] <-
                  as.numeric(b$Dose_Ferti[j])
              }
              if (toupper(b$Unité_dose[j]) == "G") {
                b$Dose_Ferti[j] <-
                  as.numeric(b$Dose_Ferti[j]) / 1000
              }
              if (toupper(b$Unité_dose[j]) == "T") {
                b$Dose_Ferti[j] <-
                  as.numeric(b$Dose_Ferti[j]) * 1000
              }
              b$Unité_dose[j] <- "Kg/Ha"
            }
          }
        }
      }


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
            stri_split_fixed(rtmp$Intervention[j], "_", simplify = TRUE)[2]
          b$Produit_phyto[j]    <- rtmp$produit[j]
          b$Dose_Phyto[j]       <- rtmp$Dose.convertie.trt[j]
          b$Unité_dose.1[j]     <- rtmp$unite.Dose.convertie[j]
          b$Date_Phyto[j]       <- rtmp$dateV05[j]
        }
      }

      # -- infos about soil work -----------------------------------------------
      # informations are stored on rows with Intervention begining with "sol"
      rtmp <-
        filter(r, grepl("^sol", Intervention))

      for (j in 1:nrow(rtmp)) {
        b$Type_Tsol[j] <-
          paste(stri_split_fixed(rtmp$Intervention[j], "_", simplify = TRUE)[-1],
                collapse = "_")
        b$Profondeur_Tsol_.cm[j]  <- rtmp$pronf_travail[j]
        b$Date_Tsol[j]            <- rtmp$dateV05[j]
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
      b <- b[rowSums(is.na(b[, 17:length(b)])) != length(b[, 17:length(b)]),]

      # bind the dataframe with the big dataframe
      B2 <- rbind.data.frame(B2, b)
    }
    i <- i + 1
  }
  # close the progress bar
  close(pb)
  return(B2[2:nrow(B2),])
}
