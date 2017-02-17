# ------------------------------------------------------------------------------
# file        : db_convert.R
# object      : convert the databases in the good format
# description : the databases must be in the format of
#               'data/BDD_dec2016_culture.csv' in order to have single database
#               for the analyses
# ------------------------------------------------------------------------------

# packages
library(tidyverse)

# ------------------------------------------------------------------------------
# converting data/BDD_enquetes_2011_robin_pr_xavier.csv

# the functions who do the job, used only once but put into a function for
# practical reasons
complete_db1 <- function(R, B) {

  # empty data.frame to store  the results
  B2 <- 
    data.frame(matrix(NA, nrow = 1, ncol = length(B)))
    colnames(B2) <- colnames(B)

  # do the conversion for each plot from R
  for (i in levels(as.factor(R$IDparcelle))) {

    # check if the ID of the plot is in the good format
    if (!(grepl("^[[:digit:]]+$", i))) {
      # if not, skip it and return a warning message
      warning(paste("\"", i, "\"", " n'est pas un nom de parcelle valide", sep = ""),
              call. = FALSE)
      next
    } else {
      cat("Processing plot ", i, "...\n", sep = "")
    }

    # subset the R data.frame with only the plot 'i'
    r <- filter(R, IDparcelle == i)

    # empty data.frame to store the conversion from this plot
    b <- data.frame(matrix(NA, nrow = 20, ncol = length(B)))
    colnames(b) <- colnames(B)

    # fill the data.frame b with unique values from r, ie values that are always
    # present only once per plot
    b$ID_Parcelle               <- r$IDparcelle[1]
    b$ID_Exploitation           <- r$IDexploitation[1]
    b$Année_SuiviParcelle       <- 2011
    b$Année_Enquête[1]          <- 2011
    b$point_X[1]                <- r$point_X[1]
    b$point_Y[1]                <- r$point_Y[1]
    b$Type_Culture[1]           <- r$culture_2011[1]
    b$Type_CultureSimplifiée[1] <- r$culture_2011[1]
    b$Surface_ha[1]             <- r$surface[1]
    b$Type_Exploitation[1]      <- r$type_exploitation[1]
    b$Type_de_sol[1]            <- r$type_de_sol[1]
    b$Interculture[1]           <- r$cipan_2011[1]
    b$Mélange[1]                <- r$melange[1]
    b$Date_Semis[1]             <- r$date.V0[r$intervention == "semis"][1]
    b$Densité_Semis[1]          <- r$densite.de.semis..kg.ha.[1]
    b$Unité_DensitéSemis[1]     <- "KG/HA"
    b$ObjRdt..qtx.[1]           <- r$Objectif.rdt..Qtx.[1]
    b$Rdt_Qtx[1]                <- r$Rdt..Qtx.[1]
    b$Culture_précédente[1]     <- r$X2010[1]
    b$Culture_suivante[1]       <- r$X2012[1]

    # get informations about fertilization
    # subset the data.frame
    rtmp <- filter(r, intervention == "fertilisation")

    # do this only if fertilization is mentionned in 'r$intervention'
    if (nrow(rtmp) > 0) {
      # then get informations from each line
      for (j in 1:nrow(rtmp)) {
        b$Produit_Ferti[j]  <- rtmp$produit[j]
        b$Dose_Ferti[j]     <- rtmp$dose.passage[j]
        b$Unité_dose[j]     <- rtmp$unité.ha[j]
        b$Date_Ferti[j]     <- rtmp$date.V0[j]
      }
    }

    # get informations about phyto products
    rtmp <-
      filter(r, intervention %in%
             c("fongicide", "herbicide", "insecticide", "molluscide", "raticide", "répulsif"))

    # do this only if there is information about phyto products
    if (nrow(rtmp) > 0) {
      for (j in 1:nrow(rtmp)) {
        b$Protection_visée[j] <- rtmp$intervention[j]
        b$Produit_phyto[j]    <- rtmp$produit[j]
        b$Dose_Phyto[j]       <- rtmp$dose.passage[j]
        b$Unité_dose[j]       <- rtmp$unité.ha[j]
        b$Date_Phyto[j]       <- rtmp$date.V0[j]
      }
    }

    # get informations about soil work
    # informations are stored in r[, 15:21] which are named "travail_sol__[1-7]"
    rtmp <- r[, 15:21]
  
    for (j in 1:length(rtmp)) {
      if (is.na(rtmp[1, j]) || rtmp[1, j] == "0") {
        next
      } else if (grepl("semis", rtmp[1, j])) {
        next
      } else {
        b$Type_Tsol[j] <- rtmp[1, j]
      }
    }

    # get information about 'enrobage'
    rtmp <- filter(r, intervention == "enrobage")

    # do this only if there is information about enrobage
    if (nrow(rtmp) > 0) {
      b$Enrobage <- "oui"

      pdt_enrobage <- character(0)

      for (j in 1:nrow(rtmp)) {
        pdt_enrobage <- c(pdt_enrobage, rtmp$produit[j])
      }
      b$Produit_Enrobage[1] <- paste(pdt_enrobage, collapse = "+")
    }

    # strip the dataframe to remove lines with only NA
    b <- b[rowSums(is.na(b[, 5:length(b)])) != length(b[, 5:length(b)]),]

    # bind the dataframe with the big dataframe
    B2 <- rbind.data.frame(B2, b)
  }
  return(B2[2:nrow(B2),])
}

# reading the tables
R <- read.csv("data/BDD_enquetes_2011_robin_pr_xavier.csv",
              stringsAsFactors = FALSE)
B <- read.csv("data/BDD_dec2016_culture.csv",
              stringsAsFactors = FALSE)

resultat <- complete_db1(R, B)
write.csv(resultat, "data/converted_data.csv", row.names = FALSE)
