bdd <- read.csv("data/BDD_dec2016_culture.csv")
bdd$ID_Parcelle <- as.factor(bdd$ID_Parcelle)
longlat <- read.csv("data/LatitudeLongitude.csv")
levels(bdd$ID_Parcelle) %in% levels(longlat$ID)

bdd_robin <- read.csv("data/BDD_enquetes_2011_robin_pr_xavier.csv")
bdd_grison <- read.csv("data/GrisonAL_2011_releve_flore.csv")
levels(bdd$ID_Parcelle) %in% levels(bdd_robin$IDparcelle)
levels(bdd_robin$IDparcelle) %in% levels(bdd_grison$code_parcelle)
colnames(bdd_robin)
colnames(bdd)

zzz <- data.frame(a = 1:5,
                  b = rep("A+B+C+D", 5))
library(dplyr)
library(tidyverse)
zzz %>%
  separate(b, int)
     df <- data.frame(x = c("a", "a b", "a b c", NA))
     df %>% separate(x, c("a", "b"))

# ------------------------------------------------------------------------------
# homogénéisation des bases de données

bdd_robin <- read.csv("data/BDD_enquetes_2011_robin_pr_xavier.csv",
                      stringsAsFactors = FALSE)
colnames(bdd_robin)



longest <- function(...) {
  a   <- 0
  arg <- list(...)

  for (i in 1:length(arg)) {
    if (length(arg[[i]]) > a) {
      a <- length(arg[[i]])
      lngst <- i
    }
  }
  return(lngst)
}

R <- read.csv("data/BDD_enquetes_2011_robin_pr_xavier.csv",
              stringsAsFactors = FALSE)
B <- read.csv("data/BDD_dec2016_culture.csv",
              stringsAsFactors = FALSE)

levels(as.factor(R$IDparcelle))

complete_db(R_sub, B)
R_sub <- R[1:34,]
i <- 8
complete_db <- function(R, B) {

  for (i in levels(as.factor(R$IDparcelle))) {
    if (!(grepl("^[[:digit:]]+$", i))) {
      warning(paste("\"", i, "\"", " n'est pas un nom de parcelle valide", sep = ""),
              call. = FALSE)
      next
    } else {
      # print(i)
    }
    r <-
      R %>%
      filter(IDparcelle == i)
    # tableau vide
    b <- data.frame(matrix(NA, nrow = nrow(r), ncol = length(bdd)))
    colnames(b) <- colnames(B)

    # remplir la première ligne du tableau avec les valeurs uniques
    b$ID_Parcelle[1] <- r$IDparcelle[1]
    b$ID_Exploitation[1] <- r$IDexploitation[1]
    b$Année_SuiviParcelle[1] <- 2011
    b$Année_Enquête[1] <- 2011
    b$point_X[1] <- r$point_X[1]
    b$point_Y[1] <- r$point_Y[1]
    b$Type_Culture[1] <- r$culture_2011[1]
    b$Type_CultureSimplifiée[1] <- r$culture_2011[1]
    b$Surface_ha[1] <- r$surface[1]
    b$Type_Exploitation[1] <- r$type_exploitation[1]
    b$Type_de_sol[1] <- r$type_de_sol[1]
    b$Interculture[1] <- r$cipan_2011[1]
    b$Mélange[1] <- r$melange[1]
    b$Date_Semis[1] <- r$date.V0[r$intervention == "semis"][1]
    b$Densité_Semis[1] <- r$densite.de.semis..kg.ha.[1]
    b$Unité_DensitéSemis[1] <- "KG/HA"
    b$ObjRdt..qtx.[1] <- r$Objectif.rdt..Qtx.[1]
    b$Rdt_Qtx[1] <- r$Rdt..Qtx.[1]
    b$Culture_précédente[1] <- r$X2010[1]
    b$Culture_suivante[1] <- r$X2012[1]

  }
}
complete_db(R_sub, B)
