transpose_flora <- function(tab, base = 0) {

  # convert the abundance according to base
  if (base == 0)
    tab$Note[tab$Note > 1] <- 1
  if (base == 2)
    tab$Note[tab$Note > 2] <- 2

  # Prepare an empty matrix filled with 0 (for 0 abundance observed)
  nrowA <- length(unique(tab$Parcelle)) * length(unique(tab$Taxon))
  ncolA <- 2+8*4*4
  A <- matrix(ncol = ncolA, nrow=nrowA , data = rep(0, ncolA*nrowA ))
  A <- data.frame(A)

  zones <- rep(paste("Z", 1:8, sep = ""), rep(16, 8))
  quadr <- rep(paste("Q", 1:4, sep = ""), rep(4, 4))
  subqd <- rep(letters[1:4], 4)
  names <- paste(zones, quadr, subqd, sep = "")

  colnames(A) <- c("sp", "carre.parc", names)

  A$sp <- rep(unique(tab$Taxon), length(unique(tab$Parcelle)))

  carre.parc <- character()
  for (i in 1:length(unique(tab$Parcelle))) {
    carre.parc <-
      c(carre.parc, rep(unique(tab$Parcelle)[i], length(unique(tab$Taxon))))
  }
  A$carre.parc <- carre.parc

  # Remplis les quadrats vides (>10 min)
  for (i in 1:nrow(tab)) {
    sp        <- tab[i, "Taxon"]
    field     <- tab[i, "Parcelle"]
    zone      <- tab[i, "Zone"]
    quadrat   <- tab[i, "Quadrat"]
    subqd     <- tab[i, "Placette"] 
    abondance <- tab[i, "Note"]
    code      <- paste(zone, quadrat, subqd, sep = "")

    A[A$sp == sp & A$carre.parc == field, colnames(A) == code] <- abondance
  }

  # remove zones that are not present in the dataset
  zones_in <- unique(paste(tab$Zone, tab$Quadra, tab$Placette, sep = ""))
  A <- A[, colnames(A) %in% c("sp", "carre.parc", zones_in)]

  return(A)

}
