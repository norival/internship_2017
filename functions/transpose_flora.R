transpose_flora <- function(tab, base = 0) {

  # Prepare an empty matrix filled with 0 (for 0 abundance observed)
  names <- unique(paste(tab$Zone, tab$Quadra, tab$Placette, sep = ""))
  nrowA <- length(unique(tab$Parcelle)) * length(unique(tab$Taxon))
  ncolA <- length(names) + 2
  A <- matrix(ncol = ncolA, nrow = nrowA , data = rep(0, ncolA*nrowA ))
  A <- data.frame(A)

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

  x <- A[, 3:length(A)]

  if (base == 0)
    x[x > 1] <- 1
  if (base == 2)
    x[x > 2] <- 2

  conv <- cbind.data.frame(A[, 1:2], x)

  return(list(origin = A, converted = conv))

}

# ------------------------------------------------------------------------------

estim_summary <- function(tab, tab_estim, surf) {

  # replace very small estimate values with 0
  tab_estim[tab_estim <= 1e-05] <- 0

  func <- function(x) {
    # convert abundance indices by taking the geometric mean for each class
    x[x == 2] <- (2+9)/2
    x[x == 3] <- (10+99)/2
    x[x == 4] <- (100+999)/2
    x[x == 5] <- (1000+9999)/2

    return(x)
  }

  tab <-
    tab %>%
    dplyr::select(-carre.parc) %>%
    mutate_if(is.numeric, funs(func)) %>%
    mutate_if(is.numeric, funs(. / surf)) %>%
    group_by(sp) %>%
    summarise_all(mean) %>%
    as.data.frame()
  rownames(tab) <- tab$sp

  real <- apply(tab[, 2:length(tab)], 1, mean)
  esti <- apply(tab_estim, 2, mean)
  esti <- esti[order(names(esti))]
  dat  <- cbind.data.frame(real, esti)

  return(dat)

}
