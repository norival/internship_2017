transpose_flora <- function(tab, base = 0) {
  # this function returns 2 dataframes:
  #   origin    : data transposed to the same format as monitoring data so the
  #               functions to compute abundance can be used
  #   converted : data transposed to the same format but with the values
  #               converted into whether base 0 or base2 format

  # Prepare an empty matrix filled with 0 (for 0 abundance observed)
  names <- unique(paste(tab$Zone, tab$Quadra, tab$Placette, sep = ""))
  nrowA <- length(unique(tab$Parcelle)) * length(unique(tab$Taxon))
  ncolA <- length(names) + 2
  A <- matrix(ncol = ncolA, nrow = nrowA , data = rep(0, ncolA*nrowA))
  A <- data.frame(A)

  colnames(A) <- c("sp", "carre.parc", names)

  A$sp <- rep(unique(tab$Taxon), length(unique(tab$Parcelle)))

  # create a carre.parc variable
  A$carre.parc <-
    rep(unique(tab$Parcelle),
        rep(length(unique(A$sp)), length(unique(tab$Parcelle))))

  # fill in the empty quadras
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

  # create a dataframe converted according to the 'base' argument
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

  # replace log10 values with the geometric mean of the class and divide the
  # results by the surface of one subquadra (0.25^2)
  tab <-
    tab %>%
    dplyr::select(-carre.parc) %>%
    mutate_if(is.numeric, funs(func)) %>%
    mutate_if(is.numeric, funs(. / surf)) %>%
    group_by(sp) %>%
    summarise_all(mean) %>%
    as.data.frame()
  rownames(tab) <- tab$sp

  # compute the mean by species for both estimates and real values
  real <- apply(tab[, 2:length(tab)], 1, mean)
  esti <- apply(tab_estim, 2, mean)
  esti <- esti[order(names(esti))]
  dat  <- cbind.data.frame(real, esti)

  return(dat)

}
