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
    mutate_if(is.numeric, funs(func))

  # create empty matrix to store results: one line per plot and one column per
  # species
  mat_vide <- matrix(Inf,
                     ncol = length(unique(tab$sp)),
                     nrow = length(unique(tab$carre.parc)))
  abond_per_plot <- as.data.frame(mat_vide)

  colnames(abond_per_plot) <- unique(tab$sp)
  rownames(abond_per_plot) <- unique(tab$carre.parc)

  for (parc in unique(tab$carre.parc)) {
    # do it for each plot
    tab_parc <- tab[tab$carre.parc == parc,]

    for (sp in unique(tab_parc$sp)) {
      # compute the sum for each species and report to the sampled surface,
      # which is surf * number of points
      tab_sp <- tab_parc[tab_parc$sp == sp,]
      ab <- sum(tab_sp[3:length(tab_sp)]) / (surf * (length(tab_sp) - 2))
      abond_per_plot[parc, sp] <- ab
    }
  }

  mat_vide <- matrix(0, ncol = 2, nrow = nrow(abond_per_plot) * ncol(abond_per_plot))
  dat <- data.frame(mat_vide)
  colnames(dat) <- c("real", "estimate")

  # create table with 2 column: one for the real value and one for the estimate
  # value
  i <- 0
  for (parc in rownames(abond_per_plot)) {
    for (sp in colnames(abond_per_plot)) {
      i <- i+1
      dat$real[i]     <- abond_per_plot[parc, sp]
      dat$estimate[i] <- tab_estim[parc, sp]
    }
  }

  return(dat)
}
