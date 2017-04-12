transpose_flora <- function(tab, base = 0) {
  # this function returns 2 dataframes:
  #   origin    : data transposed to the same format as monitoring data so the
  #               functions to compute abundance can be used
  #   converted : data transposed to the same format but with the values
  #               converted into whether base 0 or base2 format
  # The 'position' column is used to set compatibility with the estimations
  # functions.

  # Prepare an empty matrix filled with 0 (for 0 abundance observed)
  names <- unique(paste(tab$Zone, tab$Quadra, tab$Placette, sep = ""))
  nrowA <- length(unique(tab$Parcelle)) * length(unique(tab$Taxon))
  ncolA <- length(names) + 3
  A <- matrix(ncol = ncolA, nrow = nrowA , data = rep(0, ncolA*nrowA))
  A <- data.frame(A)

  colnames(A) <- c("sp", "carre.parc", "position", names)

  A$sp <- rep(unique(tab$Taxon), length(unique(tab$Parcelle)))
  A$position <- "phantom"

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
  x <- A[, 4:length(A)]
  if (base == 0)
    x[x > 1] <- 1
  if (base == 2)
    x[x > 2] <- 2

  conv <- cbind.data.frame(A[, 1:3], x)

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

  # keep the original tab before converting it
  taborig <- tab

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
      ab <- sum(tab_sp[4:length(tab_sp)]) / (surf * (length(tab_sp) - 3))
      abond_per_plot[parc, sp] <- ab
    }
  }

  mat_vide <- matrix(0, ncol = 9, nrow = nrow(abond_per_plot) * ncol(abond_per_plot))
  dat <- data.frame(mat_vide)
  colnames(dat) <- c("parc", "sp", "real", "estimate", paste("n", 0:4, sep = ""))

  # create table with 2 column: one for the real value and one for the estimate
  # value
  i <- 0
  for (parc in rownames(abond_per_plot)) {
    for (sp in colnames(abond_per_plot)) {
      i <- i+1
      orig <- as.numeric(taborig[taborig$carre.parc == parc & taborig$sp == sp, 4:length(taborig)])
      dat$n0[i]       <- length(orig[orig == 0])
      dat$n1[i]       <- length(orig[orig == 1])
      dat$n2[i]       <- length(orig[orig == 2])
      dat$n3[i]       <- length(orig[orig == 3])
      dat$n4[i]       <- length(orig[orig == 4])
      dat$sp[i]       <- sp
      dat$parc[i]     <- parc
      dat$real[i]     <- abond_per_plot[parc, sp]
      dat$estimate[i] <- tab_estim[parc, sp]
    }
  }

  return(dat)
}

# ------------------------------------------------------------------------------

transpose_flora_tot <- function(tab) {
  # this function returns 4 tables transposed in the right way for abundance
  # estimation

  # Prepare an empty matrix filled with 0 (for 0 abundance observed)
  sp    <- unique(colnames(tab[, 16:length(tab)]))
  pc    <- unique(tab$Parcelle)
  n_pc  <- length(pc)
  n_sp  <- length(sp)
  sp    <- rep(sp, n_pc)
  carre.parc <- rep(pc, rep(n_sp, length(pc)))
  qd <- unique(paste0(tab$Zone, tab$Quadrat))

  A <- matrix(0, ncol = length(qd), nrow = length(carre.parc))

  colnames(A) <- qd

  ids <- paste0(carre.parc, sp)

  for (i in 1:nrow(tab)) {
    # i <- 1
    for (j in 16:ncol(tab)) {
      # for (i in 1:50) {
      iid <- which(ids == paste0(tab$Parcelle[i], colnames(tab)[j]))
      iqd <- paste0(tab$Zone[i], tab$Quadrat[i])
      A[iid, iqd] <- tab[i, j]
    }
  }

  # convert tables according to different degradations of data
  A0  <- A
  A0[A0 > 1] <- 1
  A2  <- A
  A2[A2 > 2] <- 2
  A10 <- A
  A10[A10 >= 2 &    A10 < 10]     <- 2
  A10[A10 >= 10 &   A10 < 100]    <- 3
  A10[A10 >= 100 &  A10 < 1000]   <- 4
  A10[A10 >= 1000 & A10 < 10000]  <- 5

  # we need a ghost column so the dataframe can be inputed to the function
  position <- rep("phantom", length(carre.parc))

  A   <- cbind.data.frame(sp, carre.parc, position, A, stringsAsFactors = FALSE)
  A0  <- cbind.data.frame(sp, carre.parc, position, A0, stringsAsFactors = FALSE)
  A2  <- cbind.data.frame(sp, carre.parc, position, A2, stringsAsFactors = FALSE)
  A10 <- cbind.data.frame(sp, carre.parc, position, A10, stringsAsFactors = FALSE)

  return(list(orig = A, base0 = A0, base2 = A2, base10 = A10))
}

# ------------------------------------------------------------------------------

estim_summary_tot <- function(tab, tab_estim, surf) {

  # replace very small estimate values with 0
  tab_estim[tab_estim <= min(tab_estim)] <- 0

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
      ab <- sum(tab_sp[4:length(tab_sp)]) / (surf * (length(tab_sp) - 3))
      abond_per_plot[parc, sp] <- ab
    }
  }

  mat_vide <- matrix(0, ncol = 9, nrow = nrow(abond_per_plot) * ncol(abond_per_plot))
  dat <- data.frame(mat_vide)
  colnames(dat) <- c("parc", "sp", "real", "estimate", paste("n", 0:4, sep = ""))

  # create table with 2 column: one for the real value and one for the estimate
  # value
  i <- 0
  for (parc in rownames(abond_per_plot)) {
    for (sp in colnames(abond_per_plot)) {
      i <- i+1
      orig <- as.numeric(tab[tab$carre.parc == parc & tab$sp == sp, 4:length(tab)])
      dat$n0[i]       <- length(orig[orig == 0])
      dat$n1[i]       <- length(orig[orig == 1])
      dat$n2[i]       <- length(orig[orig == 2])
      dat$n3[i]       <- length(orig[orig == 3])
      dat$n4[i]       <- length(orig[orig == 4])
      dat$sp[i]       <- sp
      dat$parc[i]     <- parc
      dat$real[i]     <- abond_per_plot[parc, sp]
      dat$estimate[i] <- tab_estim[parc, sp]
    }
  }

  return(dat)
}

# ------------------------------------------------------------------------------

estim_summary_tot_gm <- function(tab, tabgm, surf) {

  mat_vide <- matrix(0, ncol = 4, nrow = nrow(tab))
  dat <- data.frame(mat_vide)
  colnames(dat) <- c("parc", "sp", "real", "estimate")

  dat$parc <- tab$carre.parc
  dat$sp   <- tab$sp

  surfech <- surf * (length(tab) - 3)

  for (i in 1:nrow(tab)) {
    dat$real[i]     <- mean(as.numeric(tab[i, 4:length(tab)])) / surfech
    dat$estimate[i] <- mean(as.numeric(tabgm[i, 4:length(tabgm)])) / surfech
  }

  return(dat)
}
