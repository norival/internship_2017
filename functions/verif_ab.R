transpose_flora_tot <- function(tab) {
  tab <- flore2013
  # this function returns 4 dataframes:
  #   orig      : data transposed to the same format as monitoring data so the
  #               functions to compute abundance can be used
  #   basen     : data transposed to the same format but with the values
  #               converted into whether base0, base 10 or base2 format
  # The 'position' column is used to set compatibility with the estimations
  # functions.

  # normalize the 'Design', 'Zone', 'Quadra' columns to have unique identifiers
  for (parc in unique(tab$Parcelle)) {
    n <- nrow(tab[tab$Parcelle == parc,])
    tab[tab$Parcelle == parc, "Design"] <- paste0("D", 1:n)
    tab[tab$Parcelle == parc, "Zone"]   <- paste0("Z", 1:n)
    tab[tab$Parcelle == parc, "Quadrat"] <- paste0("Q", 1:n)
  }

  # Prepare an empty matrix filled with 0 (for 0 abundance observed)
  sp    <- unique(colnames(tab[, 16:length(tab)]))
  pc    <- unique(tab$Parcelle)
  n_pc  <- length(pc)
  n_sp  <- length(sp)
  sp    <- rep(sp, n_pc)
  carre.parc <- rep(pc, rep(n_sp, length(pc)))
  qd <- unique(paste0(tab$Design, tab$Zone, tab$Quadrat))

  A <- matrix(0, ncol = length(qd), nrow = length(carre.parc))

  colnames(A) <- qd

  ids <- paste0(carre.parc, sp)

  for (i in 1:nrow(tab)) {
    # i <- 1
    for (j in 16:ncol(tab)) {
      # for (i in 1:50) {
      iid <- which(ids == paste0(tab$Parcelle[i], colnames(tab)[j]))
      iqd <- paste0(tab$Design[i], tab$Zone[i], tab$Quadrat[i])
      if (is.na(tab[i, j])) tab[i, j] <- 0
      A[iid, iqd] <- tab[i, j]
    }
  }

  # convert tables according to different degradations of data
  ## presence/absence
  A0  <- A
  A0[A0 > 1] <- 1
  ## log_2
  A2  <- A
  A2[A2 > 2] <- 2
  ## log_10
  A10 <- A
  A10[A10 >= 1     & A10 < 10]     <- 1
  A10[A10 >= 10    & A10 < 100]    <- 2
  A10[A10 >= 100   & A10 < 1000]   <- 3
  A10[A10 >= 1000  & A10 < 10000]  <- 4
  A10[A10 >= 10000 & A10 < 100000] <- 5

  # we need a ghost column so the dataframe can be inputed to the function
  position <- rep("phantom", length(carre.parc))

  A   <- cbind.data.frame(sp, carre.parc, position, A, stringsAsFactors = FALSE)
  A0  <- cbind.data.frame(sp, carre.parc, position, A0, stringsAsFactors = FALSE)
  A2  <- cbind.data.frame(sp, carre.parc, position, A2, stringsAsFactors = FALSE)
  A10 <- cbind.data.frame(sp, carre.parc, position, A10, stringsAsFactors = FALSE)

  return(list(orig = A, base0 = A0, base2 = A2, base10 = A10))
}

# ------------------------------------------------------------------------------

estim_summary <- function(tab, tab_estim, surf) {

  # replace very small estimate values with 0
  tab_estim[tab_estim <= min(tab_estim)] <- 0

  # mat_vide <- matrix(0, ncol = 9, nrow = nrow(abond_per_plot) * ncol(abond_per_plot))
  mat_vide <- matrix(0, ncol = 7, nrow = nrow(tab))
  dat <- data.frame(mat_vide)
  colnames(dat) <- c("parc", "sp", "real", "estimate", paste("n", 0:2, sep = ""))

  # create table with 2 column: one for the real value and one for the estimate
  # value
  surfech <- surf * (length(tab) - 3)
  i <- 0
  for (parc in rownames(tab_estim)) {
    for (sp in colnames(tab_estim)) {
      i <- i+1
      abreal <- as.numeric(tab[tab$carre.parc == parc & tab$sp == sp, 4:length(tab)])
      dat$real[i]     <- sum(abreal) / surfech
      dat$n0[i]       <- sum(abreal == 0)
      dat$n1[i]       <- sum(abreal == 1)
      dat$n2[i]       <- length(abreal) - dat$n0[i] - dat$n1[i]
      dat$sp[i]       <- sp
      dat$parc[i]     <- parc
      dat$estimate[i] <- tab_estim[parc, sp]
    }
  }

  return(dat)
}

# ------------------------------------------------------------------------------

estim_summary_gm <- function(tab, tabgm, surf) {

  mat_vide <- matrix(0, ncol = 4, nrow = nrow(tab))
  dat <- data.frame(mat_vide)
  colnames(dat) <- c("parc", "sp", "real", "estimate")

  dat$parc <- tab$carre.parc
  dat$sp   <- tab$sp

  surfech <- surf * (length(tab) - 3)

  for (i in 1:nrow(tab)) {
    dat$real[i]     <- sum(as.numeric(tab[i, 4:length(tab)])) / surfech
    dat$estimate[i] <- sum(as.numeric(tabgm[i, ])) / surfech
  }

  return(dat)
}

# ------------------------------------------------------------------------------

bootstrap <- function(nboot, tab) {
  bootres <- data.frame(interc    = numeric(nboot),
                        estimate  = numeric(nboot),
                        r.squared = numeric(nboot),
                        sigma     = numeric(nboot))

  for (i in 1:nboot) {
    samp <- sample(1:nrow(tab), nrow(tab), replace = TRUE)
    dat <- tab[samp, c("real", "estimate")]

    mod <- lm(real ~ estimate, data = dat)

    bootres[i, "interc"]    <- as.numeric(mod$coefficients[1])
    bootres[i, "estimate"]  <- as.numeric(mod$coefficients[2])
    bootres[i, "r.squared"] <- summary(mod)$r.squared
    bootres[i, "sigma"]     <- sigma(mod)
  }

  return(bootres)
}

# ------------------------------------------------------------------------------

bootpred <- function(x, boot) {

  mat <- matrix(nrow = length(x), ncol = nrow(boot), 0)

  for (i in 1:length(x)) {
    for (j in 1:nrow(boot)) {
      mat[i, j] <- boot[j, 2] * log(x[i]) + boot[j, 1]
      mat[i, j] <- exp(mat[i, j]) * exp(boot[j, 4]^2 / 2)
    }
  }

  moy   <- rowMeans(mat)
  icinf <- apply(mat, 1, function(x) quantile(x, 0.025))
  icsup <- apply(mat, 1, function(x) quantile(x, 0.975))
  dat   <- cbind.data.frame(x, moy, icinf, icsup)

  return(dat)
}

# ------------------------------------------------------------------------------

optim_maxtheta <- function(transposed, maxtheta, fun = "gammapoisson", surf = 1,
                           nboot = 2500) {

  # function to optimize the maxtheta parameter in the minimised function
  # returns a list with prediction from bootstraps

  results <- list()
  for (i in maxtheta) {
    # the loop compute estimation for all values of maxtheta, run the bootstrap
    # procedure on it, and store the results, we can the determine which value of
    # maxtheta is the best
    cat("maxtheta =", i, "\n")

    estim <- estim_abundance(x = transposed[["base2"]], surf = surf, n_cores = 3,
                             fun = fun, addpos = FALSE, maxtheta = i)
    cor_estim <- estim_summary(transposed[["orig"]], estim, surf = surf)

    lcor_estim <-
      cbind.data.frame(real = log(cor_estim$real), estimate = log(cor_estim$estimate))

    cat("bootstrap...\n")
    bootestim <- bootstrap(nboot, lcor_estim)

    cat("prediction...\n")
    predbootestim <- bootpred(0:30, bootestim)

    results[[i]] <- predbootestim

    rm(list=c("estim", "cor_estim", "lcor_estim", "bootestim", "predbootestim"))
  }

  return(results)
}

# ------------------------------------------------------------------------------

min_quadras <- function(tab, min = 5, nboot = 50) {

  max <- ncol(tab) - 3

  results <- data.frame(nqd = rep(max:min, rep(nrow(tab), length(max:min))),
                        obs_mean = 0,
                        est_mean = 0,
                        obs_inf  = 0,
                        obs_sup  = 0,
                        est_inf  = 0,
                        est_sup  = 0)

  for (i in max:min) {
    cat("Estimation for", i, "quadrates...\n")
    restmp <- array(0, c(nrow(tab), nboot, 2))

    for (j in 1:nboot) {
      cat("Bootstrap ", j, "/", nboot, "...\n", sep = "")

      # resample the number of quadrates
      newtab <- t(apply(tab[, 4:length(tab)], 1, function(x) sample(x, i)))

      # create fake columns
      newtaborig <- cbind.data.frame(sp         = tab$sp,
                                     carre.parc = tab$carre.parc,
                                     position   = tab$position,
                                     newtab,
                                     stringsAsFactors = FALSE)

      newtab[newtab > 2] <- 2
      newtabb2 <- cbind.data.frame(sp         = tab$sp,
                                   carre.parc = tab$carre.parc,
                                   position   = tab$position,
                                   newtab,
                                   stringsAsFactors = FALSE)

      # compute estimations
      estim <- estim_abundance(newtabb2, surf = 1, n_cores = 2, fun = "gammapoisson",
                               maxtheta = 20, addpos = FALSE, progress = F)

      estimsum <- estim_summary(tab = newtaborig, tab_estim = estim, surf = 1)
      restmp[,j,1] <- estimsum$real
      restmp[,j,2] <- estimsum$estimate
    }

    # get mean for observed values
    results$obs_mean[results$nqd == i] <- apply(restmp, c(1,3), mean)[,1]
    # get mean for estimated values
    results$est_mean[results$nqd == i] <- apply(restmp, c(1,3), mean)[,2]

    results$obs_inf[results$nqd == i]  <-
      apply(restmp, c(1,3), function(x) quantile(x, 0.025))[,1]
    results$obs_sup[results$nqd == i]  <-
      apply(restmp, c(1,3), function(x) quantile(x, 0.975))[,1]

    results$est_inf[results$nqd == i]  <-
      apply(restmp, c(1,3), function(x) quantile(x, 0.025))[,2]
    results$est_sup[results$nqd == i]  <-
      apply(restmp, c(1,3), function(x) quantile(x, 0.975))[,2]
  }

  return(results)
}
