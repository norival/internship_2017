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

estim_summary <- function(tab_orig, tab_estim, surf) {

  options(stringsAsFactors = FALSE)
  tab_orig <- tab_orig[rowSums(tab_orig[,4:length(tab_orig)]) != 0,]
  surfech  <- surf * (length(tab_orig) - 3)

  group <- function(x, surfech) {
    cbind(sum(as.numeric(x[4:length(x)])),
          tab_estim[x[2], x[1]] * surfech)
  }

  a <- t(apply(tab_orig, 1, group, surfech))
  result <- cbind.data.frame(tab_orig[,1:2], a)
  colnames(result)[3:4] <- c("observed", "estimate")

  return(result)
}

# ------------------------------------------------------------------------------

estim_summary_gm <- function(tab, tabgm, surf) {

  mat_vide <- matrix(0, ncol = 4, nrow = nrow(tab))
  dat <- data.frame(mat_vide)
  colnames(dat) <- c("sp", "carre.parc", "observed", "estimate")

  dat$carre.parc <- tab$carre.parc
  dat$sp         <- tab$sp

  surfech <- surf * (length(tab) - 3)

  for (i in 1:nrow(tab)) {
    dat$observed[i] <- sum(as.numeric(tab[i, 4:length(tab)]))
    dat$estimate[i] <- sum(as.numeric(tabgm[i, ]))
  }

  return(dat[dat$observed != 0,])
}

# ------------------------------------------------------------------------------

bootstrap <- function(nboot, tab) {
  bootres <- data.frame(interc    = numeric(nboot),
                        estimate  = numeric(nboot),
                        r.squared = numeric(nboot),
                        sigma     = numeric(nboot))

  for (i in 1:nboot) {
    samp <- sample(1:nrow(tab), nrow(tab), replace = TRUE)
    dat <- tab[samp, c("observed", "estimate")]

    mod <- lm(observed ~ estimate, data = dat)

    bootres[i, "interc"]    <- as.numeric(mod$coefficients[1])
    bootres[i, "estimate"]  <- as.numeric(mod$coefficients[2])
    bootres[i, "r.squared"] <- summary(mod)$r.squared
    bootres[i, "sigma"]     <- sigma(mod)
  }

  return(bootres)
}

# ------------------------------------------------------------------------------

bootpred <- function(x, boot) {

  funpred <- function(bootline, x) {
    val <- bootline[2] * log(x) + bootline[1]
    exp(val) * exp(bootline[4]^2 / 2)
  }

  pred <- apply(boot, 1, funpred, x = x)

  moy   <- rowMeans(pred)
  icinf <- apply(pred, 1, function(x) quantile(x, 0.025))
  icsup <- apply(pred, 1, function(x) quantile(x, 0.975))
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
      cbind.data.frame(observed = log(cor_estim$observed),
                       estimate = log(cor_estim$estimate))

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

min_quadras <- function(tab, min, nboot = 100, n_cores = 2, surf = 1)
{
  options(stringsAsFactors = FALSE)
  library(doSNOW)

  max <- ncol(tab) - 3

  # remove lines with only 0s
  tab <- tab[rowSums(tab[,4:length(tab)]) != 0,]

  cl <- makeCluster(n_cores)
  registerDoSNOW(cl)
  toexport <- c("estim_abundance", "estim_core_gpoisson", "lgpoisson",
                "log_lik_gpoisson", "estim_summary")

  errs <- data.frame(nqd = rep(min:max, rep(nboot, length(min:max))),
                     err = 0)

  for (nqd in max:min) {
    cat("estimating for", nqd, "quadras...\n")
    a <- foreach(i = 1:nboot, .export = toexport, .combine = "c") %dopar% {
      newtab <-
        t(apply(tab[,4:length(tab)], 1, function(x) sample(x, nqd, replace = TRUE)))
      newtabb2 <- t(apply(newtab, 1, function(x) {
                             x[x > 2] <- 2
                             return(x)}))
      newtabb2 <- cbind.data.frame(tab[,1:3], newtabb2)
      estim <- estim_abundance(newtabb2, surf = 1, fun = "gammapoisson",
                                maxtheta = 20)
      newtab <- cbind.data.frame(tab[,1:3], newtab)
      estim_sum <- estim_summary(tab, estim, surf)
      estim_sum$error <- abs(estim_sum$observed - estim_sum$estimate)

      return(mean(estim_sum$error))
    }
    errs$err[errs$nqd == nqd] <- a
  }

  stopCluster(cl)

  return(errs)
}
