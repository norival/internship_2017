# ------------------------------------------------------------------------------
# core estimations functions
# ------------------------------------------------------------------------------

h.fct <- function(ltheta, v = v) {
  # calcule la vraissemblance des données des quadras
  # v = observations
  # ltheta = log du paramètre de poisson (intensité)

  library(compoisson)
  theta <- exp(ltheta)

  # si param est énorme, pas possible
  if(max(theta)>30){return(100000)}

  # proba d'abondance pour les espèces ayant un indice d'abondance de 0
  lp0 <- com.log.density(0,theta[1],theta[2])
  # proba d'abondance pour les espèces ayant un indice d'abondance de 1
  lp1 <- com.log.density(1,theta[1],theta[2])
  # proba d'abondance pour les espèces ayant un indice d'abondance de 2
  lp2 <- log(1-exp(lp0)-exp(lp1))
  lp <- c(lp0,lp1,lp2)
  ll <- (-1)*sum(lp[v+1])
  return(ll)
}

# ------------------------------------------------------------------------------

lgpoisson <- function(k, r, theta) {
  # log-gamma-poisson mixture law:
  # - 'r' and 'theta': paramaters for the gamma law
  # - value: return the log-likelihood 'ln P(X=k)'

  p <- 1 / (theta + 1)
  q <- 1 - p

  pk <-
    gamma(k + r) / (gamma(r) * factorial(k)) * p^r * q^k

  return(log(pk))
}

# ------------------------------------------------------------------------------

log_lik_gpoisson <- function(param, v, maxtheta = 30) {
  # get the log likelihood of the lgpoisson function
  # the function to minimize when estimating with Poisson-Gamma distribution

  # 'param' are given as log:
  param <- exp(param)

  # prevents the function to block on high parameter values, 30 seems to be a
  # reasonable choice
  if (!any(is.nan(param)) && max(param) > maxtheta) return(100000)

  # log-likelihood to observe 0 plant
  lp0 <- lgpoisson(k = 0, param[1], param[2])
  # log-likelihood to observe 1 plant
  lp1 <- lgpoisson(k = 1, param[1], param[2])
  # log-likelihood to observe more than 1 plant
  lp2 <- log(1 - exp(lp0) - exp(lp1))

  lp <- c(lp0, lp1, lp2)

  # log-likelihood of abundance vector (sum of all log-likelihoods) * -1 because
  # the function is minimized:
  ll <- (-1)*sum(lp[v+1])

  return(ll)
}

# ------------------------------------------------------------------------------

estim_core_gpoisson <- function(v, maxtheta) {
  # core function to estimate abundance from the gamma-poisson mixture law (or
  # negative binomial). It minimises the function log_lik_gpoisson, computes the
  # mean and returns it
  Zu <- nlminb(c(0, 0), log_lik_gpoisson, v = v, maxtheta = maxtheta,
               lower = c(-50, -50), upper = c(50, 50),
               control = list(itermax = 1000, abstol = 1e-20))

  r  <- exp(Zu$par[1])
  p  <- 1 / (exp(Zu$par[2]) + 1)
  mm <- r * ((1 - p) / p)

  return(mm)
}

# ------------------------------------------------------------------------------

estim_core_compoisson <- function(v, maxtheta) {
  # core function to estimate abundance from the com-poisson distribution.
  # It minimises the function h.fct, computes the
  # mean and returns it
  library(compoisson)

  Zu <- nlminb(c(0, 0), h.fct, v = v1, maxtheta = maxtheta,
               lower = c(-50, -50), upper = c(50, 50),
               control = list(itermax = 1000, abstol = 1e-20))

  mm <- com.mean(exp(Zu$par[1]), exp(Zu$par[2]))

  return(mm)
}

# ------------------------------------------------------------------------------
# utilities functions
# ------------------------------------------------------------------------------

group_subqd <- function(x, base2 = TRUE, n.subqd = 4) {
  # function to group subquadras when sample are of the form 0/1 with 4
  # subquadras and convert it to 0/1/2
  #  -> we consider there cannot be more than one plant per subquadra

  mat <- x[, 4:length(x)]

  newmat <- matrix(nrow = nrow(mat), ncol = ncol(mat) / n.subqd, 0)

  for(i in 1:ncol(newmat)) {
    # compte tous les sous quadras
    ii <- 4*(i-1) + (1:4)
    quadra <- as.numeric(rowSums(mat[, ii]))
    if (base2) {
      # si > 2, plusieurs plantes dans le quadra -> 2
      quadra[quadra > 2] <- 2
    } else {
      # si > 1, plusieurs plantes dans le quadra -> 1
      quadra[quadra > 1] <- 1
    }
    newmat[,i] <- quadra
  }

  colnames(newmat) <- paste0("q", 1:ncol(newmat))

  return(cbind.data.frame(x[, 1:3], newmat))
}

# ------------------------------------------------------------------------------
# wrapper functions
# ------------------------------------------------------------------------------

estim_abundance <- function(x, surf, n_cores = 2, progress = TRUE, addpos = TRUE,
                            fun = "gammapoisson", maxtheta = 30, nopar = FALSE)
{
  # -- function to estimate abundances -----------------------------------------
  # How it works:
  # To increase speed, we first sort the abundances vectors to get vectors that
  # are uniques (same abundance notes). The estimation function is then run only
  # on those unique vectors (generally much less than the total number of
  # vectors) and the result is stored. Finally, estimations for the whole
  # dataframe are drawn from the few unique vectors estimations, and results are
  # stored in a matrix site * species and returned
  # ----------------------------------------------------------------------------

  library(doSNOW)
  options(stringsAsFactors = FALSE)

  # build empty site*species matrix to store the final results
  abond_per_plot <- matrix(0,
                           ncol = length(unique(x$sp)),
                           nrow = length(unique(x$carre.parc)))

  colnames(abond_per_plot) <- unique(x$sp)
  rownames(abond_per_plot) <- unique(x$carre.parc)

  # start cluster
  cl <- makeCluster(n_cores)
  registerDoSNOW(cl)

  # wee need to export the core functions to the cluster environnement
  if (fun == "gammapoisson") {
    clusterExport(cl, c("lgpoisson", "log_lik_gpoisson", "estim_core_gpoisson"))
  }
  if (fun == "compoisson") {
    clusterExport(cl, c("h.fct", "estim_core_gpoisson"))
  }

  # remove plots with only '0'
  x <- x[rowSums(x[,4:length(x)]) != 0, ]

  # separate the numeric parts and the inofrmations part
  infos <- as.matrix(x[,1:3])
  tab   <- as.matrix(x[,4:length(x)])

  # sort abundances vectors
  tabsort <- t(apply(tab, 1, sort))

  # get only unique vectors in tabsort
  tabsort <- tabsort[!duplicated(tabsort),]

  # collapse abundance vector to get an 'id' vector
  ids <- as.character(apply(tabsort, 1, function(x) paste0(x, collapse = "")))

  # estimate abundance on unique vectors only
  if (fun == "gammapoisson") {
    ab <- parApply(cl, tabsort, 1,
                   function(x, maxtheta) estim_core_gpoisson(x, maxtheta),
                   maxtheta)
  } else if (fun == "compoisson") {
    ab <- parApply(cl, tabsort, 1,
                   function(x, maxtheta) estim_core_compoisson(x, maxtheta),
                   maxtheta)
  }

  # now get estimates for the whole dataframe
  ids_full <- as.character(parApply(cl, tab, 1, function(x) paste0(sort(x), collapse = "")))
  ab_full  <- as.numeric(parSapply(cl, ids_full, function(x, ab, ids) ab[ids == x], ab, ids))
  
  a <- cbind(infos[,1:2], ab_full)

  for (i in 1:nrow(a)) {
    abond_per_plot[a[i, 2], a[i, 1]] <- as.numeric(a[i, 3]) / surf
  }

  stopCluster(cl)

  return(abond_per_plot)
}

# ------------------------------------------------------------------------------

estim_abundance01 <- function(x, surf, gp.subquadra = FALSE, base2 = FALSE,
                              progress = TRUE, addpos = TRUE, fun = "h.fct") {
  # estimates abundance when data is 0/1
  # This one is not used anymore for estimations but is used by the script that
  # checks estimations

  if (addpos & sum(grepl("Pa|In", x$carre.parc)) == 0) {
    # adds the position to the name of the plot, like in the old days
    pos <- gsub("[[:digit:]]+", "", x$position)
    x$carre.parc <- paste(x$carre.parc, stringr::str_to_title(pos), sep = "-")
  }

  # if we decide to pass the results to log base2, we must group the subquadras
  if (base2) gp.subquadra <- TRUE

  # Création d'un tableau vide pour récupérer les estmations d'abondances
  abond_per_plot <- matrix(Inf,
                           ncol = length(unique(x$sp)),
                           nrow = length(unique(x$carre.parc)))

  colnames(abond_per_plot) <- unique(x$sp)
  rownames(abond_per_plot) <- unique(x$carre.parc)

  iparc <- 0
  for (parc in unique(x$carre.parc)) {
    # pour chaque parcelle
    dat_parc <- x[x$carre.parc == parc, ]
    # print(head(dat_parc[dat_parc$position == "pa2",]))

    if (progress) {
      iparc <- iparc+1
      ipb <- 0
      cat("Computing parc ", parc, " (", iparc, "/",
          length(unique(x$carre.parc)), ")...\n", sep = "")
      pb <- txtProgressBar(min = 0, max = length(unique(dat_parc$sp)),
                            style = 3, width = 80)
    }

    for (sp in unique(dat_parc$sp)) {
      # pour chaque espèce
      if (progress) {
        ipb <- ipb+1
        setTxtProgressBar(pb, ipb)
      }

      dat_sp <- dat_parc[dat_parc$sp == sp, ]

      ab_subq <- NULL
      for (i in 1:nrow(dat_sp)) {
        # combiner les pa1 et pa2, donne 80 sous quadra
        ab_subq <- c(ab_subq, as.numeric(dat_sp[i, 4:ncol(dat_sp)]))
      }

      if (gp.subquadra) {
        # group subquadra into on quadra
        ab_q <- NULL
        for(i in 1:10) {
          # compte tous les sous quadras
          ii <- 4*(i-1) + (1:4)
          v1 <- sum(ab_subq[ii])
          if (base2) {
            # si > 2, plusieurs plantes dans le quadra -> 2
            v1[v1 > 2] <- 2
          } else {
            # si > 1, plusieurs plantes dans le quadra -> 1
            v1[v1 > 1] <- 1
          }
          ab_q <- c(ab_q, v1)
        }
      } else {
        ab_q <- ab_subq
      }

      if (base2) {
        # on estime l'abondance de la parcelle par la loi de poisson
        Zu <- nlminb(c(0, 0), h.fct, v = ab_q, lower = c(-50, -50),upper = c(50, 50))

        # on fait la moyenne de poisson sur le paramètre de Zu, qui correspond aux
        # moyennes. On repasse en exponentielle car on avait fait un log
        lambda <- com.mean(exp(Zu$par[1]), exp(Zu$par[2]))
      } else {
        n1 <- length(ab_q[ab_q == 0])
        n2 <- length(ab_q[ab_q == 1])
        lambda <- log((n1 + n2) / n1)
      }

      # On rajoute cette moyenne dans le tableau vide initial
      abond_per_plot[parc, sp] <- lambda / surf
    }
    if (progress) close(pb)
  }

  return(abond_per_plot)

}

# ------------------------------------------------------------------------------
# this is just a non-parellelized version of the part that do the estimation,
# used when 'nopar' arg is set to TRUE.

estim_nopar <- function(x, out, surf, fun, maxtheta, progress) {

  cat("No parallelization! This can be very slow!\n")
  cat("Estimation of abundances...\n")

  if (progress) {
    pb  <- txtProgressBar(min = 0, max = nrow(x), style = 3, width = 80)
  }

  for (i in 1:nrow(x)) {
    if (progress) setTxtProgressBar(pb, i)

    v1 <- as.numeric(x[i, 4:ncol(x)])

    if (fun == "h.fct") {
      Zu <- nlminb(c(0, 0), h.fct, v = v1, lower = c(-50, -50), upper = c(50, 50))
      mm <- com.mean(exp(Zu$par[1]), exp(Zu$par[2]))

    } else if (fun == "gammapoisson") {
      Zu <- nlminb(c(0, 0), gammapoisson, v = v1, maxtheta = maxtheta,
                   lower = c(-50, -50), upper = c(50, 50),
                   control = list(iter.max = 1000, abs.tol = 1e-20))

      r <- exp(Zu$par[1])
      p <- 1 / (exp(Zu$par[2]) + 1)
      mm <- r * ((1 - p) / p)
    }

    out[x$carre.parc[i], x$sp[i]] <- mm / surf
  }

  if (progress) close(pb)

  return(out)
}

