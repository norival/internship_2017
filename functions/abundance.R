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

estim_core_poisson <- function(v) {
  n1 <- length(v[v == 0])
  n2 <- length(v[v == 1])

  lambda <- log((n1 + n2) / n1)

  return(lambda)
}

# ------------------------------------------------------------------------------

estim_core_gpoisson <- function(v, maxtheta) {
  # core function to estimate abundance from the gamma-poisson mixture law (or
  # negative binomial). It minimises the function log_lik_gpoisson, computes the
  # mean and returns it
  Zu <- nlminb(c(0, 0), log_lik_gpoisson, v = v, maxtheta = maxtheta,
               lower = c(-50, -50), upper = c(50, 50),
               control = list(iter.max = 1000, abs.tol = 1e-20))

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
               control = list(iter.max = 1000, abs.tol = 1e-20))

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

addpos <- function(x) {

  if (sum(grepl("Pa|In", x$carre.parc)) != length(x$carre.parc)) {
    # adds the position to the name of the plot, like in the old days
    pos <- x$position
    substr(pos, 1, 1) <- toupper(substr(pos, 1, 1))
    x$carre.parc <- paste(x$carre.parc, pos, sep = "-")
  } else {
    cat("Position is already in the name, nothing to do")
    return(x)
  }

  return(x)

}

# ------------------------------------------------------------------------------
# wrapper function, the one to use
# ------------------------------------------------------------------------------

estim_abundance <- function(x, surf, fun = "gammapoisson", maxtheta = 30)
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

  options(stringsAsFactors = FALSE)

  # build empty site*species matrix to store the final results
  abond_per_plot <- matrix(0,
                           ncol = length(unique(x$sp)),
                           nrow = length(unique(x$carre.parc)))

  colnames(abond_per_plot) <- unique(x$sp)
  rownames(abond_per_plot) <- unique(x$carre.parc)

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
    ab <- apply(tabsort, 1,
                function(x, maxtheta) estim_core_gpoisson(x, maxtheta),
                maxtheta)
  } else if (fun == "compoisson") {
    ab <- apply(tabsort, 1,
                function(x, maxtheta) estim_core_compoisson(x, maxtheta),
                maxtheta)
  } else if (fun == "poisson") {
    ab <- apply(tabsort, 1, function(x) estim_core_poisson(x))
  }

  # now get estimates for the whole dataframe
  ids_full <- as.character(apply(tab, 1, function(x) paste0(sort(x), collapse = "")))
  ab_full  <- as.numeric(sapply(ids_full, function(x, ab, ids) ab[ids == x], ab, ids))
  
  a <- cbind(infos[,1:2], ab_full)

  for (i in 1:nrow(a)) {
    abond_per_plot[a[i, 2], a[i, 1]] <- as.numeric(a[i, 3]) / surf
  }

  return(abond_per_plot)
}
