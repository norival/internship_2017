h.fct <- function(ltheta,v=v) {
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

lgpoisson <- function(k, r, theta) {
  # log-gamma-posson mixture law:
  # - 'r' and 'theta': paramaters for the gamma law
  # - value: return the log-likelihood 'ln P(X=k)'

  p <- 1 / (theta + 1)
  q <- 1 - p

  pk <-
    gamma(k + r) / (gamma(r) * factorial(k)) * p^r * q^k

  return(log(pk))
}

gammapoisson <- function(param, v, maxtheta = 30) {
  # on définit ensuite la fonction à minimiser:
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

  # log-vraissemblance du vecteur d'abondances (somme des log-vraissemblances de
  # chaque abondance). On mutliplie par '-1' car on cherche à minimiser la
  # fonction
  # log-likelihood of abundance vector (sum of all likelihoods) * -1 because the
  # function is minimized:
  ll <- (-1)*sum(lp[v+1])

  return(ll)
}

estim_abundance <- function(x, surf, n_cores = 2, progress = TRUE, addpos = TRUE,
                            fun = "h.fct", maxtheta = 30, nopar = FALSE) {

  if (!nopar) library(doSNOW)

  if (addpos & sum(grepl("Pa|In", x$carre.parc)) != length(x$carre.parc)) {
    # adds the position to the name of the plot, like in the old days
    x$carre.parc <- paste(x$carre.parc, stringr::str_to_title(x$pos), sep = "-")
  }

  abond_per_plot <- matrix(Inf,
                           ncol = length(unique(x$sp)),
                           nrow = length(unique(x$carre.parc)))

  colnames(abond_per_plot) <- unique(x$sp)
  rownames(abond_per_plot) <- unique(x$carre.parc)

  # add this if parallelization is not working (eg 'makeCluster()' hangs
  # indefinitely), it uses the non parallelized function at the end of this file
  # and exit the main function
  if (nopar) {
    out <- estim_nopar(x = x, out = abond_per_plot, surf = surf, fun = fun,
                       maxtheta = maxtheta, progress = progress)
    return(out)
  }

  if (progress) {
    cat("Estimation of abundances\n")

    pb  <- txtProgressBar(min = 0, max = nrow(x), style = 3, width = 80)
    progressbar <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progressbar)
  } else {
    opts <- list()
  }

  cl <- makeCluster(n_cores)
  registerDoSNOW(cl)
  
  export <- c(fun, "lgpoisson", "maxtheta")

  a <- foreach(i = 1:nrow(x), .combine = rbind, .export = export, .options.snow = opts) %dopar% {

    if (progress) setTxtProgressBar(pb, i)

    # param de Poisson par espèce
    # ici on récupère la ligne i, qui correspond à une espèce pour une parcelle
    v1 <- as.numeric(x[i, 4:ncol(x)])

    if (fun == "h.fct") {
      # on estime l'abondance de la parcelle par la loi de poisson
      Zu <- nlminb(c(0, 0), h.fct, v = v1, lower = c(-50, -50), upper = c(50, 50))

      # on fait la moyenne de poisson sur le paramètre de Zu, qui correspond aux
      # moyennes. On repasse en exponentielle car on avait fait un log
      mm <- com.mean(exp(Zu$par[1]), exp(Zu$par[2]))

    } else if (fun == "gammapoisson") {
      Zu <- nlminb(c(0, 0), gammapoisson, v = v1, maxtheta = maxtheta,
                   lower = c(-50, -50), upper = c(50, 50),
                   control = list(iter.max = 1000, abs.tol = 1e-20))

      r <- exp(Zu$par[1])
      p <- 1 / (exp(Zu$par[2]) + 1)
      mm <- r * ((1 - p) / p)
    }

    cbind.data.frame(x$carre.parc[i], x$sp[i], mm,
                     stringsAsFactors = F)
  }

  # On remplit la matrice vide avec les valeurs de a
  if (progress) {
    cat("\nFormating results\n")
    pb  <- txtProgressBar(min = 0, max = nrow(a), style = 3, width = 80)
  }

  for (i in 1:nrow(a)) {
    if (progress) {
      setTxtProgressBar(pb, i)
    }
    abond_per_plot[a[i, 1], a[i, 2]] <- a[i, 3]
  }

  if (progress) close(pb)

  # rapporter à 1 m^2
  abond_per_plot <- abond_per_plot / surf

  return(abond_per_plot)

  stopCluster(cl)
}

# ------------------------------------------------------------------------------
# This one is not used anymore for estimations but is used by the script that
# checks estimations

estim_abundance01 <- function(x, surf, gp.subquadra = FALSE, base2 = FALSE,
                              progress = TRUE, addpos = TRUE, fun = "h.fct") {
  # estimates abundance when data is 0/1

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

group_subqd <- function(x, base2 = TRUE, n.subqd = 4) {
  # x <- data2016
  # n.subqd <- 4

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
