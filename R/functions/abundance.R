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

estim_abundance <- function(x, surf, n_cores = 2, progress = TRUE) {
  library(doSNOW)

  if (progress) {
    cat("Estimation of abundances\n")

    pb  <- txtProgressBar(min = 0, max = nrow(x), style = 3, width = 80)
    progressbar <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progressbar)
  } else {
    opts <- list()
  }

  mat_vide <- matrix(Inf,
                     ncol = length(unique(x$sp)),
                     nrow = length(unique(x$carre.parc)))
  abond_per_plot <- as.data.frame(mat_vide)

  colnames(abond_per_plot) <- unique(x$sp)
  rownames(abond_per_plot) <- unique(x$carre.parc)

  cl <- makeCluster(n_cores)
  registerDoSNOW(cl)
  

  a <- foreach(i = 1:nrow(x), .combine = rbind, .export = "h.fct", .options.snow = opts) %dopar% {

    if (progress) setTxtProgressBar(pb, i)

    # param de Poisson par espèce
    # ici on récupère la ligne i, qui correspond à une espèce pour une parcelle
    v1 <- as.numeric(x[i, 4:ncol(x)])

    # on estime l'abondance de la parcelle par la loi de poisson
    Zu <- nlminb(c(0, 0), h.fct, v = v1, lower = c(-50, -50), upper = c(50, 50))

    # on fait la moyenne de poisson sur le paramètre de Zu, qui correspond aux
    # moyennes. On repasse en exponentielle car on avait fait un log
    mm <- com.mean(exp(Zu$par[1]), exp(Zu$par[2]))
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
      cat("\n")
    }
    abond_per_plot[a[i, 1], a[i, 2]] <- a[i, 3]
  }

  # rapporter à 1 m^2
  abond_per_plot <- abond_per_plot / surf

  return(abond_per_plot)

  stopCluster(cl)
}

estim_abundance01 <- function(x, surf, gp.subquadra = FALSE, base2 = FALSE, progress = TRUE) {
  # estimates abundance when data is 0/1

  # if we decide to pass the results to log base2, we must group the subquadras
  if (base2) gp.subquadra <- TRUE

  # Création d'un tableau vide pour récupérer les estmations d'abondances
  mat_vide <- matrix(Inf,
                     ncol = length(unique(x$sp)),
                     nrow = length(unique(x$carre.parc)))
  abond_per_plot <- as.data.frame(mat_vide)

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
