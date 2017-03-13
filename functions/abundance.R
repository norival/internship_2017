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

  cat("Estimation of abundances\n")

  if (progress) {
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

  cat("\nFormating results\n")
  # On remplit la matrice vide avec les valeurs de a
  if (progress)
    pb  <- txtProgressBar(min = 0, max = nrow(a), style = 3, width = 80)

  for (i in 1:nrow(a)) {
    if (progress) setTxtProgressBar(pb, i)
    abond_per_plot[a[i, 1], a[i, 2]] <- a[i, 3]
  }

  # rapporter à 1 m^2
  abond_per_plot <- abond_per_plot / surf

  cat("\n")

  return(abond_per_plot)

  stopCluster(cl)
}
