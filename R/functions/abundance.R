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

abundance <- function(x, surf) {

  mat_vide <- matrix(Inf,
                     ncol = length(unique(x$sp)),
                     nrow = length(unique(x$carre.parc)))
  abond_per_plot <- as.data.frame(mat_vide)

  colnames(abond_per_plot) <- unique(x$sp)
  rownames(abond_per_plot) <- unique(x$carre.parc)

  for (parc in unique(x$carre.parc)) {

    # On récupère les lignes pour la parcelle parc
    dat_sub <- x[x$carre.parc == parc, ]

    # ab <- NULL
    for (i in (1:nrow(dat_sub))) {
      # param de Poisson par espèce
      # ici on récupère la ligne i, qui correspond à une espèce pour la parelle
      # parc
      v1 <- as.numeric(dat_sub[i, 4:ncol(dat_sub)])

      # on estime l'abondance de la parcelle par la loi de poisson
      Zu <- nlminb(c(0, 0), h.fct, v = v1, lower = c(-50, -50), upper = c(50, 50))

      # on fait la moyenne de poisson sur le paramètre de Zu, qui correspond aux
      # moyennes. On repasse en exponentielle car on avait fait un log
      mm <- com.mean(exp(Zu$par[1]), exp(Zu$par[2]))

      # On rajoute cette moyenne dans le tableau vide initial
      abond_per_plot[parc, dat_sub$sp[i]] <- mm

    }
  }

  # rapporter à 1 m^2
  abond_per_plot <- abond_per_plot / surf

  return(abond_per_plot)
}
