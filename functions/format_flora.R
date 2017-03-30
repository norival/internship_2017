transpose_df <- function(tab, n_quadras, n_subqd = 1, pos = "") {

  # get rid of factors in 'tab'
  i <- sapply(tab, is.factor)
  tab[i] <- lapply(tab[i], as.character)

  # create empty matrix to store abundance data
  n_sp <- length(unique(tab$sp))
  n_pl <- length(unique(tab$carre.parc))
  nrowA <- n_pl * n_sp * length(pos)
  ncolA <- n_quadras * n_subqd
  A <- matrix(0, nrowA, ncolA)

  # generate quadras names
  qd <- rep(paste("q", 1:n_quadras, sep = ""), rep(n_subqd, n_subqd))
  if (n_subqd > 1) {
    colnames(A) <- paste(qd, letters[1:n_subqd], sep = "")
  } else {
    colnames(A) <- qd
  }

  # create ids to get the line number
  sp <- rep(unique(tab$sp), n_pl * length(pos))

  # create a 'carre.parc' variable
  parc <- unique(tab$carre.parc)
  carre.parc <- rep(parc, rep(n_sp, length(parc)))

  # create a 'position' variable
  position <- rep(pos, rep(length(unique(tab$sp)), length(pos)))
  position <- rep(position, length(parc))

  # create single 'ids' variable to get row numbers
  ids <- paste(sp, carre.parc, position)

  if (n_subqd > 1) {
    for (i in 1:nrow(tab)) {
      iid <- which(ids == paste(tab$sp[i], tab$carre.parc[i], tab$position[i]))
      iqd <- paste("q", tab$plot[i], tab$quadra[i], sep = "")
      A[iid, iqd] <- tab$abondance[i]
    }
  } else {
    for (i in 1:nrow(tab)) {
      iid <- which(ids == paste(tab$sp[i], tab$carre.parc[i], tab$position[i]))
      iqd <- paste("q", tab$quadra[i], sep = "")
      A[iid, iqd] <- tab$abondance[i]
    }
  }

  # bind ids + data
  A <- cbind.data.frame(sp, carre.parc, position, A, stringsAsFactors = FALSE)

  return(A)
}
