transpose_df <- function(tab, n_quadras, n_subqd = 1) {

  # get rid of factors in 'tab'
  i <- sapply(tab, is.factor)
  tab[i] <- lapply(tab[i], as.character)

  # tab$position <- tolower(tab$position)

  # adds the position to the name of the plot if not present already
  if (sum(grepl("Pa|In", tab$carre.parc)) != length(tab$carre.parc)) {
    a <- tab$position
    substr(a, 1, 1) <- toupper(substr(a, 1, 1))
    tab$carre.parc <- paste(tab$carre.parc, a, sep = "-")
  }

  # generate all possible combinations of sp * carre.parc * position
  idstab <- expand.grid(sp         = unique(tab$sp),
                        carre.parc = unique(tab$carre.parc),
                        position   = unique(tab$position),
                        KEEP.OUT.ATTRS   = FALSE,
                        stringsAsFactors = FALSE)

  # remove impossible combinations
  test <- as.character(sapply(idstab$carre.parc, function(x) {
                                test <- unlist(strsplit(x, "-"))
                                test[length(test)]}))
  COND    <- (tolower(test) == idstab$position)
  idstab  <- idstab[COND,]

  # create ids variable
  ids <- paste0(idstab$sp, idstab$carre.parc, idstab$position)

  # create empty matrix to store abundance data
  A <- matrix(0, nrow = length(ids), ncol = (n_quadras * n_subqd))

  # generate quadras names
  qd <- rep(paste0("q", 1:n_quadras), rep(n_subqd, n_quadras))
  if (n_subqd > 1) {
    colnames(A) <- paste0(qd, letters[1:n_subqd])
  } else {
    colnames(A) <- qd
  }

  for (i in 1:nrow(tab)) {
    iid <- which(ids == paste0(tab$sp[i], tab$carre.parc[i], tab$position[i]))
    iqd <- paste0("q", tab$plot[i], tab$quadrat[i], sep = "")
    A[iid, iqd] <- tab$abondance[i]
  }

  # bind idstab + data
  A <- cbind.data.frame(idstab, A, stringsAsFactors = FALSE)

  return(A)
}
