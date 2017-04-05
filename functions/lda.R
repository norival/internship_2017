tidy_lda_post <- function(.post) {

  .plots  <- .post$topics
  .spp    <- .post$terms

  # group the data in a tidy manner so it can be easily plotted
  ## plots data
  .all_plots <-
    data.frame(parc = character(), x = numeric(), val = numeric(), id = character())
  for (i in 1:ncol(.plots)) {
    .tmp <-
      cbind.data.frame(rownames(.plots), 1:nrow(.plots), .plots[, i], paste("gp", i, sep = ""))
    .all_plots <- rbind.data.frame(.all_plots, .tmp)
  }
  colnames(.all_plots) <- c("parc", "x", "val", "gp")
  .all_plots$parc <- as.character(.all_plots$parc)
  .all_plots$gp   <- as.character(.all_plots$gp)

  ## species data
  .all_spp <-
    data.frame(sp = character(), x = numeric(), val = numeric(), id = character())
  for (i in 1:nrow(.spp)) {
    .tmp <-
      cbind.data.frame(colnames(.spp), 1:ncol(.spp), .spp[i, ], paste("gp", i, sep = ""))
    .all_spp <- rbind.data.frame(.all_spp, .tmp)
  }
  colnames(.all_spp) <- c("sp", "x", "rel_ab", "gp")
  .all_spp$sp <- as.character(.all_spp$sp)
  .all_spp$gp <- as.character(.all_spp$gp)

  .tidy_dat <- list(plots = .all_plots, spp = .all_spp)

  return(.tidy_dat)
}
