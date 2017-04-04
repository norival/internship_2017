tidy_lda_post <- function(.post) {

  .plots  <- .post$topics
  .spp    <- .post$terms

  # group the data in a tidy manner so it can be easily plotted
  ## plots data
  .all_plots <- data.frame(x = numeric(), val = numeric(), id = character())
  for (i in 1:ncol(.plots)) {
    .tmp <-
      cbind.data.frame(1:nrow(.plots), .plots[, i], paste("gp", i, sep = ""))
    .all_plots <- rbind.data.frame(.all_plots, .tmp)
  }
  colnames(.all_plots) <- c("x", "val", "gp")

  ## species data
  .all_spp <- data.frame(x = numeric(), val = numeric(), id = character())
  for (i in 1:nrow(.spp)) {
    .tmp <-
      cbind.data.frame(1:ncol(.spp), .spp[i, ], paste("gp", i, sep = ""))
    .all_spp <- rbind.data.frame(.all_spp, .tmp)
  }
  colnames(.all_spp) <- c("x", "rel_ab", "gp")

  .tidy_dat <- list(plots = .all_plots, spp = .all_spp)

  return(.tidy_dat)
}
