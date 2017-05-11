par_lda <- function(tab, groups, control, datname, path, ncores = 3) {
  # wrapper function to run in parallel LDA fitting on several groups.
  # models are saved in 'path' and returned as a list. Fitting can be very long
  # and sometimes parallelization crashes, so, writing the result to a file a
  # safety measure.
  # - tab is a matrix site*species
  # - groups is a vector containing every group to test
  # - control is a list of control parameters passed to the LDA function
  # - datname is the name given to the data to be saved, constructed like this:
  #   'lda_year_2006_K_groups' where K is the place where the number of groups
  #   will be put (it is replaced automatically for each group)
  # - path is the path where the data is stored

  library(topicmodels)
  library(doSNOW)

  # adds trailing '/' to path if not present
  if (!grepl("/$", path)) {
    path <- paste0(path, "/")
  }

  cl <- makeCluster(ncores, outfile = "")
  registerDoSNOW(cl)

  all_ldas <- foreach(k = groups, .export = "LDA") %dopar% {

    # get file and model name
    modelname <- gsub("K", k, datname)
    filename  <- paste0(path, modelname, ".Rdata")
    assign(modelname, LDA(tab, k = k, method = "Gibbs", control = control))

    save(list = modelname, file = filename)
    return(get(modelname))
  }

  stopCluster(cl)

  return(all_ldas)

}

# ------------------------------------------------------------------------------

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
