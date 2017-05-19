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

tidy_lda_post <- function(mod) {
  post <- posterior(mod)

  plots <- post$topics
  spp   <- post$terms

  # group the data in a tidy manner so it can be easily plotted
  ## plots data
  all_plots <- matrix(0, 0, 3)
  for (i in 1:ncol(plots)) {
    tmp <- cbind(rownames(plots), paste("gp", i, sep = ""), plots[, i])
    all_plots <- rbind(all_plots, tmp)
  }
  rownames(all_plots) <- 1:nrow(all_plots)
  all_plots <- data.frame(all_plots, stringsAsFactors = FALSE)
  colnames(all_plots) <- c("parc", "group", "percent")
  all_plots$percent   <- as.numeric(all_plots$percent)

  ## species data
  all_spp <- matrix(0, 0, 3)
  for (i in 1:nrow(spp)) {
    tmp <- cbind(colnames(spp), paste0("gp", i), spp[i,])
    all_spp <- rbind(all_spp, tmp)
  }
  rownames(all_spp) <- 1:nrow(all_spp)
  all_spp <- data.frame(all_spp, stringsAsFactors = FALSE)
  colnames(all_spp) <- c("sp", "group", "rel_ab")
  all_spp$rel_ab    <- as.numeric(all_spp$rel_ab)

  return(list(plots = all_plots, spp = all_spp))
}
