# ------------------------------------------------------------------------------
# generate files needed for LDA analyses
# ------------------------------------------------------------------------------

# -- read the data -------------------------------------------------------------
files <- list.files("data/generated", pattern = "abond_per_plot_[[:digit:]]{4}",
                    full.names = TRUE)

surf <- 20
all_years <- numeric()
for (file in files) {
  year      <- strsplit(file, "[_\\.]")[[1]][4]
  all_years <- c(all_years, year)
  var_name  <- paste("weeds", year, sep = "")

  assign(var_name, read.csv(file, stringsAsFactor = FALSE, row.names = 1) * surf)
}


# -- group and clean values ----------------------------------------------------
# group all years together

allsp <- character()
allpc <- character()

for (year in all_years) {
  tmp <- get(paste("weeds", year, sep = ""))
  allsp <- c(allsp, colnames(tmp))
  allpc <- c(allpc, paste(year, rownames(tmp), sep = "_"))
}

allsp <- unique(allsp)
allpc <- unique(allpc)

mat <- matrix(0, nrow = length(allpc), ncol = length(allsp))
colnames(mat) <- allsp
rownames(mat) <- allpc

for (pc in rownames(mat)) {
  year   <- substr(pc, 1, 4)
  pcorig <- substr(pc, 6, nchar(pc))
  tmp    <- get(paste("weeds", year, sep = ""))

  # set minimum values to 0
  tmp[tmp == min(tmp)] <- 0

  for (sp in colnames(tmp)) {
    mat[pc, sp] <- tmp[pcorig, sp]
  }
}

# remove interface
dat <- mat[!(grepl("In", rownames(mat))),]

# truncate the values to the smaller greater integer
dat <- ceiling(dat)

# remove plots with no plant because LDA function cannot handle it
dat <- dat[rowSums(dat) != 0,]

# write file
write.csv(dat, "data/generated/abond_per_plot_all_years.csv")
