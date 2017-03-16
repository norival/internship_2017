# summary des données flores
files <- list.files("data/generated", pattern = "abondt_per_quadra", full.names = TRUE)
files <- files[!grepl("binomiale", files)]

surf <- 20
for (file in files) {
  year <- strsplit(file, "[_\\.]")[[1]][4]
  var_name <- paste("weeds", year, sep = "")

  assign(var_name, read.csv(file, stringsAsFactor = FALSE, row.names = 1) * surf)
}


# ------------------------------------------------------------------------------
# 2006
a <- weeds2006

# removes interface for which abundance have not been estimated
a <- a[!grepl("-In", rownames(a)),]

# replace very small values by 0
a[a <= 3.5e-09] <- 0

# remove species that are not present
a <- a[, apply(a, 2, sum) != 0]

# nb plots
plots <- unique(gsub("-2006-Pa", "", rownames(a)))
n_plots <- length(plots)

# present species
sp <- colnames(a)

# nb species per plot
mean(apply(a, 1, function(x) sum(x != 0)))

# percentage of presence per species
perc_sp <- apply(a, 2, function(x) sum(x != 0) / length(x) * 100)
length(perc_sp[perc_sp >= 50])
perc_sp[perc_sp >= 25 & perc_sp < 50]

`%incl%` <- function(x, range)  {
  res <- ifelse(x >= range[1] & x < range[2],
                TRUE,
                FALSE)
  return(res)
}
23 %incl% c(24, 40)

# classify species by percentage:
#   [25;100] -> abundant
#   [5;25[   -> intermediate
#   [0;5[    -> rare
a$status <- ""
