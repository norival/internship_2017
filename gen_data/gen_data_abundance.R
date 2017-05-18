# ------------------------------------------------------------------------------
# code to generate data needed for abundance estimation
# ------------------------------------------------------------------------------

# generate 'weeds<year>.csv'
source("util/code_annee_tot.R", encoding = "latin1")

# generate 'transpose_abundance*.csv'
for (year in c(2006:2016)) {
  if (year == 2012) next
  print(paste("year", year))
  filename <- paste("util/code", year, ".R", sep = "")
  source(filename, encoding = "latin1")
}
