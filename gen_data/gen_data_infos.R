# ------------------------------------------------------------------------------
# generate various files to easily access informations from databases
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# get cultures from carre.parc and year

files <- list.files("data/generated", "weeds",
                    full.names = TRUE)

tab <- matrix("", 0, 3)
for (file in files) {
  a <- read.csv(file, sep = ";", stringsAsFactors = FALSE)
  colnames(a) <- tolower(gsub("é|è", "e", colnames(a)))

  tab <- rbind(tab, cbind(a$year, a$carre.parc, a$crop.analyses))
}

tab <- as.data.frame(tab, stringsAsFactors = FALSE)
colnames(tab) <- c("year", "carre.parc", "crop.analyses")
tab <- tab[!duplicated(paste0(tab$year, tab$carre.parc)),]
tab$crop.analyses <- tolower(tab$crop.analyses)
tab$crop.analyses[tab$crop.analyses == "maïs"]      <- "maize"
tab$crop.analyses[tab$crop.analyses == "tournesol"] <- "sunflower"
tab$crop.analyses[tab$crop.analyses == "colza"]     <- "osr"
tab$crop.analyses[tab$crop.analyses == "trèfle"]    <- "trefle"
tab$crop.analyses[tab$crop.analyses == "luzerne"]   <- "lucerne"
tab$crop.analyses[tab$crop.analyses == "prairie"]   <- "grassl"

write.csv(tab, "data/generated/corres_parc_crop.csv", row.names = FALSE)
