# ------------------------------------------------------------------------------
# generate various files to easily access informations from databases
# ------------------------------------------------------------------------------

source("functions/gen_data_infos.R")

# ------------------------------------------------------------------------------
# get cultures from carre.parc and year

files <- list.files("data/generated", "weeds",
                    full.names = TRUE)

tab <- matrix("", 0, 3)
all.sp <- character()
for (file in files) {
  a <- read.csv(file, sep = ";", stringsAsFactors = FALSE)
  colnames(a) <- tolower(gsub("é|è", "e", colnames(a)))

  tab <- rbind(tab, cbind(a$year, a$carre.parc, a$crop.analyses))
  all.sp <- c(all.sp, a$sp)
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


# ------------------------------------------------------------------------------
# get traits informations
# get infos on SLA, Plant Height and Seed Mass from table compiled by Bérenger
# Bourgeois.
# When dealing with non present species or genus only identified species, we
# take the average of sepcies of the same genus, if possible.

# read table
ref <- read.csv("data/raw/jauz_20170403_sp_with_traits.csv", sep = " ",
                stringsAsFactors = FALSE)
colnames(ref)[1] <- "sp"
sp <- unique(all.sp)

# get species names to the same format as in the reference table
sp <- gsub("-", " ", sp)
sp <- gsub("  ", " ", sp)
sp <- toupper(sp)

# compute traits values using the 'get_trait_value' function
sla.val <- sapply(sp, get_trait_value, ref = ref, trait = "SLA")
sm.val  <- sapply(sp, get_trait_value, ref = ref, trait = "SM")
ph.val  <- sapply(sp, get_trait_value, ref = ref, trait = "PH")

# combine them in a single data frame
all.trait.val <- cbind.data.frame(sp = names(sla.val),
                                  SLA = as.numeric(sla.val),
                                  PH  = as.numeric(ph.val),
                                  SM  = as.numeric(sm.val))

write.csv(all.trait.val, "data/generated/traits_val.csv", row.names = FALSE)
