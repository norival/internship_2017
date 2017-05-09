# Script pour calculer les richesses à 20m²
#
# Sabrina Gaba
# crée le 6 juillet 2015
# modifié le 
###########################################################

###########################################
## Aggregation des especes
###########################################
weeds <- read.table("data/generated/weeds2015.csv", sep = ";")

# remove 'luzerne' and 'prairie' and 'trèfle'
weeds <- weeds[weeds$Crop.Analyses != "luzerne",]
weeds <- weeds[weeds$Crop.Analyses != "prairie",]
weeds <- weeds[weeds$Crop.Analyses != "trèfle",]

test <- aggregate(data.frame(abondance = weeds$abondance), 
                  by = list(sp = weeds$sp, quadrat = weeds$quadrat, 
                            plot=weeds$plot, position = weeds$Par.interf,
                            crop=weeds$Crop.Analyses,
                            carre.parc = weeds$"carré.parc"), sum)

##Il y a des doublons dans le jeu de donnees 
##(somme des abondances par sous-quadrat ne peut pas être supérieure à 1)
nrow(test[test$abondance > 1, ])
# 50

## Juste set those lines with 1 value (the original data must be fixed after). 
test$abondance[test$abondance > 1] <- 1

##nrow(test)
# 29554

## Création de la matrice par parcelles
# tab <- xtabs(abondance~ sp + quadrat + plot + position + carre.parc + crop, test)
 
#############################################################################
## Matrice site x especes avec ligne pour les sous-quadrats vides
#############################################################################
source("functions/format_flora.R", encoding = "utf8")

# fix dirty data
test$quadrat <- tolower(test$quadrat)
test$plot[test$plot == " 1"] <- "1"

A <- transpose_df(tab=test, n_quadras = 10, n_subqd = 4, pos = c("pa1", "pa2", "in"))

write.table(A, "data/generated/transpose_abondance_per_sousquadrat2015.csv", sep = ";")


# ------------------------------------------------------------------------------
# I commented the stuff below because I don't use it and it is quite slow to run
# ------------------------------------------------------------------------------

#######################################################
## Matrice quadrat (en lignes) x (especes en colonnes)
#######################################################

# B <- matrix(ncol=4 + length(unique(A$sp)), nrow=100*length(unique(A$carre.parc)), data = 0)
# B<-data.frame(B)
# colnames(B) <- c("field", "position", "plot", "quadrat", unique(as.character(A$sp))) 
# B$quadrat = rep(c("a", "b", "c", "d"), 25*length(unique(A$carre.parc)))
# B$plot = rep(c(rep(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4), rep(7, 4), rep(8, 4), rep(9, 4), rep(10, 4)), 2), 
#                rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4)), length(unique(A$carre.parc)))
# B$position = rep(c(rep("pa1", 40), rep("pa2", 40), rep("in", 20)), length(unique(A$carre.parc)))

# fieldB = c()
# for ( i in 1: length(unique(A$carre.parc))) {
#   fieldB = c(fieldB, rep(unique(A$carre.parc)[i], 100))}
# B$field = fieldB

# for (i in 1:nrow(A)) {
#   for (j in 4:(ncol(A)-1)) {
    
#     if (A[i, j] > 0) {
      
#       spX <- A[i, "sp"]
#       fieldX <- A[i, "carre.parc"]
#       positionX <- A[i, "position"]
#       plotX <- substr(colnames(A)[j], 2, 2)
#       quadratX <- substr(colnames(A)[j], 3, 3)
#       abondance = A[i, j] 
      
#       B[B$field == fieldX & B$position == positionX & B$plot == plotX & B$quadrat == quadratX, colnames(B)==spX]<- abondance  }}}

# head(B)
# write.table(B, "data/generated/transpose_species_abondance_per_sousquadrat2015.csv", sep = ";")


# #############################################################################################
# ##### Table avec les abondances par quadrats (ici plot)
# ##################################################################################################
# basics <- test
# colnames(basics)
# # "sp" "quadrat"  "plot" "position"  "culture"  "carre.parc" "crop" "abondance"
# test <- aggregate(data.frame(abondance = basics$abondance), 
#                   by = list(sp = basics$sp, plot=basics$plot,
#                             position = basics$position, 
#                             carre.parc = basics$carre.parc,
#                             crop=basics$crop),sum)
# colnames(test)
# test$frequency <- test$abondance/4 ##freq de l'esp sur le 4 ss-quadrats


# ##Enregistrement du tableau par sous quadrat
# head(test)
# write.table(test, "data/generated/abondance_per_quadrat2015.csv", sep = ";")

# #############################################################################
# ## Matrice site x especes avec ligne pour les quadrats vides
# #############################################################################
# #length(unique(test$sp))
# #183 species

# #length(unique(test$carre.parc))
# #156 different sampling of fields

# ### Prepare an empty matrix filled with 0 (for 0 abundance observed)
# nrowA <- length(unique(test$carre.parc)) * length(unique(test$sp)) * 3
# #183sp*156parc * 3 = 85644 lignes

# A <- matrix(ncol=13, nrow=nrowA , data = rep(0, 13*nrowA ))
# A<-data.frame(A)
# colnames(A) <- c("sp", "carre.parc", "position", "q1","q2","q3","q4","q5","q6","q7","q8","q9","q10")
# #dim(A)
# head(A)

# ## Init the species, field number, and then position in A
# ##To understand, just try that : rep (c(1,2,3), 3)

# A$sp <- rep(unique(test$sp), length(unique(test$carre.parc)) *3)
# carre.parc <- c()
# for (i in 1:length(unique(test$carre.parc))) {
#   carre.parc = c(carre.parc, rep(unique(test$carre.parc)[i], length(unique(test$sp))*3)) }
# A$carre.parc = carre.parc
# A$position <- rep(c(rep("pa1", length(unique(test$sp))), rep("pa2", length(unique(test$sp))), rep("in", length(unique(test$sp)))), length(unique(test$carre.parc)))
# A$done <- rep(0, nrowA )

# ## Remplis les quadrats vides (~7 min)
# for (i in 1:length(test$position)) {
#   spX <- test[i, "sp"]
#   fieldX <- test[i, "carre.parc"]
#   positionX <- test[i, "position"]
#   plot <- test[i, "plot"] 
#   abondance <- test[i, "abondance"]
#   code=paste("q",plot, sep="")
  
#   A[A$sp == spX & A$carre.parc == fieldX & A$position == positionX, colnames(A)==code]<- abondance  
# }

# head(A, 25)
# write.table(A, "data/generated/transpose_abondance_per_quadrat2015.csv", sep = ";")



# #########################################################################
# # Matrice site x especes par parcelle (plein champ/pas interface)
# #########################################################################
# basics1=subset(basics,basics$position!="in")
# test <- aggregate(data.frame(abondance = basics1$abondance), 
#                   by = list(sp = basics1$sp,carre.parc = basics1$carre.parc,
#                             crop=basics1$crop),sum)
# colnames(test)

# write.table(test, "data/generated/transpose_abondance_per_fieldcore2015.csv", sep = ";")

# #########################################################################
# # Matrice site x especes par parcelle (plein champ/interface)
# #########################################################################
# basics2=basics
# basics2$position = substr(basics2$position, 1, 2)
# test <- aggregate(data.frame(abondance = basics2$abondance), 
#                   by = list(sp = basics2$sp,carre.parc = basics2$carre.parc,
#                             position = basics2$position),sum)

# colnames(test)

# write.table(test, "data/generated/transpose_abondance_per_position2015.csv", sep = ";")

# #########################################################################
# # Matrice site x especes par parcelle 
# #########################################################################
# test <- aggregate(data.frame(abondance = basics$abondance), 
#                   by = list(sp = basics$sp, 
#                             carre.parc = basics$carre.parc,
#                             crop=basics$crop),sum)
# colnames(test)

# write.table(test, "data/generated/transpose_abondance_per_field2015.csv", sep = ";")

#################################################################

# ------------------------------------------------------------------------------
# End of commented stuff
# ------------------------------------------------------------------------------
