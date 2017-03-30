# Script pour calculer les richesses à 20m²
#
# Sabrina Gaba
# crée le 6 juillet 2016
# modifié le 
###########################################################

#####Charge le fichier des relevés de flore 2014
data2014=read.csv("data/raw/monitoring2014.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,encoding="latin1")
#data2014=read.csv("monitoring2014_corrigéparMarylin_29082016.csv", sep=";", dec= "," ,
                  #stringsAsFactors=FALSE,h=T)
#vérification des données
dim(data2014) #dimension du fichier de donnees
head(data2014) #premieres lignes
summary(data2014) #synthese des donnees
var <- colnames(data2014)
unique(data2014$Crop.Analyses)

# Remove the field sampled two times:
data2014 = data2014[-which(data2014$carré.parc == "5641-6103b"), ] 

# There is a problem with these fields (cf the shapefile on QGIS: the real sampled field does not correspond to carré.parc)
data2014[which(data2014$carré.parc == "10042-13632"), ]$carré.parc =	"10042-10632"
data2014[which(data2014$carré.parc == "1717-1460"), ]$carré.parc =	"1717-6641"
data2014[which(data2014$carré.parc == "3708-3953"), ]$carré.parc =	"3708-3991"
data2014[which(data2014$carré.parc == "7459-8252"), ]$carré.parc =	"7459-8258"
data2014[which(data2014$carré.parc == "9829-10433"), ]$carré.parc =	"9829-10488"

data2014b=data2014

###Correction des noms d'espèces adventices
##code Joël
source("util/modifs_fichier.R", encoding = "latin1")
data2014=modifs_fichier(tab=data2014)


#############################################################
#####Creation d'un jeu de donnees avec les cultures annuelles
#############################################################

# On conserve Year, date, Nb_parcelle, carré.parc, Par.interf, 
#Crop.Analyses, pt, lat, LONG, Espèce_origin, abondance, ParPoint
kept <- c(3:12,19)

weeds2014 <- data2014[, kept ]
colnames(weeds2014)
weeds2014C<-subset(weeds2014,weeds2014$Crop.Analyses!="luzerne")
weeds2014C<-subset(weeds2014C,weeds2014C$Crop.Analyses!="prairie")


##weeds2014C$Crop.Analyses[weeds2014C$pt==1]
sort(unique(weeds2014C$pt))
weeds2014C$pt[weeds2014C$pt=="6q" ] <- "6d"
 
######################################################################            
### Mise en forme pour matrice sites x especes et calcul par quadrat
######################################################################

## Another lines will be the field number and position
#"carré-parc" :: numero de parcelle
#"Par.interf" : in, pa1, pa2
#"Crop.Analyses" : cereal, colza, tournesol
#"lat"
#"LONG"
colnames(weeds2014C)
length(unique(weeds2014C$"carré.parc"))
## 156 parcelles

unique(weeds2014C$Par.interf)
#[1] "in"  "pa1" "pa2" "pa3" 
#retrait de la 3ème ligne en plein champ
weeds2014C=subset(weeds2014C,weeds2014C$Par.interf!="pa3")

length(unique(weeds2014C$Crop.Analyses))
## 10 cultures

length(unique(weeds2014$Espèce_origin))
## 215 espèces soit 215 colonnes dans la matrice

weeds2014C1 <- cbind(weeds2014C, as.factor(weeds2014C$Espèce_origin))
colnames(weeds2014C1) [12] <- "sp"

## Separe les noms des quadrats en quadrats et sub-quadrats
#nchar(weeds2014C1$pt)
## Nombre de quadrats
#substr(weeds2014C1[nchar(weeds2014C1$pt)==2, ]$pt, 0, 1)
#substr(weeds2014C1[nchar(weeds2014C1$pt)==3, ]$pt, 0, 2)
## Nombre de sub-quadrats
#substr(weeds2014C1[nchar(weeds2014C1$pt)==2, ]$pt, 2, 3)
#substr(weeds2014C1[nchar(weeds2014C1$pt)==3, ]$pt, 3, 4)

#### 3 niveaux : field, plot (quadrat 1m²) et 
### quadrat (équivalent aux sous-quadrats a, b, c, d)

colnames(weeds2014C1)
#[1] "Year"          "date"          "No_parcelle"   "carré.parc"    "Par.interf"   
#[6] "Crop.Analyses" "pt"            "lat"           "LONG"          "Espèce_origin"
#[11] "abondance"     "ParPoint"      "sp" 

weeds1 <- weeds2014C1[nchar(weeds2014C1$pt)==2, ]
weeds2 <- weeds2014C1[nchar(weeds2014C1$pt)==3, ]

plot <- substr(weeds1$pt, 0, 1)
quadrat <- substr(weeds1$pt, 2, 3)
weeds1 <- cbind(weeds1, plot)
weeds1 <- cbind(weeds1, quadrat )
head(weeds1)

plot <- substr(weeds2$pt, 0, 2)
quadrat <- substr(weeds2$pt, 3, 4)
weeds2 <- cbind(weeds2, plot)
weeds2 <- cbind(weeds2, quadrat )
head(weeds2)

weeds <- rbind(weeds1, weeds2)
head(weeds, 15)

write.table(weeds, "data/generated/weeds2014.csv", sep = ";")


###########################################
## Aggregation des especes
###########################################
weeds <- read.table("data/generated/weeds2014.csv", sep = ";")
test <- aggregate(data.frame(abondance = weeds$abondance), 
                  by = list(sp = weeds$sp, quadrat = weeds$quadrat, 
                            plot=weeds$plot, position = weeds$Par.interf, 
                            carre.parc = weeds$"carré.parc",crop=weeds$Crop.Analyses), sum)

##Il y a des doublons dans le jeu de donnees 
##(somme des abondances par sous-quadrat ne peut pas être supérieure à 1)
nrow(test[test$abondance > 1, ])
# 33

## Juste set those lines with 1 value (the original data must be fixed after). 
test[test$abondance > 1, ]$abondance <- 1

##nrow(test)
# 29554

## Création de la matrice par parcelles
# tab <- xtabs(abondance~ sp + quadrat + plot + position + carre.parc + crop, test)


#############################################################################
## Matrice site x especes avec ligne pour les sous-quadrats vides
#############################################################################
# I made some optimisation: manipulating a matrix is considerably faster than
# manipulating a dataframe so I seperate the ids in a character vector in order
# to keep a numeric matrix. Ids and data are bound next.
source("functions/format_flora.R", encoding = "utf8")

# fix dirty data
test$quadrat <- tolower(test$quadrat)
test$plot[test$plot == " 1"] <- "1"

A <- transpose_df(tab=test, n_quadras = 10, n_subqd = 4, pos = c("pa1", "pa2", "in"))
#(A[which(A$carre.parc == "10042-10663" & A$sp == "Geranium-dissectum"), ]) 
 
write.table(A, "data/generated/transpose_abondance_per_sousquadrat2014.csv", sep = ";")

# ------------------------------------------------------------------------------
# I commented the stuff below because I don't use it and it is quite slow to run
# ------------------------------------------------------------------------------

# #######################################################
# ## Matrice quadrat (en lignes) x (especes en colonnes)
# #######################################################
# 
# B <- matrix(ncol=4 + length(unique(A$sp)), nrow=100*length(unique(A$carre.parc)), data = 0)
# B<-data.frame(B)
# colnames(B) <- c("field", "position", "plot", "quadrat", unique(as.character(A$sp))) 
# B$quadrat = rep(c("a", "b", "c", "d"), 25*length(unique(A$carre.parc)))
# B$plot = rep(c(rep(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4), rep(7, 4), rep(8, 4), rep(9, 4), rep(10, 4)), 2), 
#                rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4)), length(unique(A$carre.parc)))
# B$position = rep(c(rep("pa1", 40), rep("pa2", 40), rep("in", 20)), length(unique(A$carre.parc)))
# 
# fieldB = c()
# for ( i in 1: length(unique(A$carre.parc))) {
#   fieldB = c(fieldB, rep(unique(A$carre.parc)[i], 100))}
# B$field = fieldB
# 
# for (i in 1:nrow(A)) {
#   for (j in 4:(ncol(A)-1)) {
#     
#     if (A[i, j] > 0) {
#       spX <- A[i, "sp"]
#       fieldX <- A[i, "carre.parc"]
#       positionX <- A[i, "position"]
#       plotX <- substr(colnames(A)[j], 2, 2)
#       quadratX <- substr(colnames(A)[j], 3, 3)
#       abondance = A[i, j] 
# 
#       B[B$field == fieldX & B$position == positionX & B$plot == plotX &
#         B$quadrat == quadratX, colnames(B)==spX] <- abondance
#     }
#   }
# }
# 
# head(B)
# write.table(B, "data/generated/transpose_species_abondance_per_sousquadrat2014.csv", sep = ";")
# 
# ###############################################################################
# ##### Table avec les abondances par quadrats (ici plot)
# ###############################################################################
# basics <- test
# colnames(basics)
# # "sp" "quadrat"  "plot" "position"  "culture"  "carre.parc" "crop" "abondance"
# test <- aggregate(data.frame(abondance = basics$abondance), 
#                   by = list(sp = basics$sp, plot=basics$plot, position =
#                             basics$position, carre.parc =
#                             basics$carre.parc),
#                   sum)
# colnames(test)
# test$frequency <- test$abondance/4 ##freq de l'esp sur le 4 ss-quadrats
# 
# 
# ##Enregistrement du tableau par sous quadrat
# head(test)
# write.table(test, "data/generated/abondance_per_quadrat2014.csv", sep = ";")
# test <- read.csv("data/generated/abondance_per_quadrat2014.csv", sep = ";",
#                  stringsAsFactors = FALSE)
# 
# #############################################################################
# ## Matrice site x especes avec ligne pour les quadrats vides
# #############################################################################
# #length(unique(test$sp))
# #183 species
# 
# #length(unique(test$carre.parc))
# #156 different sampling of fields
# 
# ### Prepare an empty matrix filled with 0 (for 0 abundance observed)
# nrowA <- length(unique(test$carre.parc)) * length(unique(test$sp)) * 3
# #183sp*156parc * 3 = 85644 lignes
# 
# A <- matrix(ncol=13, nrow=nrowA , data = rep(0, 13*nrowA ))
# A<-data.frame(A)
# colnames(A) <- c("sp", "carre.parc", "position", "q1","q2","q3","q4","q5","q6","q7","q8","q9","q10")
# #dim(A)
# #head(A)
# 
# ## Init the species, field number, and then position in A
# ##To understand, just try that : rep (c(1,2,3), 3)
# 
# A$sp <- rep(unique(test$sp), length(unique(test$carre.parc)) *3)
# carre.parc <- c()
# for (i in 1:length(unique(test$carre.parc))) {
#   carre.parc = c(carre.parc, rep(unique(test$carre.parc)[i], length(unique(test$sp))*3)) }
# A$carre.parc = carre.parc
# A$position <- rep(c(rep("pa1", length(unique(test$sp))), rep("pa2", length(unique(test$sp))), rep("in", length(unique(test$sp)))), length(unique(test$carre.parc)))
# A$done <- rep(0, nrowA )
# 
# ## Remplis les quadrats vides (~7 min)
# for (i in 1:length(test$position)) {
#   spX <- test[i, "sp"]
#   fieldX <- test[i, "carre.parc"]
#   positionX <- test[i, "position"]
#   plot <- test[i, "plot"] 
#   abondance <- test[i, "abondance"]
#   code=paste("q",plot, sep="")
#   
#   A[A$sp == spX & A$carre.parc == fieldX & A$position == positionX, colnames(A)==code]<- abondance  
# }
# 
# head(A, 25)
# write.table(A, "data/generated/transpose_abondance_per_quadrat2014.csv", sep = ";")
# 
# #########################################################################
# # Matrice site x especes par parcelle (plein champ/pas interface)
# #########################################################################
# basics1=subset(basics,basics$position!="in")
# test <- aggregate(data.frame(abondance = basics1$abondance), 
#                   by = list(sp = basics1$sp,carre.parc = basics1$carre.parc,
#                             crop=basics1$crop),sum)
# colnames(test)
# 
# write.table(test, "data/generated/transpose_abondance_per_fieldcore2014.csv", sep = ";")
# 
# #########################################################################
# # Matrice site x especes par parcelle (plein champ/interface)
# #########################################################################
# basics2=basics
# basics2$position = substr(basics2$position, 1, 2)
# test <- aggregate(data.frame(abondance = basics2$abondance), 
#                   by = list(sp = basics2$sp,carre.parc = basics2$carre.parc,
#                             position = basics2$position),sum)
# 
# colnames(test)
# 
# write.table(test, "data/generated/transpose_abondance_per_position2014.csv", sep = ";")
# 
# #########################################################################
# # Matrice site x especes par parcelle 
# #########################################################################
# test <- aggregate(data.frame(abondance = basics$abondance), 
#                   by = list(sp = basics$sp, carre.parc = basics$carre.parc,
#                             crop=basics$crop),sum)
# colnames(test)
# 
# write.table(test, "data/generated/transpose_abondance_per_field2014.csv", sep = ";")

# ------------------------------------------------------------------------------
# End of commented stuff
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Estimation des abondances
# Problème: les abondances sont notées 0/1 (absence/présence).

# ------------------------------------------------------------------------------
# Estimation des abondances par une loi de Poisson par sous-quadrat

source("functions/abundance.R", encoding = "utf8")

weeds <- read.csv("data/generated/transpose_abondance_per_sousquadrat2014.csv",
                  sep = ";", stringsAsFactors = FALSE)

# récpérer uniquement les parcelles et supprimer la variable 'done'
weeds <- weeds[weeds$position != "in", 1:ncol(weeds) - 1]

abond_per_plot <- estim_abundance01(weeds, surf = 0.25)

write.csv(abond_per_plot, "data/generated/abondt_per_subquadra_2014_binomiale.csv",
          row.names = TRUE)

# ------------------------------------------------------------------------------
# En regroupant les sous-quadras

abond_per_plot <- estim_abundance01(weeds, surf = 1, gp.subquadra = TRUE)

write.csv(abond_per_plot, "data/generated/abondt_per_quadra_2014_binomiale.csv",
          row.names = TRUE)

# ------------------------------------------------------------------------------
# Passage des notes en log2
# On considère ici qu'il ne peut y avoir plus d'une plante par sous-quadra. On
# regroupe donc les sous-quadras en faisant la somme des indices d'abondances et
# si une note est > 2, on la remplace par 2.

abond_per_plot <- estim_abundance01(weeds, surf = 1, gp.subquadra = T, base2 = T)

write.csv(abond_per_plot, "data/generated/abondt_per_quadra_2014_base2.csv",
          row.names = TRUE)
