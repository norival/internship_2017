# Script pour calculer les richesses à 20m²
#
# Sabrina Gaba
# crée le 6 juillet 2016
# modifié le 
###########################################################

#####Charge le fichier des relevés de flore 2016
data2016=read.csv("data/raw/monitoring2016.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")
#vérification des données
dim(data2016) #dimension du fichier de donnees
head(data2016) #premieres lignes
summary(data2016) #synthese des donnees
var <- colnames(data2016)

# Complete the Crop.Analyses field with OSC (cf onglet infos in the bota 2016 file)
unique(data2016$Crop.Analyses)
data2016[which(data2016$OSC==41),]$Crop.Analyses = "blé"
data2016[which(data2016$OSC==40),]$Crop.Analyses = "céréale"
data2016[which(data2016$OSC==50),]$Crop.Analyses = "colza"
data2016[which(data2016$OSC==61),]$Crop.Analyses = "lin"
data2016[which(data2016$OSC==76),]$Crop.Analyses = "lin/lentille"
data2016[which(data2016$OSC==25),]$Crop.Analyses = "luzerne"
data2016[which(data2016$OSC==71),]$Crop.Analyses = "maïs"
data2016[which(data2016$OSC==48),]$Crop.Analyses = "mél ce/leg"
data2016[which(data2016$OSC==44),]$Crop.Analyses = "orge hiver"
data2016[which(data2016$OSC==77),]$Crop.Analyses = "pavot"
data2016[which(data2016$OSC==62),]$Crop.Analyses = "pois"
data2016[which(data2016$OSC==11),]$Crop.Analyses = "prairie"
data2016[which(data2016$OSC==72),]$Crop.Analyses = "tournesol"
data2016[which(data2016$OSC==26),]$Crop.Analyses = "trèfle"
data2016[which(data2016$OSC==42),]$Crop.Analyses = "ble barbu"
unique(data2016$Crop.Analyses)  


###Correction des noms d'espèces adventices
##code Joël
source("util/modifs_fichier.R", encoding = "latin1")
data2016=modifs_fichier(tab=data2016)

#############################################################
#####Creation d'un jeu de donnees avec les cultures annuelles
#############################################################

# On conserve Year, date, Nà_parcelle, carré.parc, Par.interf, 
#Crop.Analyses, pt, lat, LONG, Espèce_origin, abondance, ParPoint
kept <- c(3:4, 6:9, 11:14, 22, 34)

weeds2016 <- data2016[, kept ]
colnames(weeds2016)
unique(weeds2016$Crop.Analyses)
weeds2016C<-subset(weeds2016,weeds2016$Crop.Analyses!="luzerne")
weeds2016C<-subset(weeds2016C,weeds2016C$Crop.Analyses!="prairie")
weeds2016C<-subset(weeds2016C,weeds2016C$Crop.Analyses!="trèfle")
weeds2016C<-subset(weeds2016C,weeds2016C$Crop.Analyses!="mél ce/leg")
weeds2016C<-subset(weeds2016C,weeds2016C$Crop.Analyses!="lin")
weeds2016C<-subset(weeds2016C,weeds2016C$Crop.Analyses!="pavot")
weeds2016C<-subset(weeds2016C,weeds2016C$Crop.Analyses!="lin/lentille")
unique(weeds2016C$Crop.Analyses)

weeds2016C$Crop.Analyses[weeds2016C$Crop.Analyses=="ble barbu"]="cereal"
weeds2016C$Crop.Analyses[weeds2016C$Crop.Analyses=="céréale"]="cereal"
weeds2016C$Crop.Analyses[weeds2016C$Crop.Analyses=="blé"]="cereal"
weeds2016C$Crop.Analyses[weeds2016C$Crop.Analyses=="orge hiver"]="cereal"
unique(weeds2016C$Crop.Analyses)

##weeds2016C$Crop.Analyses[weeds2016C$pt==1]
sort(unique(weeds2016C$pt)) # good !


######################################################################            
### Mise en forme pour matrice sites x especes et calcul par quadrat
######################################################################

## Another lines will be the field number and position
#"carré-parc" :: numero de parcelle
#"Par.interf" : in, pa1, pa2
#"Crop.Analyses" : cereal, colza, tournesol
#"lat"
#"LONG"
colnames(weeds2016C)
length(unique(weeds2016C$"carré.parc"))
## 163 parcelles

unique(weeds2016C$Par.interf)
#[1] "in"  "pa1" "pa2" "pa3" 

length(unique(weeds2016C$Crop.Analyses))
## 5 cultures

length(unique(weeds2016$Espèce_origin))
## 282 espèces soit 215 colonnes dans la matrice

weeds2016C1 <- cbind(weeds2016C, as.factor(weeds2016C$Espèce_origin))
colnames(weeds2016C1) [13] <- "sp"

## Separe les noms des quadrats en quadrats et sub-quadrats
nchar(weeds2016C1$pt)
## Nombre de quadrats
substr(weeds2016C1[nchar(weeds2016C1$pt)==2, ]$pt, 0, 1)
substr(weeds2016C1[nchar(weeds2016C1$pt)==3, ]$pt, 0, 2)
## Nombre de sub-quadrats
substr(weeds2016C1[nchar(weeds2016C1$pt)==2, ]$pt, 2, 3)
substr(weeds2016C1[nchar(weeds2016C1$pt)==3, ]$pt, 3, 4)

#### 3 niveaux : field, plot (quadrat 1m²) et 
### quadrat (équivalent aux sous-quadrats a, b, c, d)

colnames(weeds2016C1)
#[1] "Year"          "date"          "No_parcelle"   "carré.parc"    "Par.interf"   
#[6] "Crop.Analyses" "pt"            "lat"           "LONG"          "Espèce_origin"
#[11] "abondance"     "ParPoint"      "sp" 

weeds1 <- weeds2016C1[nchar(weeds2016C1$pt)==2, ]
weeds2 <- weeds2016C1[nchar(weeds2016C1$pt)==3, ]

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

write.table(weeds, "data/generated/weeds2016.csv", sep = ";")

###########################################
## Aggregation des especes
###########################################
weeds <- read.table("data/generated/weeds2016.csv", sep = ";")
test <- aggregate(data.frame(abondance = weeds$abondance), 
                  by = list(sp = weeds$sp, quadrat = weeds$quadrat, 
                            plot=weeds$plot, position = weeds$Par.interf,
                            crop=weeds$Crop.Analyses,
                            carre.parc = weeds$"carré.parc"), sum)

##Il y a des doublons dans le jeu de donnees 
## certaines données d'abondance sont NA
nrow(test[which(is.na(test$abondance)==TRUE), ]) # 11

## Juste set those lines with 1 value (the original data must be fixed after).
test[which(is.na(test$abondance)==TRUE), ]$abondance <- 1

##(somme des abondances par sous-quadrat ne peut pas être supérieure à 1)
nrow(test[test$abondance > 1, ]) # 13

## Juste set those lines with 1 value (the original data must be fixed after). 
test[test$abondance > 1, ]$abondance <- 1

nrow(test)
# 35822

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
 
write.table(A, "data/generated/transpose_abondance_per_sousquadrat2016.csv", sep = ";")


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
# write.table(B, "data/generated/transpose_species_abondance_per_sousquadrat2016.csv", sep = ";")


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
# write.table(test, "data/generated/abondance_per_quadrat2016.csv", sep = ";")

# #############################################################################
# ## Matrice site x especes avec ligne pour les quadrats vides
# #############################################################################
# length(unique(test$sp))
# #213 species

# length(unique(test$carre.parc))
# #163 different sampling of fields

# ### Prepare an empty matrix filled with 0 (for 0 abundance observed)
# nrowA <- length(unique(test$carre.parc)) * length(unique(test$sp)) * 3
# #213sp*163parc * 3 = 104157 lignes

# A <- matrix(ncol=13, nrow=nrowA , data = rep(0, 13*nrowA ))
# A<-data.frame(A)
# colnames(A) <- c("sp", "carre.parc", "position", "q1","q2","q3","q4","q5","q6","q7","q8","q9","q10")
# dim(A)
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
# write.table(A, "data/generated/transpose_abondance_per_quadrat2016.csv", sep = ";")



# #########################################################################
# # Matrice site x especes par parcelle (plein champ/pas interface)
# #########################################################################
# basics1=subset(basics,basics$position!="in")
# test <- aggregate(data.frame(abondance = basics1$abondance), 
#                   by = list(sp = basics1$sp,carre.parc = basics1$carre.parc,
#                             crop=basics1$crop),sum)
# colnames(test)

# write.table(test, "data/generated/transpose_abondance_per_fieldcore2016.csv", sep = ";")

# #########################################################################
# # Matrice site x especes par parcelle (plein champ/interface)
# #########################################################################
# basics2=basics
# basics2$position = substr(basics2$position, 1, 2)
# test <- aggregate(data.frame(abondance = basics2$abondance), 
#                   by = list(sp = basics2$sp,carre.parc = basics2$carre.parc,
#                             position = basics2$position),sum)

# colnames(test)

# write.table(test, "data/generated/transpose_abondance_per_position2016.csv", sep = ";")

# #########################################################################
# # Matrice site x especes par parcelle 
# #########################################################################
# test <- aggregate(data.frame(abondance = basics$abondance), 
#                   by = list(sp = basics$sp, 
#                             carre.parc = basics$carre.parc,
#                             crop=basics$crop),sum)
# colnames(test)

# write.table(test, "data/generated/transpose_abondance_per_field2016.csv", sep = ";")

#################################################################

# ------------------------------------------------------------------------------
# End of commented stuff
# ------------------------------------------------------------------------------
