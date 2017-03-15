#################################################################
# Script pour calculer les richesses estimées sur 32*4m² 
# à 20 m²sur les données 2008
#
# Sabrina Gaba
# crée le 24 octobre 2016
# modifié le 
#################################################################

#####Charge le fichier des relevés de flore 2008
data2008=read.csv("data/raw/monitoring2008.csv",
                  sep=";", dec= ",", stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")
#vérification des données
# dim(data2008) #dimension du fichier de donnees 24787L & 31C
# head(data2008) #premieres lignes
# summary(data2008) #synthese des donnees
var <- colnames(data2008)

unique(data2008$Crop.Analyses) #céréale/Cereal, colza/Colza,Maïs/maïs
data2008<-data2008[-which(data2008$Crop.Analyses=="#N/A"),]
data2008$Crop.Analyses=as.factor(data2008$Crop.Analyses)
levels(data2008$Crop.Analyses)=c("cereal","luzerne","colza","sunflower")
data2008$Espèce_origin[data2008$Espèce_origin=="Festuca-ovina/rubra"]="Festuca-rubra" #Festuca-ovina/rubra

###Correction des noms d'espèces adventices
##code Joël
##affiner pour tenir compte de 32 et 10 quadrats
source("util/modifs_fichier_2.R", encoding = "latin1")
#data2008=data2008[data2008$No_parcelle!="ZPS197-2008",]
#data2008=data2008[data2008$Par!="ZPS197-2008-In",]
#data2008=data2008[data2008$Par!="ZPS197-2008-Pa",]

data2008=modifs_fichier(tab=data2008)

##pb with "6340-6918" field
#data2008<-data2008[-which(data2008$Par=="6340-6918"),]
#############################################################
#####Creation d'un jeu de donnees avec les cultures annuelles
#############################################################

# On conserve Year, date, Nb_parcelle, Par, Par.interf, 
#Crop.Analyses, pt, lat, LONG, Espèce_origin, abondance, ParPoint
kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2008 <- data2008[, kept ]
colnames(weeds2008)
weeds2008C<-subset(weeds2008,weeds2008$Crop.Analyses!="friche")
weeds2008C<-subset(weeds2008,weeds2008$Crop.Analyses!="luzerne")
weeds2008C<-subset(weeds2008C,weeds2008C$Crop.Analyses!="prairie")
weeds2008C<-subset(weeds2008C,weeds2008C$Crop.Analyses!="trèfle")

#weeds2008C$Crop.Analyses[weeds2008C$pt==1]
unique(weeds2008C$pt)
weeds2008C$pt=as.factor(weeds2008C$pt)
colnames(weeds2008C)[4]="carre.parc"

######################################################################            
### Mise en forme pour matrice sites x especes et calcul par quadrat
######################################################################

## Another lines will be the field number and position
#"carré-parc" :: numero de parcelle
#"Par.interf" : in, pa
#"Crop.Analyses" : cereal, colza, tournesol
#"lat"
#"LONG"
###colnames(weeds2008C)
###length(unique(weeds2008C$carre.parc))
## 183 parcelles

###unique(weeds2008C$Par.interf)
#[1] "In"  "pa" "in" "Pa" 
weeds2008C$Par.interf[weeds2008C$Par.interf=="In"]="in"
weeds2008C$Par.interf[weeds2008C$Par.interf=="Pa"]="pa"

###length(unique(weeds2008C$Crop.Analyses))
## 5 cultures

###length(unique(weeds2008$Espèce_origin))
## 294 espèces soit 294 colonnes dans la matrice

weeds2008C1 <- cbind(weeds2008C, as.factor(weeds2008C$Espèce_origin))
colnames(weeds2008C1) [13] <- "sp"

write.table(weeds2008C1, "data/generated/weeds2008.csv", sep = ";")

###########################################
## Aggregation des especes
###########################################
#weeds<-weeds2008C1
weeds<-read.csv("data/generated/weeds2008.csv", sep = ";",dec=",")
test <- aggregate(data.frame(abondance = weeds$abondance), 
                  by = list(sp = weeds$sp, quadrat = weeds$pt, 
                            position = weeds$Par.interf,
                            carre.parc = weeds$carre.parc,
                            crop=weeds$Crop.Analyses), sum)

##Il y a des doublons dans le jeu de donnees
# nrow(test[test$abondance > 2, ])
# 19

## Juste set those lines with 2 value (the original data must be fixed after). 
test[test$abondance > 2, ]$abondance <- 2

##nrow(test)
# 29008

## Création de la matrice par parcelles
#tab <- xtabs(abondance~ sp + quadrat + position + carre.parc + crop, test)

#############################################################################
## Matrice site x especes avec ligne pour les quadrats vides
#############################################################################
#length(unique(test$sp))
#240 species

#length(unique(test$carre.parc))
#183 different sampling of fields

### Prepare an empty matrix filled with 0 (for 0 abundance observed)
nrowA <- length(unique(test$carre.parc)) * length(unique(test$sp)) * 2

A <- matrix(ncol=3+32, nrow=nrowA , data = rep(0, 35*nrowA))
A <- data.frame(A)
colnames(A) <- c("sp", "carre.parc", "position",
                 paste("q", 1:32, sep = ""))
#dim(A)
#head(A)

## Init the species, field number, and then position in A
#species names
A$sp <- rep(rep(levels(test$sp), length(unique(test$carre.parc))),2)

#carre.parc names
carre.parc<-list(NA)
length(carre.parc)<-length(levels(test$carre.parc))
for (i in 1:length(carre.parc))
{
  carre.parc[[i]]<-rep(levels(test$carre.parc)[i],length(levels(test$sp)))
}
carre.parc<-unlist(carre.parc)
A$carre.parc <- rep(carre.parc,2)

#positions (in,pa)
A$position<-c(rep("pa",length(carre.parc)),rep("in",length(carre.parc)))

# supprimer les quadrats en interface
test_noin <- test[-which(test$position == "in"),]

## Remplis les quadrats vides (>15 min)
for (i in 1:length(test_noin$position)) {
  spX       <- test_noin[i, 1]
  fieldX    <- test_noin[i, 4]
  positionX <- test_noin[i, 3]
  quadrat   <- as.numeric(test_noin[i, 2])
  abondance <- test_noin[i, 6]
  
  A[A$sp == spX & A$carre.parc == fieldX & A$position == positionX, quadrat+3] <-
    abondance
}
A <- A[A$position != "in",]
#head(A, 25)

write.table(A, "data/generated/transpose_abondance_per_quadrat2008.csv", sep = ";")

#########################################################################
# Matrice site x especes par parcelle (plein champ/pas interface)
#########################################################################
#############################################################################################
##### Table avec les abondances par quadrats (ici plot)
##################################################################################################
basics <- test
basics1=subset(basics,basics$position!="in")
test <- aggregate(data.frame(abondance = basics1$abondance), 
                  by = list(sp = basics1$sp,carre.parc = basics1$carre.parc,
                            crop=basics1$crop),sum)
#colnames(test)

write.table(test, "data/generated/transpose_abondance_per_fieldcore2008.csv", sep = ";")

#########################################################################
# Matrice site x especes par parcelle 
#########################################################################
test <- aggregate(data.frame(abondance = basics$abondance), 
                  by = list(sp = basics$sp, carre.parc = basics$carre.parc,
                            crop=basics$crop),sum)

write.table(test, "data/generated/transpose_abondance_per_field2008.csv", sep = ";")

#################################################################
##Calcul des richesses observées 'nb hill 0, 1 et 2'
################################################################

library(vegan)
##on utilise les nb de Hill
##renyi(x, scales = c(0,1, 2),hill = T)

##avec le fichier "transpose_abondance_per_field.csv" &
## "transpose_abondance_per_fieldcore.csv"
##Etape 1: estimation de la richesse observée sur 40 m²
A=read.csv("data/generated/transpose_abondance_per_fieldcore2008.csv", sep = ";",h=T)
A_Diversity=matrix(NA,nrow=length(unique(A$carre.parc)),ncol=9)
croptemp=matrix(NA,nrow=length(unique(A$carre.parc)),ncol=1)

for (i in (1:length(unique(A$carre.parc))))
{
  # (paste("carre.parc=",i,sep=""))
  temp=A[A$carre.parc==unique(A$carre.parc)[i],]
  croptemp[i,1]=unique(temp$crop)
  if(sum(temp[,4]>0)>0)
  {
    A_Diversity[i,7:9]=renyi(t(temp[,4]), scales = c(0,1,2),hill = T)
  }
  else{A_Diversity[i,7:9]=c(0,0,0)}
}

A_Diversity=as.data.frame(A_Diversity)
colnames(A_Diversity)=c("carre.parc","crop","Year","NbQuadrats","SizeQuadrats",
                        "Type_Rich","Richness","Shannon exp","Simpson inv")
A_Diversity$carre.parc=unique(A$carre.parc)
A_Diversity$Year=rep(2008,length(A_Diversity[,1]))
A_Diversity$NbQuadrats=rep(10,length(A_Diversity[,1]))
A_Diversity$SizeQuadrats=rep(4,length(A_Diversity[,1]))
A_Diversity$Type_Rich=rep("Obs",length(A_Diversity[,1]))
A_Diversity$crop=levels(A$crop)[croptemp]

mat2008 = xtabs(Richness~ carre.parc, A_Diversity)

A_Diversity_obs=A_Diversity

##Etape 1: estimation de la richesse observée sur 40 m²
A=read.csv("data/generated/transpose_abondance_per_quadrat2008.csv", sep = ";",h=T)
A=droplevels(subset(A,A$position!="in"))

A_Diversity=matrix(NA,nrow=length(unique(A$carre.parc)),ncol=10)

for (i in (1:length(unique(A$carre.parc))))
{
  # (paste("carre.parc=",i,sep=""))
  temp=A[A$carre.parc==unique(A$carre.parc)[i],]
  xtemp=matrix(NA,nrow=100,ncol=3)
  for (j in 1:100)
  {
    x=sample(4:13,5,replace=F)
    x=apply(temp[,x],1,sum)
    if(sum(temp[,4:13]>0)>0)
    {
      xtemp[j,1:3]=renyi(x, scales = c(0,1,2),hill = T)
    }
    else{
      xtemp[j,1:3]=rep(0,3)}
  }
  
  A_Diversity[i,5:7]=apply(xtemp,2,mean)
  A_Diversity[i,8:10]=apply(xtemp,2,sd)
}

A_Diversity=as.data.frame(A_Diversity)
colnames(A_Diversity)=c("carre.parc1","NbQuadrats_Stand","SizeQuadrats_Stand",
                        "Type_Rich_stand","Richness_mean","Shannon exp_mean",
                        "Simpson inv_mean","Richness_sd","Shannon exp_sd",
                        "Simpson inv_sd")
A_Diversity$carre.parc1=unique(A$carre.parc)
#A_Diversity$Year=rep(2008,length(A_Diversity[,1]))
A_Diversity$NbQuadrats_Stand=rep(5,length(A_Diversity[,1]))
A_Diversity$SizeQuadrats_Stand=rep(4,length(A_Diversity[,1]))
A_Diversity$Type_Rich_stand=rep("Stand",length(A_Diversity[,1]))

##ici##
x=match(A_Diversity_obs$carre.parc,A_Diversity$carre.parc)
xtemp=A_Diversity[x,]
A_Diversity=cbind(A_Diversity_obs,xtemp)
# plot(A_Diversity$Richness,A_Diversity$Richness_mean,
#      xlab="Species richness 40m²",ylab="Species richness 20m²")
# abline(0,1)

write.table(A_Diversity, "data/generated/Diversity_fieldcore2008.csv", sep = ";")

# -- estimation des abondances -------------------------------------------------

# package
source("functions/abundance.R", encoding = "utf8")

# on choppe les donnees
data2008 <- read.csv("data/generated/transpose_abondance_per_quadrat2008.csv",
                     sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

abond_per_plot <- estim_abundance(data2008, surf = 4, n_cores = 4)

write.csv(abond_per_plot, "data/generated/abondt_per_plot_2008.csv",
          row.names = TRUE)

rm(list = ls())
