#################################################################
# Script pour calculer les richesses à 20m² sur les données 2011
#
# Sabrina Gaba
# crée le 16 octobre 2016
# modifié le 
#################################################################

#####Charge le fichier des relevés de flore 2011
data2011=read.csv("data/raw/monitoring2011.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")
#vérification des données
dim(data2011) #dimension du fichier de donnees 24787L & 31C
head(data2011) #premieres lignes
summary(data2011) #synthese des donnees
var <- colnames(data2011)

unique(data2011$Crop.Analyses) #céréale/Cereal, colza/Colza,Maïs/maïs
levels(data2011$Crop.Analyses)=c("cereal")
data2011$Espece_origin[data2011$Espece_origin=="Festuca-ovina/rubra"]="Festuca-rubra" #Festuca-ovina/rubra

###Correction des noms d'espèces adventices
##code Joël
source("util/modifs_fichier_log10b.R",
       encoding = "latin1") #pour parcelle 
#source("modifs_fichier_log10b.R") #pour séparer pa et in
data2011=data2011[data2011$No_parcelle!="ID10_994",]
data2011=modifs_fichier(tab=data2011)

##pb with "6340-6918" field
#data2011<-data2011[-which(data2011$Par=="6340-6918"),]
#############################################################
#####Creation d'un jeu de donnees avec les cultures annuelles
#############################################################

# On conserve Year, date, Nb_parcelle, Par, Par.interf, 
#Crop.Analyses, pt, lat, LONG, Espece_origin, abondance, ParPoint
kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2011 <- data2011[, kept ]
colnames(weeds2011)
weeds2011C<-subset(weeds2011,weeds2011$Crop.Analyses!="friche")
weeds2011C<-subset(weeds2011,weeds2011$Crop.Analyses!="luzerne")
weeds2011C<-subset(weeds2011C,weeds2011C$Crop.Analyses!="prairie")
weeds2011C<-subset(weeds2011C,weeds2011C$Crop.Analyses!="trèfle")

#weeds2011C$Crop.Analyses[weeds2011C$pt==1]
unique(weeds2011C$pt)
weeds2011C$pt=as.factor(weeds2011C$pt)
colnames(weeds2011C)[4]="carre.parc"

######################################################################            
### Mise en forme pour matrice sites x especes et calcul par quadrat
######################################################################

## Another lines will be the field number and position
#"carré-parc" :: numero de parcelle
#"Par.interf" : in, pa
#"Crop.Analyses" : cereal, colza, tournesol
#"lat"
#"LONG"
###colnames(weeds2011C)
###length(unique(weeds2011C$carre.parc))
## 183 parcelles

###unique(weeds2011C$Par.interf)
#[1] "In"  "pa" "in" "Pa" 
weeds2011C$Par.interf[weeds2011C$Par.interf=="In"]="in"
weeds2011C$Par.interf[weeds2011C$Par.interf=="Pa"]="pa"

###length(unique(weeds2011C$Crop.Analyses))
## 5 cultures

###length(unique(weeds2011$Espece_origin))
## 294 espèces soit 294 colonnes dans la matrice

weeds2011C1 <- cbind(weeds2011C, as.factor(weeds2011C$Espece_origin))
colnames(weeds2011C1) [13] <- "sp"

write.table(weeds2011C1, "data/generated/weeds2011.csv", sep = ";")

###########################################
## Aggregation des especes
###########################################
#weeds<-weeds2011C1
weeds<-read.csv("data/generated/weeds2011.csv", sep = ";",dec=",")

# Création d'une colonne avec l'abondance ramenée en log base2: toutes les
# valeurs supérieures à 2 sont mises à 2
weeds$abondance_base2 <- weeds$abondance
weeds$abondance_base2[weeds$abondance_base2 > 2] <- 2

test <- aggregate(data.frame(abondance = weeds$abondance_base2), 
                  by = list(sp = weeds$sp, quadrat = weeds$pt, 
                            position = weeds$Par.interf,
                            carre.parc = weeds$carre.parc,
                            crop=weeds$Crop.Analyses), sum)

#Il y a des doublons dans le jeu de donnees
# nrow(test[test$abondance > 2, ])
# 14

## Juste set those lines with 2 value (the original data must be fixed after). 
test[test$abondance > 2, ]$abondance <- 2

##nrow(test)
# 29008

## Création de la matrice par parcelles
#tab <- xtabs(abondance~ sp + quadrat + position + carre.parc + crop, test)

#############################################################################
## Matrice site x especes avec ligne pour les quadrats vides
#############################################################################
source("functions/format_flora.R")

A <- transpose_df(tab = test, n_quadras = 10, pos = c("pa", "in"))

write.table(A, "data/generated/transpose_abondance_per_quadrat2011.csv", sep = ";")


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

write.table(test, "data/generated/transpose_abondance_per_fieldcore2011.csv", sep = ";")

#########################################################################
# Matrice site x especes par parcelle 
#########################################################################
test <- aggregate(data.frame(abondance = basics$abondance), 
                  by = list(sp = basics$sp, carre.parc = basics$carre.parc,
                            crop=basics$crop),sum)
colnames(test)

write.table(test, "data/generated/transpose_abondance_per_field2011.csv", sep = ";")

#################################################################
##Calcul des richesses observées 'nb hill 0, 1 et 2'
################################################################

library(vegan)
##on utilise les nb de Hill
##renyi(x, scales = c(0,1, 2),hill = T)

##avec le fichier "transpose_abondance_per_field.csv" &
## "transpose_abondance_per_fieldcore.csv"
##Etape 1: estimation de la richesse observée sur 40 m²
A=read.csv("data/generated/transpose_abondance_per_fieldcore2011.csv", sep = ";",h=T)
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
A_Diversity$Year=rep(2011,length(A_Diversity[,1]))
A_Diversity$NbQuadrats=rep(10,length(A_Diversity[,1]))
A_Diversity$SizeQuadrats=rep(4,length(A_Diversity[,1]))
A_Diversity$Type_Rich=rep("Obs",length(A_Diversity[,1]))
A_Diversity$crop=levels(A$crop)[croptemp]

mat2011 = xtabs(Richness~ carre.parc, A_Diversity)

A_Diversity_obs=A_Diversity

##Etape 1: estimation de la richesse observée sur 40 m²
A=read.csv("data/generated/transpose_abondance_per_quadrat2011.csv", sep = ";",h=T)
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
#A_Diversity$Year=rep(2011,length(A_Diversity[,1]))
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

write.table(A_Diversity, "data/generated/Diversity_fieldcore2011.csv", sep = ";")
