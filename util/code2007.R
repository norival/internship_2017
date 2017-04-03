#################################################################
# Script pour calculer les richesses estimées sur 32*4m² 
# à 20 m²sur les données 2007
#
# Sabrina Gaba
# crée le 24 octobre 2016
# modifié le 
#################################################################

#####Charge le fichier des relevés de flore 2007
data2007=read.csv("data/raw/monitoring2007.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")
#vérification des données
# dim(data2007) #dimension du fichier de donnees 24787L & 31C
# head(data2007) #premieres lignes
# summary(data2007) #synthese des donnees
var <- colnames(data2007)

# unique(data2007$Crop.Analyses) #céréale/Cereal, colza/Colza,Maïs/maïs
data2007<-data2007[-which(data2007$Crop.Analyses=="#N/A"),]
data2007<-data2007[-which(data2007$Crop.Analyses=="Sorghum"),]
data2007<-data2007[-which(data2007$Crop.Analyses=="Stubbles"),]


data2007$Crop.Analyses=as.factor(data2007$Crop.Analyses)
levels(data2007$Crop.Analyses)=c("cereal","luzerne","maize","pois","sunflower")
data2007$Espèce_origin[data2007$Espèce_origin=="Festuca-ovina/rubra"]="Festuca-rubra" #Festuca-ovina/rubra


###Correction des noms d'espèces adventices
##code Joël
##affiner pour tenir compte de 32 et 10 quadrats
source("util/modifs_fichier_2.R", encoding = "latin1")
#data2007=data2007[data2007$No_parcelle!="ZPS197-2007",]
#data2007=data2007[data2007$Par!="ZPS197-2007-In",]
#data2007=data2007[data2007$Par!="ZPS197-2007-Pa",]

data2007=modifs_fichier(tab=data2007)

##pb with "6340-6918" field
#data2007<-data2007[-which(data2007$Par=="6340-6918"),]
#############################################################
#####Creation d'un jeu de donnees avec les cultures annuelles
#############################################################

# On conserve Year, date, Nb_parcelle, Par, Par.interf, 
#Crop.Analyses, pt, lat, LONG, Espèce_origin, abondance, ParPoint
kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2007 <- data2007[, kept ]
colnames(weeds2007)
weeds2007C<-subset(weeds2007,weeds2007$Crop.Analyses!="friche")
weeds2007C<-subset(weeds2007,weeds2007$Crop.Analyses!="luzerne")
weeds2007C<-subset(weeds2007C,weeds2007C$Crop.Analyses!="prairie")
weeds2007C<-subset(weeds2007C,weeds2007C$Crop.Analyses!="trèfle")

#weeds2007C$Crop.Analyses[weeds2007C$pt==1]
unique(weeds2007C$pt)
weeds2007C$pt=as.factor(weeds2007C$pt)
colnames(weeds2007C)[4]="carre.parc"

######################################################################            
### Mise en forme pour matrice sites x especes et calcul par quadrat
######################################################################

## Another lines will be the field number and position
#"carré-parc" :: numero de parcelle
#"Par.interf" : in, pa
#"Crop.Analyses" : cereal, colza, tournesol
#"lat"
#"LONG"
###colnames(weeds2007C)
###length(unique(weeds2007C$carre.parc))
## 183 parcelles

###unique(weeds2007C$Par.interf)
#[1] "In"  "pa" "in" "Pa" 
weeds2007C$Par.interf[weeds2007C$Par.interf=="In"]="in"
weeds2007C$Par.interf[weeds2007C$Par.interf=="Pa"]="pa"

###length(unique(weeds2007C$Crop.Analyses))
## 5 cultures

###length(unique(weeds2007$Espèce_origin))
## 294 espèces soit 294 colonnes dans la matrice

weeds2007C1 <- cbind(weeds2007C, as.factor(weeds2007C$Espèce_origin))
colnames(weeds2007C1) [13] <- "sp"

write.table(weeds2007C1, "data/generated/weeds2007.csv", sep = ";")

###########################################
## Aggregation des especes
###########################################
#weeds<-weeds2007C1
weeds<-read.csv("data/generated/weeds2007.csv", sep = ";",dec=",")
test <- aggregate(data.frame(abondance = weeds$abondance), 
                  by = list(sp = weeds$sp, quadrat = weeds$pt, 
                            position = weeds$Par.interf,
                            carre.parc = weeds$carre.parc,
                            crop=weeds$Crop.Analyses), sum)

##Il y a des doublons dans le jeu de donnees 
##(somme des abondances par sous-quadrat ne peut pas être supérieure à 1)
#nrow(test[test$abondance > 1, ])
# 21361

## Juste set those lines with 1 value (the original data must be fixed after). 
#test[test$abondance > 1, ]$abondance <- 1

##nrow(test)
# 29008

## Création de la matrice par parcelles
#tab <- xtabs(abondance~ sp + quadrat + position + carre.parc + crop, test)

#############################################################################
## Matrice site x especes avec ligne pour les quadrats vides
#############################################################################
source("functions/format_flora.R", encoding = "utf8")

# supprimer les quadrats en interface
test_noin <- test[-which(test$position == "in"),]

A <- transpose_df(tab = test_noin, n_quadras = 32, pos = "pa")

#head(A, 25)
write.table(A, "data/generated/transpose_abondance_per_quadrat2007.csv", sep = ";")


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

write.table(test, "data/generated/transpose_abondance_per_fieldcore2007.csv", sep = ";")

#########################################################################
# Matrice site x especes par parcelle 
#########################################################################
test <- aggregate(data.frame(abondance = basics$abondance), 
                  by = list(sp = basics$sp, carre.parc = basics$carre.parc,
                            crop=basics$crop),sum)
colnames(test)

write.table(test, "data/generated/transpose_abondance_per_field2007.csv", sep = ";")

#################################################################
##Calcul des richesses observées 'nb hill 0, 1 et 2'
################################################################

library(vegan)
##on utilise les nb de Hill
##renyi(x, scales = c(0,1, 2),hill = T)

##avec le fichier "transpose_abondance_per_field.csv" &
## "transpose_abondance_per_fieldcore.csv"
##Etape 1: estimation de la richesse observée sur 40 m²
A=read.csv("data/generated/transpose_abondance_per_fieldcore2007.csv", sep = ";",h=T)
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
A_Diversity$Year=rep(2007,length(A_Diversity[,1]))
A_Diversity$NbQuadrats=rep(10,length(A_Diversity[,1]))
A_Diversity$SizeQuadrats=rep(4,length(A_Diversity[,1]))
A_Diversity$Type_Rich=rep("Obs",length(A_Diversity[,1]))
A_Diversity$crop=levels(A$crop)[croptemp]

mat2007 = xtabs(Richness~ carre.parc, A_Diversity)

A_Diversity_obs=A_Diversity

##Etape 1: estimation de la richesse observée sur 40 m²
A=read.csv("data/generated/transpose_abondance_per_quadrat2007.csv", sep = ";",h=T)
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
#A_Diversity$Year=rep(2007,length(A_Diversity[,1]))
A_Diversity$NbQuadrats_Stand=rep(5,length(A_Diversity[,1]))
A_Diversity$SizeQuadrats_Stand=rep(4,length(A_Diversity[,1]))
A_Diversity$Type_Rich_stand=rep("Stand",length(A_Diversity[,1]))

##ici##
x=match(A_Diversity_obs$carre.parc,A_Diversity$carre.parc)
xtemp=A_Diversity[x,]
A_Diversity=cbind(A_Diversity_obs,xtemp)
plot(A_Diversity$Richness,A_Diversity$Richness_mean,
     xlab="Species richness 40m²",ylab="Species richness 20m²")
abline(0,1)

write.table(A_Diversity, "data/generated/Diversity_fieldcore2007.csv", sep = ";")

# -- estimation des abondances -------------------------------------------------

# package
source("functions/abundance.R", encoding = "utf8")

# on choppe les donnees
data2007 <- read.csv("data/generated/transpose_abondance_per_quadrat2007.csv",
                     sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

abond_per_plot <- estim_abundance(data2007, surf = 4, n_cores = 4)

write.csv(abond_per_plot, "data/generated/abondt_per_quadra_2007.csv",
          row.names = TRUE)

rm(list = ls())
