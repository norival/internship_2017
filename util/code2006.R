#################################################################
# Script pour calculer les richesses estimées sur 32*4m²
# à 20 m²sur les données 2006
#
# Sabrina Gaba
# crée le 24 octobre 2016
# modifié le
#################################################################

#####Charge le fichier des relevés de flore 2006
data2006=read.csv("data/raw/monitoring2006.csv", sep=";", dec= "," ,
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")
#vérification des données
dim(data2006) #dimension du fichier de donnees 24787L & 31C
head(data2006) #premieres lignes
summary(data2006) #synthese des donnees
var <- colnames(data2006)

unique(data2006$Crop.Analyses) #céréale/Cereal, colza/Colza,Maïs/maïs
data2006<-data2006[-which(data2006$Crop.Analyses=="#N/A"),]
data2006<-data2006[-which(data2006$Crop.Analyses=="SetAside"),]
data2006<-data2006[-which(data2006$Crop.Analyses=="Stubbles"),]


data2006$Crop.Analyses=as.factor(data2006$Crop.Analyses)
levels(data2006$Crop.Analyses)=c("cereal","luzerne","maize","colza","pois","sunflower")
data2006$Espèce_origin[data2006$Espèce_origin=="Festuca-ovina/rubra"]="Festuca-rubra" #Festuca-ovina/rubra


###Correction des noms d'espèces adventices
##code Joël
##affiner pour tenir compte de 32 et 10 quadrats
source("util/modifs_fichier_2.R", encoding = "latin1")
#data2006=data2006[data2006$No_parcelle!="ZPS197-2006",]
#data2006=data2006[data2006$Par!="ZPS197-2006-In",]
#data2006=data2006[data2006$Par!="ZPS197-2006-Pa",]

# data2006=modifs_fichier(tab=data2006)

##pb with "6340-6918" field
#data2006<-data2006[-which(data2006$Par=="6340-6918"),]
#############################################################
#####Creation d'un jeu de donnees avec les cultures annuelles
#############################################################

# On conserve Year, date, Nb_parcelle, Par, Par.interf,
#Crop.Analyses, pt, lat, LONG, Espèce_origin, abondance, ParPoint
kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2006 <- data2006[, kept ]
colnames(weeds2006)
weeds2006C<-subset(weeds2006,weeds2006$Crop.Analyses!="friche")
weeds2006C<-subset(weeds2006,weeds2006$Crop.Analyses!="luzerne")
weeds2006C<-subset(weeds2006C,weeds2006C$Crop.Analyses!="prairie")
weeds2006C<-subset(weeds2006C,weeds2006C$Crop.Analyses!="trèfle")

#weeds2006C$Crop.Analyses[weeds2006C$pt==1]
unique(weeds2006C$pt)
weeds2006C$pt=as.factor(weeds2006C$pt)
colnames(weeds2006C)[4]="carre.parc"

######################################################################
### Mise en forme pour matrice sites x especes et calcul par quadrat
######################################################################

## Another lines will be the field number and position
#"carré-parc" :: numero de parcelle
#"Par.interf" : in, pa
#"Crop.Analyses" : cereal, colza, tournesol
#"lat"
#"LONG"
###colnames(weeds2006C)
###length(unique(weeds2006C$carre.parc))
## 183 parcelles

###unique(weeds2006C$Par.interf)
#[1] "In"  "pa" "in" "Pa"
weeds2006C$Par.interf[weeds2006C$Par.interf=="In"]="in"
weeds2006C$Par.interf[weeds2006C$Par.interf=="Pa"]="pa"

###length(unique(weeds2006C$Crop.Analyses))
## 5 cultures

###length(unique(weeds2006$Espèce_origin))
## 294 espèces soit 294 colonnes dans la matrice

weeds2006C1 <- cbind(weeds2006C, as.factor(weeds2006C$Espèce_origin))
colnames(weeds2006C1) [13] <- "sp"

write.table(weeds2006C1, "data/generated/weeds2006.csv", sep = ";")

###########################################
## Aggregation des especes
###########################################
#weeds<-weeds2006C1
weeds<-read.csv("data/generated/weeds2006.csv", sep = ";",dec=",")
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
# dim(A)
# head(A)

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

# vérifier la présence des espèces
# suppression des lignes où les espèces ne sont présentes dans aucun quadra
# A <- A[!rowSums(A[, 4:ncol(A)]) == ncol(A) - 3,]

# remplacement des '-1' par des 0
# A[A == -1] <- 0

#head(A, 25)
write.table(A, "data/generated/transpose_abondance_per_quadrat2006.csv", sep = ";")

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

write.table(test, "data/generated/transpose_abondance_per_fieldcore2006.csv", sep = ";")

#########################################################################
# Matrice site x especes par parcelle
#########################################################################
test <- aggregate(data.frame(abondance = basics$abondance),
                  by = list(sp = basics$sp, carre.parc = basics$carre.parc,
                            crop=basics$crop),sum)
colnames(test)

write.table(test, "data/generated/transpose_abondance_per_field2006.csv", sep = ";")

#################################################################
##Calcul des richesses observées 'nb hill 0, 1 et 2'
################################################################

library(vegan)
##on utilise les nb de Hill
##renyi(x, scales = c(0,1, 2),hill = T)

##avec le fichier "transpose_abondance_per_field.csv" &
## "transpose_abondance_per_fieldcore.csv"
##Etape 1: estimation de la richesse observée sur 40 m²
A=read.csv("data/generated/transpose_abondance_per_fieldcore2006.csv", sep = ";",h=T)
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
A_Diversity$Year=rep(2006,length(A_Diversity[,1]))
A_Diversity$NbQuadrats=rep(10,length(A_Diversity[,1]))
A_Diversity$SizeQuadrats=rep(4,length(A_Diversity[,1]))
A_Diversity$Type_Rich=rep("Obs",length(A_Diversity[,1]))
A_Diversity$crop=levels(A$crop)[croptemp]

mat2006 = xtabs(Richness~ carre.parc, A_Diversity)

A_Diversity_obs=A_Diversity

##Etape 1: estimation de la richesse observée sur 40 m²
A=read.csv("data/generated/transpose_abondance_per_quadrat2006.csv", sep = ";",h=T)
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
#A_Diversity$Year=rep(2006,length(A_Diversity[,1]))
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

write.table(A_Diversity, "data/generated/Diversity_fieldcore2006.csv", sep = ";")

# -- estimation des abondances -------------------------------------------------

# package
library(compoisson)

# on choppe les donnees
data2006.dat <- read.csv("data/generated/transpose_abondance_per_quadrat2006.csv",
                         sep = ";", stringsAsFactors = FALSE, encoding = "utf8")

# calcule la vraissemblance des données des quadras
# v = observations
# ltheta = log du paramètre de poisson (intensité)
h.fct <- function(ltheta,v=v) {
  # print(c(ltheta, v))
  theta <- exp(ltheta)

  # si param est énorme, pas possible
  if(max(theta)>30){return(100000)}

  # proba d'abondance pour les espèces ayant un indice d'abondance de 0
  lp0 <- com.log.density(0,theta[1],theta[2])
  # proba d'abondance pour les espèces ayant un indice d'abondance de 1
  lp1 <- com.log.density(1,theta[1],theta[2])
  # proba d'abondance pour les espèces ayant un indice d'abondance de 2
  lp2 <- log(1-exp(lp0)-exp(lp1))
  lp <- c(lp0,lp1,lp2)
  ll <- (-1)*sum(lp[v+1])
  return(ll)
}

# Création d'un tableau vide pour récupérer les estmations d'abondances
mat_vide <- matrix(Inf,
                   ncol = length(unique(data2006.dat$sp)),
                   nrow = length(unique(data2006.dat$carre.parc)))
abond_per_plot <- as.data.frame(mat_vide)

colnames(abond_per_plot) <- unique(data2006.dat$sp)
rownames(abond_per_plot) <- unique(data2006.dat$carre.parc)

for (parc in unique(data2006.dat$carre.parc)) {

  # On récupère les lignes pour la parcelle parc
  dat_sub <- data2006.dat[data2006.dat$carre.parc == parc, ]

  # ab <- NULL
  for (i in (1:nrow(dat_sub))) {
    # param de Poisson par espèce
    # ici on récupère la ligne i, qui correspond à un espèce pour la parelle
    # parc
    v1 <- as.numeric(dat_sub[i, 4:ncol(dat_sub)])

    # on estime l'abondance de la parcelle par la loi de poisson
    Zu <- nlminb(c(0, 0), h.fct, v = v1, lower = c(-50, -50),upper = c(50, 50))

    # on fait la moyenne de poisson sur le paramètre de Zu, qui correspond aux
    # moyennes. On repasse en exponentielle car on avait fait un log
    mm <- com.mean(exp(Zu$par[1]), exp(Zu$par[2]))

    # On rajoute cette moyenne dans le tableau vide initial
    abond_per_plot[parc, dat_sub$sp[i]] <- mm

  }
}

write.csv(abond_per_plot, "data/generated/abondt_per_plot_2006.csv",
          row.names = FALSE)
