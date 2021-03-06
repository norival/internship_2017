#################################################################
# Script pour calculer les richesses � 20m� sur les donn�es 2012
#
# Sabrina Gaba
# cr�e le 24 octobre 2016
# modifi� le 
#################################################################

###########################################
## Aggregation des especes
###########################################
#weeds<-weeds2012C1
weeds<-read.csv("Data-Prog/weeds2012.csv", sep = ";",dec=",")

# remove 'luzerne' and 'prairie' and 'tr�fle'
weeds <- weeds[weeds$Crop.Analyses != "luzerne",]
weeds <- weeds[weeds$Crop.Analyses != "prairie",]
weeds <- weeds[weeds$Crop.Analyses != "tr�fle",]

test <- aggregate(data.frame(abondance = weeds$abondance), 
                  by = list(sp = weeds$sp, quadrat = weeds$pt, 
                            position = weeds$Par.interf,
                            carre.parc = weeds$carre.parc,
                            crop=weeds$Crop.Analyses), sum)

##Il y a des doublons dans le jeu de donnees 
##(somme des abondances par sous-quadrat ne peut pas �tre sup�rieure � 1)
#nrow(test[test$abondance > 1, ])
# 21361

## Juste set those lines with 1 value (the original data must be fixed after). 
#test[test$abondance > 1, ]$abondance <- 1

##nrow(test)
# 29008

## Cr�ation de la matrice par parcelles
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

A <- matrix(ncol=3+10, nrow=nrowA , data = rep(0, 13*nrowA ))
A<-data.frame(A)
colnames(A) <- c("sp", "carre.parc", "position", "q1","q2","q3","q4","q5","q6","q7","q8","q9","q10")
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

## Remplis les quadrats vides (>15 min)
for (i in 1:length(test$position)) {
  #  for(i in (1:length(test[,1]))[test$carre.parc=="10986-11533"]){
  spX <- test[i, 1]
  fieldX <- test[i, 4]
  positionX <- test[i, 3]
  quadrat <- as.numeric(test[i, 2])
  abondance <- test[i, 6]
  
  A[A$sp == spX & A$carre.parc == fieldX & A$position == positionX, quadrat+3]<- abondance  
}

#head(A, 25)
write.table(A, "Data-Prog/transpose_abondance_per_quadrat2012.csv", sep = ";")

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

write.table(test, "Data-Prog/transpose_abondance_per_fieldcore2012.csv", sep = ";")

#########################################################################
# Matrice site x especes par parcelle 
#########################################################################
test <- aggregate(data.frame(abondance = basics$abondance), 
                  by = list(sp = basics$sp, carre.parc = basics$carre.parc,
                            crop=basics$crop),sum)
colnames(test)

write.table(test, "Data-Prog/transpose_abondance_per_field2012.csv", sep = ";")

#################################################################
##Calcul des richesses observ�es 'nb hill 0, 1 et 2'
################################################################

library(vegan)
##on utilise les nb de Hill
##renyi(x, scales = c(0,1, 2),hill = T)

##avec le fichier "transpose_abondance_per_field.csv" &
## "transpose_abondance_per_fieldcore.csv"
##Etape 1: estimation de la richesse observ�e sur 40 m�
A=read.csv("Data-Prog/transpose_abondance_per_fieldcore2012.csv", sep = ";",h=T)
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
A_Diversity$Year=rep(2012,length(A_Diversity[,1]))
A_Diversity$NbQuadrats=rep(10,length(A_Diversity[,1]))
A_Diversity$SizeQuadrats=rep(4,length(A_Diversity[,1]))
A_Diversity$Type_Rich=rep("Obs",length(A_Diversity[,1]))
A_Diversity$crop=levels(A$crop)[croptemp]

mat2012 = xtabs(Richness~ carre.parc, A_Diversity)

A_Diversity_obs=A_Diversity

##Etape 1: estimation de la richesse observ�e sur 40 m�
A=read.csv("Data-Prog/transpose_abondance_per_quadrat2012.csv", sep = ";",h=T)
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
#A_Diversity$Year=rep(2012,length(A_Diversity[,1]))
A_Diversity$NbQuadrats_Stand=rep(5,length(A_Diversity[,1]))
A_Diversity$SizeQuadrats_Stand=rep(4,length(A_Diversity[,1]))
A_Diversity$Type_Rich_stand=rep("Stand",length(A_Diversity[,1]))

##ici##
x=match(A_Diversity_obs$carre.parc,A_Diversity$carre.parc)
xtemp=A_Diversity[x,]
A_Diversity=cbind(A_Diversity_obs,xtemp)
# plot(A_Diversity$Richness,A_Diversity$Richness_mean,
#      xlab="Species richness 40m�",ylab="Species richness 20m�")
abline(0,1)

write.table(A_Diversity, "Data-Prog/Diversity_fieldcore2012.csv", sep = ";")
