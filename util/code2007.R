#################################################################
# Script pour calculer les richesses estimées sur 32*4m² 
# à 20 m²sur les données 2007
#
# Sabrina Gaba
# crée le 24 octobre 2016
# modifié le 
#################################################################

###########################################
## Aggregation des especes
###########################################
#weeds<-weeds2007C1
weeds<-read.csv("data/generated/weeds2007.csv", sep = ";",dec=",")

# remove 'luzerne' and 'prairie' and 'trèfle'
weeds <- weeds[weeds$Crop.Analyses != "luzerne",]
weeds <- weeds[weeds$Crop.Analyses != "prairie",]
weeds <- weeds[weeds$Crop.Analyses != "trèfle",]

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
test$abondance[test$abondance > 2] <- 2

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

A <- transpose_df(tab = test_noin, n_quadras = 32)

#head(A, 25)
write.table(A, "data/generated/transpose_abondance_per_quadrat2007.csv", sep = ";")

# ------------------------------------------------------------------------------
# I commented the stuff below because I don't use it and it is quite slow to run
# ------------------------------------------------------------------------------
#
# #########################################################################
# # Matrice site x especes par parcelle (plein champ/pas interface)
# #########################################################################
# #############################################################################################
# ##### Table avec les abondances par quadrats (ici plot)
# ##################################################################################################
# basics <- test
# basics1=subset(basics,basics$position!="in")
# test <- aggregate(data.frame(abondance = basics1$abondance), 
#                   by = list(sp = basics1$sp,carre.parc = basics1$carre.parc,
#                             crop=basics1$crop),sum)
# #colnames(test)
# 
# write.table(test, "data/generated/transpose_abondance_per_fieldcore2007.csv", sep = ";")
# 
# #########################################################################
# # Matrice site x especes par parcelle 
# #########################################################################
# test <- aggregate(data.frame(abondance = basics$abondance), 
#                   by = list(sp = basics$sp, carre.parc = basics$carre.parc,
#                             crop=basics$crop),sum)
# colnames(test)
# 
# write.table(test, "data/generated/transpose_abondance_per_field2007.csv", sep = ";")
# 
# #################################################################
# ##Calcul des richesses observées 'nb hill 0, 1 et 2'
# ################################################################
# 
# library(vegan)
# ##on utilise les nb de Hill
# ##renyi(x, scales = c(0,1, 2),hill = T)
# 
# ##avec le fichier "transpose_abondance_per_field.csv" &
# ## "transpose_abondance_per_fieldcore.csv"
# ##Etape 1: estimation de la richesse observée sur 40 m²
# A=read.csv("data/generated/transpose_abondance_per_fieldcore2007.csv", sep = ";",h=T)
# A_Diversity=matrix(NA,nrow=length(unique(A$carre.parc)),ncol=9)
# croptemp=matrix(NA,nrow=length(unique(A$carre.parc)),ncol=1)
# 
# for (i in (1:length(unique(A$carre.parc))))
# {
#   # (paste("carre.parc=",i,sep=""))
#   temp=A[A$carre.parc==unique(A$carre.parc)[i],]
#   croptemp[i,1]=unique(temp$crop)
#   if(sum(temp[,4]>0)>0)
#   {
#     A_Diversity[i,7:9]=renyi(t(temp[,4]), scales = c(0,1,2),hill = T)
#   }
#   else{A_Diversity[i,7:9]=c(0,0,0)}
# }
# 
# A_Diversity=as.data.frame(A_Diversity)
# colnames(A_Diversity)=c("carre.parc","crop","Year","NbQuadrats","SizeQuadrats",
#                         "Type_Rich","Richness","Shannon exp","Simpson inv")
# A_Diversity$carre.parc=unique(A$carre.parc)
# A_Diversity$Year=rep(2007,length(A_Diversity[,1]))
# A_Diversity$NbQuadrats=rep(10,length(A_Diversity[,1]))
# A_Diversity$SizeQuadrats=rep(4,length(A_Diversity[,1]))
# A_Diversity$Type_Rich=rep("Obs",length(A_Diversity[,1]))
# A_Diversity$crop=levels(A$crop)[croptemp]
# 
# mat2007 = xtabs(Richness~ carre.parc, A_Diversity)
# 
# A_Diversity_obs=A_Diversity
# 
# ##Etape 1: estimation de la richesse observée sur 40 m²
# A=read.csv("data/generated/transpose_abondance_per_quadrat2007.csv", sep = ";",h=T)
# A=droplevels(subset(A,A$position!="in"))
# 
# A_Diversity=matrix(NA,nrow=length(unique(A$carre.parc)),ncol=10)
# 
# for (i in (1:length(unique(A$carre.parc))))
# {
#   # (paste("carre.parc=",i,sep=""))
#   temp=A[A$carre.parc==unique(A$carre.parc)[i],]
#   xtemp=matrix(NA,nrow=100,ncol=3)
#   for (j in 1:100)
#   {
#     x=sample(4:13,5,replace=F)
#     x=apply(temp[,x],1,sum)
#     if(sum(temp[,4:13]>0)>0)
#     {
#       xtemp[j,1:3]=renyi(x, scales = c(0,1,2),hill = T)
#     }
#     else{
#       xtemp[j,1:3]=rep(0,3)}
#   }
#   
#   A_Diversity[i,5:7]=apply(xtemp,2,mean)
#   A_Diversity[i,8:10]=apply(xtemp,2,sd)
# }
# 
# A_Diversity=as.data.frame(A_Diversity)
# colnames(A_Diversity)=c("carre.parc1","NbQuadrats_Stand","SizeQuadrats_Stand",
#                         "Type_Rich_stand","Richness_mean","Shannon exp_mean",
#                         "Simpson inv_mean","Richness_sd","Shannon exp_sd",
#                         "Simpson inv_sd")
# A_Diversity$carre.parc1=unique(A$carre.parc)
# #A_Diversity$Year=rep(2007,length(A_Diversity[,1]))
# A_Diversity$NbQuadrats_Stand=rep(5,length(A_Diversity[,1]))
# A_Diversity$SizeQuadrats_Stand=rep(4,length(A_Diversity[,1]))
# A_Diversity$Type_Rich_stand=rep("Stand",length(A_Diversity[,1]))
# 
# ##ici##
# x=match(A_Diversity_obs$carre.parc,A_Diversity$carre.parc)
# xtemp=A_Diversity[x,]
# A_Diversity=cbind(A_Diversity_obs,xtemp)
# # plot(A_Diversity$Richness,A_Diversity$Richness_mean,
# #      xlab="Species richness 40m²",ylab="Species richness 20m²")
# abline(0,1)
# 
# write.table(A_Diversity, "data/generated/Diversity_fieldcore2007.csv", sep = ";")

# ------------------------------------------------------------------------------
# End of commented stuff
# ------------------------------------------------------------------------------
