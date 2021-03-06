# Lecture des fichiers enquetes
#
# Cr�e le 5 aout 2016 par Sabrina Gaba
# Modifi� le XX  par XX
#
##############################################################

setwd("~/Donnees/Chize-Enquetes/Prog")

##R packages � charger
library(tcltk2) #pour questions/r�ponses interactives
library(ggplot2)
library(FactoMineR) #pour les analyses multivari�es
library(plyr)


##Lit les fichiers et pr�pare des sous-fichiers par th�me
print("Lecture du fichier enqu�te")
source("load_enquete.R")
print("Chargement du code pour calculs dose & ift")
source("R_pesticides.R")

#############################################
########Analyse descriptive des pratiques
#############################################
#Synth�se des informations concernant le travail du sol
summary(fich_Wsol)
summary(fich_semis)

#Exemple d'une analyse multivari�e
ITK_ACP=data.frame(
  ID_Parcelle=fich_farm$ID_Parcelle,
  Densit�_Semis=fich_semis$Densit�_Semis,
  Date_Semis=fich_semis$Date_Semis.1,
  Inter_rang=fich_semis$Inter.rang_cm,
  nb_Wsol=fich_Wsol$nb_Wsol,
  prof_Wsol=fich_Wsol$prof_Wsol,
  prof_Wsol_min=fich_Wsol$prof_Wsol_min,
  prof_Wsol_max=fich_Wsol$prof_Wsol_max
  )

res.pca.tsol = PCA(ITK_ACP,quali.sup=1)
par(mfrow=c(1,2))
plot.PCA(res.pca.tsol, axes=c(1, 2), choix="var",cex=0.8)
plot.PCA(res.pca.tsol, axes=c(1, 2), choix="ind", habillage=1,cex=0.8)

##Analyses des pesticides

##Pour obtenir les doses et les IFTs
##Herbicides
tab_IFT=Intensite_Traitement(tab=fich_Herbi,tab_info=info_Herbi,
                                surf.trait=Surf_traitH,Nb_Trait_Rep=Nb_TraitH_Rep,
                                type="HERBICIDE",crop="Colza")

  # tab= tableau de contingence cr�� avec load_enquete.R exemple avec fich_Herbi
  # tab_info = tableau cr�� avec load_enquete.R contenant les �l�ments relatifs au traitement (ID_parcelle, ID_exploitant, Date, Surface...)
  #surf.trait = tableau de contingence cr�� avec load_enquete.R avec les valeurs de surfaces trait�es. Ex. Surf_traitH
  #Nb_Trait_Rep = tableau de contingence contenant le nb de fois qu'un produit a �t� utilis� dans une parcelle
  # type = cat�gorie des produit: AUTRE, FONGICIDE, HERBICIDE ou INSECTICIDE / ACARICIDE
  # crop = type de cultures.

names(tab_IFT)

#exemple figure
hist(tab_IFT$IFT,xlab="Indice de Fr�quence de Traitement",
     main=paste("IFT",type,"en",crop,sep=" "),breaks=10)

ggplot(tab_IFT, aes(y = IFT,x=Surface_ha)) + 
  geom_smooth(method="lm",se=FALSE,color='black') +
  geom_point(size=2)+
  labs(y=paste("IFT",unique(tab_IFT$Protection_vis�e),sep=" "),
       x="Surface (ha)")+
  theme_bw()

##Insecticides
tab_IFT=Intensite_Traitement(tab=fich_Insect,tab_info=info_Insect,
                             surf.trait=Surf_traitI,Nb_Trait_Rep=Nb_TraitI_Rep,
                             type="INSECTICIDE / ACARICIDE",crop="Colza")
write.table(tab_IFT,file="IFT_insecticides_colza_19aout2016.csv",sep=";")

##Insecticides
tab_IFT=Intensite_Traitement(tab=fich_Fongi,tab_info=info_Fongi,
                             surf.trait=Surf_traitF,Nb_Trait_Rep=Nb_TraitF_Rep,
                             type="FONGICIDE",crop="Colza")
write.table(tab_IFT,file="IFT_fongicides_colza_19aout2016.csv",sep=";")

