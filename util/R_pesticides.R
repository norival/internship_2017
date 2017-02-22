# Code pour calculer des Doses, IFT, NODU...
#
# Crée le 2 aout 2016 par Sabrina Gaba
# Dernières modifications: le 10 aout 2016  par Sabrina
#
##############################################################

setwd("~/Donnees/Chize-Enquetes/Referentiel")
##setwd("~/BaseDonnees/Herbicides")
dose_recommandee=read.table("dose-reference-ift-grande-culture.csv",h=T,sep=";",
                            na.strings = "")

setwd("~/Donnees/Chize-Enquetes/Prog")

Intensite_Traitement= function (tab=fich_Herbi,tab_info=info_Herbi,
                                surf.trait=Surf_traitH,Nb_Trait_Rep=Nb_TraitH_Rep,
                                  type="HERBICIDE",crop="Colza")
{
  # tab= tableau de contingence créé avec load_enquete.R exemple avec fich_Herbi
  # tab_info = tableau créé avec load_enquete.R contenant les éléments relatifs au traitement (ID_parcelle, ID_exploitant, Date, Surface...)
  #surf.trait = tableau de contingence créé avec load_enquete.R avec les valeurs de surfaces traitées. Ex. Surf_traitH
  #Nb_Trait_Rep = tableau de contingence contenant le nb de fois qu'un produit a été utilisé dans une parcelle
  # type = catégorie des produit: AUTRE, FONGICIDE, HERBICIDE ou INSECTICIDE / ACARICIDE
  # crop = type de cultures.
  
  colnames(tab)=gsub(x = colnames(tab),pattern = "\\.",replacement = " ")
  
  ##Calcul de la dose
  dose=rowSums(tab)
  hist(dose,main="Histogramme des doses (KG/HA)",breaks=20)
  
  ##Calcul IFT
  #####Sélection des doses recommandées par type et culture
  DoseReco=subset(dose_recommandee,dose_recommandee$Catégorie.de.produit==type)
  DoseReco$Culture=DoseReco$Culture[,drop=T]
  DoseReco$Nom.du.produit=DoseReco$Nom.du.produit[,drop=T]
  x=match(as.character(DoseReco$Nom.du.produit),as.character(colnames(tab)))
  x[is.na(x)]=0
  x[x>0]=1
  DoseReco_tab=DoseReco[x==1,]
  DoseReco_tab$Nom.du.produit=DoseReco_tab$Nom.du.produit[,drop=T]
  
  if(length(unique(DoseReco_tab$Nom.du.produit))<length(colnames(tab)))
  {
    sel=setdiff(colnames(tab),levels( DoseReco_tab$Nom.du.produit))
    x=NULL
    for (i in sel)
    {
      if(dose_recommandee$Catégorie.de.produit[dose_recommandee$Nom.du.produit==i]==type)
      {
        print(i)
        x=c(x,i)
      }
      else { print(paste("A corriger",i,"est un", 
                        dose_recommandee$Catégorie.de.produit[dose_recommandee$Nom.du.produit==i],sep=" ")) }
    }
  }
   
  
  ##Calcul des doses de reférence pour la culture cible
  ##Si la culture n'est pas présente dans le référentiel, on prend le min des doses
  DoseRef=NULL
  temp=unique(DoseReco_tab$Nom.du.produit)
  DoseReco_tab$Dose=as.numeric(as.character(DoseReco_tab$Dose))
  for (i in 1:length(temp))
  {
    #print(i)
    if(sum(intersect(DoseReco_tab$Culture[DoseReco_tab$Nom.du.produit==temp[i]],
                     crop)==crop)>0)
    {DoseRef=c(DoseRef,DoseReco_tab$Dose[ DoseReco_tab$Nom.du.produit==temp[i] &
      DoseReco_tab$Culture==crop]) }
    else {DoseRef=c(DoseRef,min(DoseReco_tab$Dose[ DoseReco_tab$Nom.du.produit==temp[i]]))}
  }
  DoseRef=data.frame(DoseRef)
  DoseRef$Nom.du.produit=temp  
  rm(temp)
  DoseRef=DoseRef[order(DoseRef$Nom.du.produit,colnames(tab)),]
  
  ##Calcul de l'IFT produit
  ###prise en compte des produits appliqués à plusieurs dates (somme dans tab)
  MDoseRef=t(Nb_Trait_Rep)*DoseRef$DoseRef
  MDoseRef=apply(MDoseRef,1,funRatio)
  MDoseRef[MDoseRef=="Inf"]=0
  ####rapport doses apportées sur doses recommandées (à ajouter année)
  ## Ratio=t(tab)*(1/DoseRef$DoseRef)
  Ratio=tab*MDoseRef
  ####prise en compte de la surface traitée
  R1=surf.trait/tab_info$Surface_ha
  Ratio=Ratio * R1
  IFT=rowSums(Ratio,na.rm=T)
  
  ###Fichier sortie
  tab_IFT=data.frame(
    ID_Parc_Tri=tab_info$ID_Parc_Tri,
    ID_Parcelle=tab_info$ID_Parcelle,
    ID_Exploitation=tab_info$ID_Exploitation,
    Surface_ha=tab_info$Surface_ha,
    Culture=rep(crop,length(IFT)),
    Protection_visée=rep(type,length(IFT)),
    NbProd=as.vector(apply(tab,1,funSum)),
    Dose=dose,
    IFT=IFT)
  
  return(tab_IFT)

}