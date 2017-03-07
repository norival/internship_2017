# Fonctions pour le programme load_enquete
#
# Crée le 2 aout 2016 par Sabrina Gaba
# Modifié le XX  par XX
#
##############################################################

##Trouve les doublons
duplicated2 <- function(x){
  if (sum(dup <- duplicated(x))==0)
    return(dup)
  if (class(x) %in% c("data.frame","matrix"))
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))]
  else duplicated(c(x[dup],x))[-(1:sum(dup))]
}

######################################################
##Fonction pour calculer la profondeur du travail du sol
funWsol = function(x){
  sel=rep(NA,length(x))
  for (i in 1:length(x))
{
    if(x[i]=="superficiel" & is.na(x[i])==F) {sel[i] = 2}
    if(x[i]!="superficiel" & is.na(x[i])==F & gregexpr("_",x[i]) [[1]][1] == -1) 
    {
      sel[i] = as.numeric(as.character(x[i])) 
    }
    if(x[i]!="superficiel" & is.na(x[i])==F & gregexpr("_",x[i]) [[1]][1] > -1) 
      {
      sel[i] = mean(as.numeric(unlist(strsplit(as.character(x[i]), "_"))))
    }
  }
  sel
}
##########################################################
##Fonction pour vérification des doses fertilisants
funDoseN = function(z=Id_Parc,x=unite_dose,y=produit)
{
  ref=c("KG/HA","L/HA","Unité/HA") 
  temp="La dose de fertilisants est exprimée en KG/HA ou L/HA ou Unité/HA"
  if(length(setdiff(levels(x),ref)) > 0) 
  {
    print(setdiff(levels(na.omit(x)),ref))
    temp=paste(z[na.omit(x)==setdiff(levels(na.omit(x)),ref)],
               y[na.omit(x)==setdiff(levels(na.omit(x)),ref)],sep=":")
  }
  print(temp)
}
###########################################################
##Fonction pour vérification des doses phyto
funDose = function(z=Id_Parc,x=unite_dose,y=produit)
{
  ref=c("KG/HA","L/HA") 
  temp="La dose de pesticides est exprimée en KG/HA ou L/HA"
  if(length(setdiff(levels(x),ref)) > 0) 
  {
    print(setdiff(levels(na.omit(x)),ref))
    temp=paste(z[na.omit(x)==setdiff(levels(na.omit(x)),ref)],
               y[na.omit(x)==setdiff(levels(na.omit(x)),ref)],sep=":")
  }
  print(temp)
}

#funDose(z=fich_Herbi$ID_Parc_Tri,x=fich_Herbi$Dose_Phyto,y=fich_Herbi$Produit_phyto)
########################
fmin = function (x) 
  {
  if(sum(is.na(x))==length(x)) {NA}
  else min(unique(x),na.rm=T)
}

fmax = function (x) 
{
  if(sum(is.na(x))==length(x)) {NA}
  else max(unique(x),na.rm=T)
}

funique = function (x) 
{
  if(sum(is.na(x))==length(x)) {NA}
  else unique(na.omit(x))
}

#############################################################
##fonction pour transformer les valeurs des listes en vecteur
## valable quand il n'y a qu'une valeur par liste

funlist = function(x,num=T)
{
  n=length(x)
  X=NULL
  if (num==T){
    for (i in 1:n)
  {
    X=c(X,as.numeric(as.character(x[[i]][1])))
   }
  }
  if (num==F){
    for (i in 1:n)
    {
      X=c(X,as.character(x)[[i]][1])
    }
  }
  
  return(X)
    
}

#####################################################################
#############################################################
##fonction pour transformer les valeurs des listes en data.frame
## valable quand il n'y a plus d'une valeur par liste

funlist_multi = function(x,missing.data=NA,date=F)
{
  #x est la colonne du data.frame sous forme de list à transformer en une matrice
  #de n colonnes; n étant le nombre de valeurs par ligne.
  #missing.data est la valeur par défaut à mettre dans la matrice. 
  #ex. pour interculture = "Aucune", pour les traitements = 0, ...
  #date=T pour les données de date
  
  n=length(x)
  nbcol=max(unlist(lapply(x,length)))
  X=matrix(missing.data,nrow=n,ncol=nbcol)
  
  for (i in 1:n)
    {
    if(date==F){
      # Xavier: ajout d'une vérification: remplacer par NA quand la valeur de
      # 'test' est numérique et inférieure à 1. La valeur renvoyée par x[0.5]
      # est 'character(0)' alors que celle renvoyée par x[1.5] est 'NA'. Cela
      # faisait donc planter la fonction quand l'indice était < 1.
      # test <- x[[i]][1:length(x[[i]])][1]
      # if (is.numeric(test) && test < 1) {
      #   X[i,(1:length(x[[i]]))] <- NA
      # } else {
      #   X[i,(1:length(x[[i]]))]=levels(as.factor(x[[1]]))[x[[i]][1:length(x[[i]])]]
      X[i,(1:length(x[[i]]))]=levels(x[[1]])[x[[i]][1:length(x[[i]])]]
      # }
    }
    if(date==T){
      X[i,(1:length(x[[i]]))]=as.Date(x[[i]][1:length(x[[i]])],"%d/%m/%Y")
    }
  }
  
  return(X)
  
}

##########################################################
##fonction pour calculer des ratios dans des matrices
funRatio=function(x) {1/x}

##########################################################
##fonction pour calculer des sums de cellules non vides
funSum=function(x) {sum(x>0)}
