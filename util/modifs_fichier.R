#
# Programme Joël pour corriger les listes d'espèces
# pour les relevés de flore 0/1 sur quadrats de 0.25m²
#
##################################################################

modifs_fichier = function(tab=data2015)
{
   afaire.dat <- read.csv("util/modifs_liste.csv",header=T,sep=";", encoding = "latin1")
  depart.dat <- tab 
## suppression d'espece [col 4]
  casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,4])]
  numlig <- NULL
  for (icas in casconcernes)
    {
    numlig <- c(numlig,(1:nrow(depart.dat))[as.character(depart.dat[,13])==as.character(afaire.dat[icas,1])])
    }

  interm1.dat <- depart.dat
  if(length(numlig)>0){ interm1.dat <- depart.dat[-numlig,]}

## remplacement par un nouveau[3]
  casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,3])]
  numlig <- NULL
  
  for (icas in casconcernes)
    {
    numlig <- c(numlig,(1:nrow(depart.dat))[as.character(depart.dat[,13])==as.character(afaire.dat[icas,1])])
    }

  v1 <- depart.dat[numlig,13]
  jv1 <- match(v1,afaire.dat[casconcernes,1])
  v1 <- (afaire.dat[casconcernes,3])[jv1]
  
  interm2.dat <- interm1.dat
  interm2.dat[,13] <- as.character(interm2.dat[,13])
  interm2.dat[numlig,13] <- as.character(v1)

## Existe deux fois [5]

  casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,5])]
  numlig <- NULL

  for (icas in casconcernes)
 {
    numlig <- c(numlig,(1:nrow(depart.dat))[as.character(depart.dat[,13])==as.character(afaire.dat[icas,1])])
    }

### A faire à la main
print(c("lignes susceptibles d'etre des doublons"))
print(numlig)

## Existe deja (sous deux noms !) [2]
 casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,2])]

 if(sum(casconcernes==30)>0){print("attention cas à gerer à la main (rubra ou ovina)")}
  numlig <- NULL
  for (icas in casconcernes)
    {
    numlig <- c(numlig,(1:nrow(depart.dat))[as.character(interm2.dat[,13])==as.character(afaire.dat[icas,1])])
    }

### les modifs on va voir cas par cas

  k <- 0
  for (icas in casconcernes)
    {
    if(icas !=30){
      for (iparc in unique(interm2.dat[,6]))
        {
        print(c(iparc,icas))
        du.dat <- interm2.dat[interm2.dat[,6]==iparc,]
        k <- k+1
        namedouble <- afaire.dat[icas,2]
        nameorig <- afaire.dat[icas,1]
        ww <- match(c(nameorig,namedouble),as.character(du.dat[,13]))
        if(is.na(ww[1])&!is.na(ww[2])) {du.dat[ww[2],13] <- nameorig}
        if(!is.na(ww[1])&!is.na(ww[2])) {
          val <- 1
          if((du.dat[1,8]=="prairie") | (du.dat[1,8]=="luzerne") |
             (du.dat[1,8]=="trèfle") |(du.dat[1,8]=="friche"))
            {val <- sum(du.dat[ww,20])}
          du.dat[ww[1],20] <- val
          du.dat <- du.dat[-ww[2]]
        }
        
        interm2.dat[interm2.dat[,6]==iparc,] <- du.dat
      }
    }
  }
  return(interm2.dat)
  }
 
