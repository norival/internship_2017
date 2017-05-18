#
# Programme Joël pour corriger les listes d'espèces
# pour les relevés de flore 0/1 sur quadrats de 0.25m²
#
##################################################################

modifs_fichier = function(tab=data2015)
{
  afaire.dat <- read.csv("util/modifs_liste.csv",header=T,sep=";",
                         encoding = "latin1")
  depart.dat <- tab

## suppression d'espece [col 4]
  casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,4])]
  numlig <- NULL
  for (icas in casconcernes)
    {
    numlig <- c(numlig,(1:nrow(depart.dat))[as.character(depart.dat$Espèce_origin)==as.character(afaire.dat[icas,1])])
    }

  interm1.dat <- depart.dat
  if(length(numlig)>0){ interm1.dat <- depart.dat[-numlig,]}



## remplacement par un nouveau[3]
  casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,3])]
  numlig <- NULL
  
  for (icas in casconcernes)
    {
    numlig <- c(numlig,(1:nrow(interm1.dat))[as.character(interm1.dat$Espèce_origin)==as.character(afaire.dat[icas,1])])
    }

  v1 <- interm1.dat$Espèce_origin[numlig]
  jv1 <- match(v1,afaire.dat[casconcernes,1])
  v1 <- (afaire.dat[casconcernes,3])[jv1]
  
  interm2.dat <- interm1.dat
  interm2.dat$Espèce_origin <- as.character(interm2.dat$Espèce_origin)
  interm2.dat$Espèce_origin[numlig] <- as.character(v1)

## remplacer par un existant
 casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,2])]


  numlig <- NULL
  for (icas in casconcernes)
    {
    numlig <- c(numlig,(1:nrow(interm2.dat))[as.character(interm2.dat$Espèce_origin)==as.character(afaire.dat[icas,1])])
    }

  v1 <- interm2.dat$Espèce_origin[numlig]
  jv1 <- match(v1,afaire.dat[casconcernes,1])
  v1 <- (afaire.dat[casconcernes,2])[jv1] ## fait
  
  interm3.dat <- interm2.dat
  interm3.dat$Espèce_origin <- as.character(interm3.dat$Espèce_origin)
  interm3.dat$Espèce_origin[numlig] <- as.character(v1)
  interm2.dat <- interm3.dat 

  return(interm2.dat)
  }
 
