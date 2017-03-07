#
# Programme Joël pour corriger les listes d'espèces
# pour les relevés de flore en log10
#
##################################################################

#Règle pour ajouter deux espèces:
#soit elles sont presentes à plus d'un representant et c'est le max
#des deux qui gagne
#soit elles ne sont presentes qu'une seule fois et ça rentre dans la classe 2

modifs_fichier = function(tab=data2013)
{
  afaire.dat <- read.csv("modifs_liste.csv",header=T,sep=";")
  depart.dat <- tab

## suppression d'espece [col 4]
casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,4])]
numlig <- NULL
for (icas in casconcernes)
{
numlig <- c(numlig,(1:nrow(depart.dat))[as.character(depart.dat$Espèce_origin)==as.character(afaire.dat[icas,1])])#[,13]
}

interm1.dat <- depart.dat
if(length(numlig)>0){ interm1.dat <- depart.dat[-numlig,]}

## remplacement par un nouveau[3]

casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,3])]
numlig <- NULL
for (icas in casconcernes)
{
numlig <- c(numlig,(1:nrow(depart.dat))[as.character(depart.dat$Espèce_origin)==as.character(afaire.dat[icas,1])]) #[,13]
}

v1 <- depart.dat$Espèce_origin[numlig]#13
jv1 <- match(v1,afaire.dat[casconcernes,1])
v1 <- (afaire.dat[casconcernes,3])[jv1]

interm2.dat <- interm1.dat
interm2.dat$Espèce_origin<- as.character(interm2.dat$Espèce_origin) #13
interm2.dat$Espèce_origin[numlig] <- as.character(v1) #13

## existe deux fois [5]

casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,5])]
numlig <- NULL
for (icas in casconcernes)
{
numlig <- c(numlig,(1:nrow(depart.dat))[as.character(depart.dat$Espèce_origin)==as.character(afaire.dat[icas,1])])
}

### a faire à la main
print(c("lignes susceptibles d'etre des doublons"))
print(numlig)

## existe deja (sous deux noms !) [2]

casconcernes <- (1:nrow(afaire.dat))[!is.na(afaire.dat[,2])]

if(sum(casconcernes==30)>0){print("attention cas à gerer à la main (rubra ou ovina)")}
numlig <- NULL
for (icas in casconcernes)
{
numlig <- c(numlig,(1:nrow(depart.dat))[as.character(interm2.dat$Espèce_origin)==as.character(afaire.dat[icas,1])])
}

### les modifs on va voir cas par cas

k <- 0
for (icas in casconcernes)
{
if(icas !=30){
for (iparc in unique(interm2.dat$No_parcelle))
{
print(c(iparc,icas))
du.dat <- interm2.dat[interm2.dat$No_parcelle==iparc,]
k <- k+1
namedouble <- afaire.dat[icas,2]
nameorig <- afaire.dat[icas,1]
ww <- match(c(nameorig,namedouble),as.character(du.dat$Espèce_origin))
if(is.na(ww[1])&!is.na(ww[2])) {du.dat$Espèce_origin[ww[2]] <- nameorig}
if(!is.na(ww[1])&!is.na(ww[2])) {
  
  selocc <-(du.dat$Crop.Analyses[1]=="prairie") | (du.dat$Crop.Analyses[1,]=="luzerne") |
     (du.dat$Crop.Analyses[1]=="trèfle") |(du.dat$Crop.Analyses[1]=="friche")
       if(selocc){val <- sum(du.dat$abondance[ww,])}
  if(!selocc){
  w1 <- du.dat$abondance[ww[1]]
  w2 <- du.dat$abondance[ww[2]]
  w12 <- max(w1,w2)
  if((w1==1)&(w2==1)) w12 <- 2
  du.dat$abondance[ww[1]] <- w12
}
  du.dat <- du.dat[-ww[2]]


 }
interm2.dat[interm2.dat$No_parcelle==iparc,] <- du.dat
}
  }

}
return(interm2.dat)
}
 