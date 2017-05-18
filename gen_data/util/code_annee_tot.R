#
# Compilation des années 2006-2016
#
#
############################################################################

# setwd("~/Donnees/Chize_Flore/Prog")

#########################################################
# 2006
#########################################################
print("year 2006")
data2006=read.csv("data/raw/monitoring2006.csv", sep=";", dec= "," ,
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")

unique(data2006$Crop.Analyses) #céréale/Cereal, colza/Colza,Maïs/maïs
data2006<-data2006[-which(data2006$Crop.Analyses=="#N/A"),]
data2006<-data2006[-which(data2006$Crop.Analyses=="SetAside"),]
data2006<-data2006[-which(data2006$Crop.Analyses=="Stubbles"),]


data2006$Crop.Analyses=as.factor(data2006$Crop.Analyses)
levels(data2006$Crop.Analyses)=c("cereal","luzerne","maize","colza","pois","sunflower")
data2006$Espèce_origin[data2006$Espèce_origin=="Festuca-ovina/rubra"]="Festuca-rubra" #Festuca-ovina/rubra

source("util/modifs_fichier_2.R", encoding = "latin1")
data2006=modifs_fichier(tab=data2006)

kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2006 <- data2006[, kept ]
colnames(weeds2006)

weeds2006C<-subset(weeds2006,weeds2006$Crop.Analyses!="friche")

weeds2006C$pt=as.factor(weeds2006C$pt)
colnames(weeds2006C)[4]="carre.parc"

weeds2006C$Par.interf[weeds2006C$Par.interf=="In"]="in"
weeds2006C$Par.interf[weeds2006C$Par.interf=="Pa"]="pa"

weeds2006C1 <- cbind(weeds2006C, as.factor(weeds2006C$Espèce_origin))
colnames(weeds2006C1) [13] <- "sp"

weeds2006C1$ParPointTri = paste(weeds2006C1$No_parcelle,"2006",
                                weeds2006C1$Par.interf,sep="-")

write.table(weeds2006C1, "data/generated/weeds2006.csv", sep = ";")
rm(weeds2006,weeds2006C,weeds2006C1,data2006)

#########################################################
# 2007
#########################################################
print("year 2007")
data2007=read.csv("data/raw/monitoring2007.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")

data2007<-data2007[-which(data2007$Crop.Analyses=="#N/A"),]
data2007<-data2007[-which(data2007$Crop.Analyses=="Sorghum"),]
data2007<-data2007[-which(data2007$Crop.Analyses=="Stubbles"),]


data2007$Crop.Analyses=as.factor(data2007$Crop.Analyses)
levels(data2007$Crop.Analyses)=c("cereal","luzerne","maize","pois","sunflower")
data2007$Espèce_origin[data2007$Espèce_origin=="Festuca-ovina/rubra"]="Festuca-rubra" #Festuca-ovina/rubra

source("util/modifs_fichier_2.R", encoding = "latin1")
data2007=modifs_fichier(tab=data2007)

kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2007 <- data2007[, kept ]
colnames(weeds2007)
weeds2007C<-subset(weeds2007,weeds2007$Crop.Analyses!="friche")

unique(weeds2007C$pt)
weeds2007C$pt=as.factor(weeds2007C$pt)
colnames(weeds2007C)[4]="carre.parc"

weeds2007C$Par.interf[weeds2007C$Par.interf=="In"]="in"
weeds2007C$Par.interf[weeds2007C$Par.interf=="Pa"]="pa"

weeds2007C1 <- cbind(weeds2007C, as.factor(weeds2007C$Espèce_origin))
colnames(weeds2007C1) [13] <- "sp"

weeds2007C1$ParPointTri = paste(weeds2007C1$No_parcelle,"2007",
                                weeds2007C1$Par.interf,sep="-")

write.table(weeds2007C1, "data/generated/weeds2007.csv", sep = ";")
rm(weeds2007,weeds2007C,weeds2007C1,data2007)

#########################################################
# 2008
#########################################################
print("year 2008")
data2008=read.csv("data/raw/monitoring2008.csv",
                  sep=";", dec= ",", stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")

data2008<-data2008[-which(data2008$Crop.Analyses=="#N/A"),]
data2008$Crop.Analyses=as.factor(data2008$Crop.Analyses)
levels(data2008$Crop.Analyses)=c("cereal","luzerne","colza","sunflower")
data2008$Espèce_origin[data2008$Espèce_origin=="Festuca-ovina/rubra"]="Festuca-rubra" #Festuca-ovina/rubra

###Correction des noms d'espèces adventices
##code Joël
##affiner pour tenir compte de 32 et 10 quadrats
source("util/modifs_fichier_2.R", encoding = "latin1")
data2008=modifs_fichier(tab=data2008)

kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2008 <- data2008[, kept ]
colnames(weeds2008)
weeds2008C<-subset(weeds2008,weeds2008$Crop.Analyses!="friche")

weeds2008C$pt=as.factor(weeds2008C$pt)
colnames(weeds2008C)[4]="carre.parc"

weeds2008C$Par.interf[weeds2008C$Par.interf=="In"]="in"
weeds2008C$Par.interf[weeds2008C$Par.interf=="Pa"]="pa"

weeds2008C1 <- cbind(weeds2008C, as.factor(weeds2008C$Espèce_origin))
colnames(weeds2008C1) [13] <- "sp"

weeds2008C1$ParPointTri =paste(weeds2008C1$No_parcelle,"2008",
                               weeds2008C1$Par.interf,sep="-")

write.table(weeds2008C1, "data/generated/weeds2008.csv", sep = ";")
rm(weeds2008,weeds2008C,weeds2008C1)

#########################################################
# 2009
#########################################################
print("year 2009")
data2009=read.csv("data/raw/monitoring2009.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")

unique(data2009$Crop.Analyses) #Cereal, OSR , LUCERNE, GRASSL
levels(data2009$Crop.Analyses)=c("cereal","colza","luzerne","prairie")

source("util/modifs_fichier_2.R",encoding = "latin1")
data2009=modifs_fichier(tab=data2009)

kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2009 <- data2009[, kept ]
colnames(weeds2009)
weeds2009C<-subset(weeds2009,weeds2009$Crop.Analyses!="friche")

#weeds2009C$Crop.Analyses[weeds2009C$pt==1]
unique(weeds2009C$pt)
weeds2009C$pt=as.factor(weeds2009C$pt)
colnames(weeds2009C)[4]="carre.parc"

###unique(weeds2009C$Par.interf)
#[1] "In"  "pa" "in" "Pa" 
weeds2009C$Par.interf[weeds2009C$Par.interf=="In"]="in"
weeds2009C$Par.interf[weeds2009C$Par.interf=="Pa"]="pa"

weeds2009C1 <- cbind(weeds2009C, as.factor(weeds2009C$Espèce_origin))
colnames(weeds2009C1) [13] <- "sp"

weeds2009C1$ParPointTri =paste(weeds2009C1$No_parcelle,"2009",
                               weeds2009C1$Par.interf,sep="-")

write.table(weeds2009C1, "data/generated/weeds2009.csv", sep = ";")
rm(weeds2009,weeds2009C,weeds2009C1,data2009)

#########################################################
# 2010
#########################################################
print("year 2010")
data2010=read.csv("data/raw/monitoring2010.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")
data2010<-data2010[-which(data2010$Crop.Analyses=="#N/A"),]
data2010$Crop.Analyses=as.factor(data2010$Crop.Analyses)
levels(data2010$Crop.Analyses)=c("cereal","luzerne","prairie")

###Correction des noms d'espèces adventices
##code Joël
source("util/modifs_fichier_log10.R",encoding = "latin1")
data2010=modifs_fichier(tab=data2010)

kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2010 <- data2010[, kept ]
weeds2010C<-subset(weeds2010,weeds2010$Crop.Analyses!="friche")

unique(weeds2010C$pt)
weeds2010C$pt=as.factor(weeds2010C$pt)
colnames(weeds2010C)[4]="carre.parc"

weeds2010C$Par.interf[weeds2010C$Par.interf=="In"]="in"
weeds2010C$Par.interf[weeds2010C$Par.interf=="Pa"]="pa"

weeds2010C1 <- cbind(weeds2010C, as.factor(weeds2010C$Espèce_origin))
colnames(weeds2010C1) [13] <- "sp"

weeds2010C1$ParPointTri =paste(weeds2010C1$No_parcelle,"2010",
                               weeds2010C1$Par.interf,sep="-")

write.table(weeds2010C1, "data/generated/weeds2010.csv", sep = ";")
rm(weeds2010,weeds2010C,weeds2010C1,data2010)

#########################################################
# 2011
#########################################################
print("year 2011")
data2011=read.csv("data/raw/monitoring2011.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")
unique(data2011$Crop.Analyses) #céréale/Cereal, colza/Colza,Maïs/maïs
data2011$Crop.Analyses=as.factor(data2011$Crop.Analyses)
levels(data2011$Crop.Analyses)="cereal"
data2011$Espece_origin[data2011$Espèce_origin=="Festuca-ovina/rubra"]="Festuca-rubra" #Festuca-ovina/rubra

###Correction des noms d'espèces adventices
##code Joël
source("util/modifs_fichier_log10b.R",encoding = "latin1") #pour parcelle 
data2011=modifs_fichier(tab=data2011)

kept <-  c(3,4,6,7,8,9,11,12,13,14,22,31)

weeds2011 <- data2011[, kept ]
weeds2011C<-subset(weeds2011,weeds2011$Crop.Analyses!="friche")

weeds2011C$pt=as.factor(weeds2011C$pt)
colnames(weeds2011C)[4]="carre.parc"

weeds2011C$Par.interf[weeds2011C$Par.interf=="In"]="in"
weeds2011C$Par.interf[weeds2011C$Par.interf=="Pa"]="pa"

weeds2011C1 <- cbind(weeds2011C, as.factor(weeds2011C$Espèce_origin))
colnames(weeds2011C1) [13] <- "sp"

weeds2011C1$ParPointTri =paste(weeds2011C1$No_parcelle,"2011",
                               weeds2011C1$Par.interf,sep="-")

write.table(weeds2011C1, "data/generated/weeds2011.csv", sep = ";")
rm(weeds2011,weeds2011C,weeds2011C1,data2011)

#########################################################
# 2012
#########################################################
# print("year 2012")
# data2012=read.csv("data/raw/monitoring2012.csv", sep=";", dec= "," , 
#                   stringsAsFactors=FALSE,h=T)
# unique(data2012$Crop.Analyses) #céréale/Cereal, colza/Colza,Maïs/maïs
# data2012$Crop.Analyses=as.factor(data2012$Crop.Analyses)
# levels(data2012$Crop.Analyses)="cereal"

# ###Correction des noms d'espèces adventices
# ##code Joël
# source("util/modifs_fichier_log10b.R")
# data2012=modifs_fichier(tab=data2012)

# kept <-  c(3,4,5,6,7,8,9,10,11,12,19,20)
# weeds2012 <- data2012[, kept ]
# weeds2012C=weeds2012

# weeds2012C$pt=as.factor(weeds2012C$pt)
# weeds2012C$carre.parc=as.factor(weeds2012C$carre.parc)

# weeds2012C1 <- cbind(weeds2012C, as.factor(weeds2012C$Espèce_origin))
# colnames(weeds2012C1) [13] <- "sp"

# weeds2012C1$ParPointTri =paste(weeds2012C1$No_parcelle,"2012",
#                                weeds2012C1$Par.interf,sep="-")

# write.table(weeds2012C1, "data/generated/weeds2012.csv", sep = ";")
# rm(weeds2012,weeds2012C,weeds2012C1,data2012)

#########################################################
# 2013
#########################################################
print("year 2013")
data2013=read.csv("data/raw/monitoring2013.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,
                  encoding = "latin1")

unique(data2013$Crop.Analyses) #céréale/Cereal, colza/Colza,Maïs/maïs
data2013$Crop.Analyses[data2013$Crop.Analyses=="céréale"]="cereal"
data2013$Crop.Analyses[data2013$Crop.Analyses=="Maïs"]="maize"
data2013$Crop.Analyses[data2013$Crop.Analyses=="Colza"]="colza"
data2013$Crop.Analyses[data2013$Crop.Analyses=="Tournesol"]="tournesol"
data2013$Crop.Analyses[data2013$Crop.Analyses=="luzerne"]="luzerne"
data2013$Crop.Analyses[data2013$Crop.Analyses=="trèfle"]="trefle"
data2013b=data2013

data2013$abondance[is.na(data2013$abondance)==T]=1

source("util/modifs_fichier_log10.R", encoding = "latin1")
data2013b=modifs_fichier(tab=data2013)

kept <-  c(3:12,19)

weeds2013 <- data2013b[, kept ]
weeds2013C<-subset(weeds2013,weeds2013$Crop.Analyses!="friche")

weeds2013C$pt=as.factor(weeds2013C$pt)
colnames(weeds2013C)[4]="carre.parc"

weeds2013C$Par.interf[weeds2013C$Par.interf=="In"]="in"
weeds2013C$Par.interf[weeds2013C$Par.interf=="Pa"]="pa"

weeds2013C1 <- cbind(weeds2013C, as.factor(weeds2013C$Espèce_origin))
colnames(weeds2013C1) [12] <- "sp"

weeds2013C1$ParPoint=paste(weeds2013C1$carre.parc,"2013",weeds2013C1$Par.interf,
                           weeds2013C1$pt,sep="-")

weeds2013C1$ParPointTri =paste(weeds2013C1$No_parcelle,"2013",
                               weeds2013C1$Par.interf,sep="-")

write.table(weeds2013C1, "data/generated/weeds2013.csv", sep = ";")
rm(weeds2013,weeds2013C,weeds2013C1,data2013)

#########################################################
# 2014
#########################################################
print("year 2014")
data2014=read.csv("data/raw/monitoring2014.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,encoding="latin1")
# Remove the field sampled two times:
data2014 = data2014[-which(data2014$carré.parc == "5641-6103b"), ] 

source("util/modifs_fichier.R", encoding = "latin1")
data2014=modifs_fichier(tab=data2014)

kept <- c(3:12,19)

weeds2014 <- data2014[, kept ]
weeds2014C=weeds2014

weeds2014C$pt <- tolower(weeds2014C$pt)

weeds2014C$Par.interf[weeds2014C$Par.interf=="Pa" ] <- "pa"

weeds2014C1 <- cbind(weeds2014C, as.factor(weeds2014C$Espèce_origin))
colnames(weeds2014C1) [12] <- "sp"
colnames(weeds2014C1) [10]= "Espece_origin"

weeds2014C1$ParPoint=paste(weeds2014C1$carré.parc,"2014",weeds2014C1$Par.interf,
                            weeds2014C1$pt,sep="-")

weeds2014C1$ParPointTri =paste(weeds2014C1$No_parcelle,"2014",
                               weeds2014C1$Par.interf,sep="-")

# remove trailing whitespaces and lower strings
weeds2014C1$pt <- tolower(trimws(weeds2014C1$pt))

# remove the alphabetic part to get the plot number
# [:alpha:] means any alphabetic character
weeds2014C1$plot    <- gsub("[[:alpha:]]", "", weeds2014C1$pt)

# remove the numeric part to get the quadrat number
# [:digit:] means any digit character
weeds2014C1$quadrat <- gsub("[[:digit:]]", "", weeds2014C1$pt)

write.table(weeds2014C1, "data/generated/weeds2014.csv", sep = ";")
rm(weeds2014,weeds2014C,weeds2014C1,data2014)

#########################################################
# 2015
#########################################################
print("year 2015")
data2015=read.csv("data/raw/monitoring2015.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,encoding = "latin1")
data2015$Crop.Analyses[data2015$Crop.Analyses=="luzerne "]="luzerne"
data2015$Crop.Analyses[data2015$Crop.Analyses=="orge "]="orge"
data2015$Crop.Analyses[data2015$Crop.Analyses=="orge hivers"]="orge"
data2015$Crop.Analyses[data2015$Crop.Analyses=="orge"]="cereal"
data2015$Crop.Analyses[data2015$Crop.Analyses=="maïs"]="maize"
data2015$Crop.Analyses[data2015$Crop.Analyses=="blé"]="cereal"
data2015$Crop.Analyses[data2015$Crop.Analyses=="céréale"]="cereal"
data2015$Crop.Analyses[data2015$Crop.Analyses=="ce/luz"]="ce/leg"

data2015b=data2015
data2015b=subset(data2015b,data2015b$Crop.Analyses!="friche")

###Correction des noms d'espèces adventices
##code Joël
source("util/modifs_fichier.R", encoding = "latin1")
data2015b=modifs_fichier(tab=data2015b)

kept <- c(3:8,10:13,20:21)

weeds2015 <- data2015b[, kept ]
weeds2015C<-subset(weeds2015,weeds2015$Crop.Analyses!="friche")
weeds2015C$Crop.Analyses[weeds2015C$Crop.Analyses=="orge"]="cereal"
weeds2015C$Crop.Analyses[weeds2015C$Crop.Analyses=="céréale"]="cereal"
weeds2015C$Crop.Analyses[weeds2015C$Crop.Analyses=="blé"]="cereal"
weeds2015C$Crop.Analyses[weeds2015C$Crop.Analyses=="orge hivers"]="cereal"
unique(weeds2015C$Crop.Analyses)

weeds2015C$pt[weeds2015C$pt=="6q" ] <- "6d"

weeds2015C1 <- cbind(weeds2015C, as.factor(weeds2015C$Espèce_origin))
colnames(weeds2015C1) [13] <- "sp"

weeds2015C1$ParPoint=paste(weeds2015C1$carré.parc,"2015",weeds2015C1$Par.interf,
                           weeds2015C1$pt,sep="-")

weeds2015C1$ParPointTri = paste(weeds2015C1$No_parcelle,"2015",
                               weeds2015C1$Par.interf,sep="-")

# remove trailing whitespaces and lower strings
weeds2015C1$pt <- tolower(trimws(weeds2015C1$pt))

# remove the alphabetic part to get the plot number
# [:alpha:] means any alphabetic character
weeds2015C1$plot    <- gsub("[[:alpha:]]", "", weeds2015C1$pt)

# remove the numeric part to get the quadrat number
# [:digit:] means any digit character
weeds2015C1$quadrat <- gsub("[[:digit:]]", "", weeds2015C1$pt)

write.table(weeds2015C1, "data/generated/weeds2015.csv", sep=";")
rm(weeds2015,weeds2015C,weeds2015C1,data2015)

#########################################################
# 2016
#########################################################
print("year 2016")
data2016=read.csv("data/raw/monitoring2016.csv", sep=";", dec= "," , 
                  stringsAsFactors=FALSE,h=T,encoding = "latin1")

# Complete the Crop.Analyses field with OSC (cf onglet infos in the bota 2016 file)
unique(data2016$Crop.Analyses)
data2016[which(data2016$OSC==41),]$Crop.Analyses = "blé"
data2016[which(data2016$OSC==40),]$Crop.Analyses = "céréale"
data2016[which(data2016$OSC==50),]$Crop.Analyses = "colza"
data2016[which(data2016$OSC==61),]$Crop.Analyses = "lin"
data2016[which(data2016$OSC==76),]$Crop.Analyses = "lin/lentille"
data2016[which(data2016$OSC==25),]$Crop.Analyses = "luzerne"
data2016[which(data2016$OSC==71),]$Crop.Analyses = "maïs"
data2016[which(data2016$OSC==48),]$Crop.Analyses = "ce/leg"
data2016[which(data2016$OSC==44),]$Crop.Analyses = "orge hiver"
data2016[which(data2016$OSC==77),]$Crop.Analyses = "pavot"
data2016[which(data2016$OSC==62),]$Crop.Analyses = "pois"
data2016[which(data2016$OSC==11),]$Crop.Analyses = "prairie"
data2016[which(data2016$OSC==72),]$Crop.Analyses = "tournesol"
data2016[which(data2016$OSC==26),]$Crop.Analyses = "trefle"
data2016[which(data2016$OSC==42),]$Crop.Analyses = "ble barbu"

###Correction des noms d'espèces adventices
##code Joël
source("util/modifs_fichier.R", encoding = "latin1")
data2016=modifs_fichier(tab=data2016)
####
kept <- c(3:4, 6:9, 11:14, 22, 34)
weeds2016 <- data2016[, kept ]
weeds2016C=weeds2016
weeds2016C$Crop.Analyses[weeds2016C$Crop.Analyses=="ble barbu"]="cereal"
weeds2016C$Crop.Analyses[weeds2016C$Crop.Analyses=="céréale"]="cereal"
weeds2016C$Crop.Analyses[weeds2016C$Crop.Analyses=="blé"]="cereal"
weeds2016C$Crop.Analyses[weeds2016C$Crop.Analyses=="orge hiver"]="cereal"
unique(weeds2016C$Crop.Analyses)

weeds2016C1 <- cbind(weeds2016C, as.factor(weeds2016C$Espèce_origin))
colnames(weeds2016C1) [13] <- "sp"

weeds2016C1$ParPoint=paste(weeds2016C1$carré.parc,"2016",weeds2016C1$Par.interf,
                           weeds2016C1$pt,sep="-")

weeds2016C1$ParPointTri =paste(weeds2016C1$No_parcelle,"2016",
                               weeds2016C1$Par.interf,sep="-")

# remove trailing whitespaces and lower strings
weeds2016C1$pt <- tolower(trimws(weeds2016C1$pt))

# remove the alphabetic part to get the plot number
# [:alpha:] means any alphabetic character
weeds2016C1$plot    <- gsub("[[:alpha:]]", "", weeds2016C1$pt)

# remove the numeric part to get the quadrat number
# [:digit:] means any digit character
weeds2016C1$quadrat <- gsub("[[:digit:]]", "", weeds2016C1$pt)

write.table(weeds2016C1,file="data/generated/weeds2016.csv",sep=";")
rm(weeds2016,weeds2016C,weeds2016C1)
