# Lecture des fichiers enquetes
#
# Cr�e le 2 aout 2016 par Sabrina Gaba
# Modifi� le 22/02/2017 par Xavier Laviron
#
##############################################################

##setwd("~/Stage/2016/M2_AlexandraCottel")
# setwd("~/Donnees/Chize-Enquetes/Prog")

#fonctions utilis�es dans le script
source("util/functions.R", encoding = "latin1")

##Charger le fichier de donn�es enqu�tes
fich <- read.csv("data/generated/BDD_full.csv", na.strings = c("NA", ""))

# summary(fich) # synth�se des donn�es du fichier
# names(fich) #nom des colonnes

##################CREATION D'UN ID_PARCELLE##########################
##Plusieurs enqu�tes peuvent correspondre � un m�me ID_Parcelle si la 
##parcelle a �t� exp�riment�e plusieurs ann�es de suite.
##le Nouvel identifiant parcelle va permettre de trier les parcelles en 
##tenant compte des ann�es des exp�rimentations et/ou des protocoles.

ID_Parc_Tri <- paste(fich[,1], fich[,2], fich[,4], sep = "_") 


##################INFOS GENERALES####################################
## Cr�ation d'un sous-jeu de donn�es correspondant aux informations g�n�rales
## sur la parcelle

kept <- c(1:13)
fich_general <- fich[, kept]

#1ligne = 1 parcelle
fich_general <-
  aggregate(data.frame(Colza_Nectar           = fich_general$Colza_Nectar,
                       Exp�rimentation_autre  = fich_general$Exp�rimentation_autre,
                       point_X                = fich_general$point_X,
                       point_Y                = fich_general$point_Y,
                       Type_Culture           = fich_general$Type_Culture, 
                       Type_CultureSimplifi�e = fich_general$Type_CultureSimplifi�e,
                       Surface_ha             = fich_general$Surface_ha,
                       Ann�e_Enqu�te          = fich_general$Ann�e_Enqu�te,
                       Type_Formulaire        = fich_general$Type_Formulaire), 
            by = list(as.character(fich_general$ID_Parcelle),
                      fich_general$ID_Exploitation,
                      as.character(fich_general$Ann�e_SuiviParcelle)),
            function (x) unique(na.omit(x)))

colnames(fich_general)[1:3] <-
  c("ID_Parcelle", "ID_Exploitation", "Ann�e_SuiviParcelle")

fich_general <-
  data.frame(ID_Parcelle            = as.character(fich_general[,1]),
             ID_Exploitation        = as.character(fich_general[,2]),
             Ann�e_SuiviParcelle    = factor(fich_general[,3]),
             Colza_Nectar           = funlist(fich_general$Colza_Nectar),
             Exp�rimentation_autre  = funlist_multi(fich_general$Exp�rimentation_autre,
                                                    missing.data = "Aucune"),
             point_X                = funlist(fich_general$point_X),
             point_Y                = funlist(fich_general$point_Y),
             Type_Culture           = funlist_multi(fich_general$Type_Culture), 
             Type_CultureSimplifi�e = funlist_multi(fich_general$Type_CultureSimplifi�e),
             Surface_ha             = funlist(fich_general$Surface_ha),
             Ann�e_Enqu�te          = funlist(fich_general$Ann�e_Enqu�te),
             Type_Formulaire        = funlist_multi(fich_general$Type_Formulaire))

##################INFOS EXPLOITATIONS####################################
##Cr�ation d'un sous-jeu de donn�es correspondant aux informations sur l'exploitation

kept <- c(1:2, 4, 14:24)
fich_farm <- fich[,kept]

#1ligne = 1 parcelle
#Cr�er un variable materiel
Materiel <-
  paste(fich_farm$Mat�riel_Exploitation_Type,
        fich_farm$Mat�riel_Largeur..en.m.,
        fich_farm$Mat�riel_Puissance..en.cv.,
        fich_farm$Mat�riel_Conso..en.l.heure.,
        fich_farm$Mat�riel_D�bit..ha.heure,
        sep="_")
                 
fich_farm <-
  aggregate(data.frame(Type_Exploitation  = fich_farm$Type_Exploitation,
                       Syst_Prod          = fich_farm$Syst_Prod,
                       Parcelle_MAE       = fich_farm$Parcelle_MAE, 
                       Type_de_sol        = fich_farm$Type_de_sol,
                       Coop               = fich_farm$Coop,
                       ETA                = fich_farm$ETA,
                       Materiel           = Materiel),
            by = list(as.character(fich_farm$ID_Parcelle),
                      fich_farm$ID_Exploitation,
                      as.character(fich_farm$Ann�e_SuiviParcelle)),
            function (x) unique(na.omit(x)))

colnames(fich_farm)[1:3] <-
  c("ID_Parcelle", "ID_Exploitation", "Ann�e_SuiviParcelle")

fich_farm <-
  data.frame(ID_Parcelle          = as.character(fich_farm[,1]),
             ID_Exploitation      = as.character(fich_farm[,2]),
             Ann�e_SuiviParcelle  = factor(fich_general[,3]),
             Syst_Prod            = funlist_multi(fich_farm$Syst_Prod),
             Parcelle_MAE         = funlist_multi(fich_farm$Parcelle_MAE),
             Type_de_sol          = funlist_multi(fich_farm$Type_de_sol),
             Coop                 = funlist_multi(fich_farm$Coop),
             ETA                  = funlist_multi(fich_farm$ETA),
             Materiel             = funlist_multi(fich_farm$Materiel))

#########################SEMIS####################################
##Cr�ation d'un sous-jeu de donn�es correspondant aux informations au semis

kept <- c(1:2,4,25:39)
fich_semis <- fich[,kept]

#1ligne = 1 parcelle

fich_semis <-
  aggregate(fich_semis[, 4:dim(fich_semis)[2]],
            by = list(as.character(fich_semis$ID_Parcelle),
                      fich_semis$ID_Exploitation,
                      as.character(fich_semis$Ann�e_SuiviParcelle)),
            function (x) unique(na.omit(x)))

# convertit les dates en objets dates
fich_semis$Date_Semis_Inter <-
  lapply(fich_semis$Date_Semis_Inter, strptime, "%d/%m/%Y")
fich_semis$Date_R�colte_Inter <-
  lapply(fich_semis$Date_R�colte_Inter, strptime, "%d/%m/%Y")
fich_semis$Date_Semis <-
  lapply(fich_semis$Date_Semis, strptime, "%d/%m/%Y")

colnames(fich_semis)[1:3] <-
  c("ID_Parcelle", "ID_Exploitation", "Ann�e_SuiviParcelle")

fich_semis <-
  data.frame(ID_Parcelle          = as.character(fich_semis[,1]),
             ID_Exploitation      = as.character(fich_semis[,2]),
             Ann�e_SuiviParcelle  = factor(fich_semis[, 3]),
             Nb_interculture      = unlist(lapply(fich_semis$Interculture, length)),
             Interculture         = funlist_multi(fich_semis$Interculture,
                                                  missing.data = "Aucune"),
             Date_Semis_Inter     = funlist(fich_semis$Date_Semis_Inter), # � modifier funlist_multi quand il y a aura des valeurs
             Date_R�colte_Inter   = funlist(fich_semis$Date_R�colte_Inter), # idem
             R�colte_Inter        = funlist(fich_semis$Date_R�colte_Inter),
             M�lange              = funlist_multi(fich_semis$M�lange),
             Vari�t�              = funlist(fich_semis$Vari�t�),
             Nom_.Vari�t�         = funlist_multi(fich_semis$Nom_.Vari�t�),
             Type_Vari�t�         = funlist_multi(fich_semis$Type_Vari�t�),
             Date_Semis           = funlist_multi(fich_semis$Date_Semis, date = T),
             Densit�_Semis        = funlist(fich_semis$Densit�_Semis),
             Unit�_Densit�Semis   = funlist_multi(fich_semis$Unit�_Densit�Semis),
             Inter.rang_cm        = funlist(fich_semis$Inter.rang_cm),
             Enrobage             = funlist_multi(fich_semis$Enrobage),
             Obj_Enrobage         = funlist_multi(fich_semis$Obj_Enrobage),
             Produit_Enrobage     = funlist_multi(fich_semis$Produit_Enrobage))

##pour modifier les dates de semis et r�coltes
###as.Date(VALEUR,origin="1970-01-01")

############################RECOLTE####################################
##Cr�ation d'un sous-jeu de donn�es correspondant aux informations � la r�colte

kept <- c(1:2,4,40:45)
fich_recolte <- fich[,kept]

#1ligne = 1 parcelle

fich_recolte <-
  aggregate(fich_recolte[, 4:dim(fich_recolte)[2]],
            by = list(as.character(fich_recolte$ID_Parcelle),
                      fich_recolte$ID_Exploitation,
                      as.character(fich_recolte$Ann�e_SuiviParcelle)),
            function (x) unique(na.omit(x)))

# convertit les dates en objets dates
fich_recolte$Date_R�colte <- lapply(fich_recolte$Date_R�colte, strptime, "%d/%m/%Y")
fich_recolte$Date_Fauche  <- lapply(fich_recolte$Date_Fauche, strptime, "%d/%m/%Y")

colnames(fich_recolte)[1:3] <-
  c("ID_Parcelle", "ID_Exploitation", "Ann�e_SuiviParcelle")

fich_recolte <-
  data.frame(ID_Parcelle              = as.character(fich_recolte[, 1]),
             ID_Exploitation          = as.character(fich_recolte[, 2]),
             Ann�e_SuiviParcelle      = factor(fich_recolte[, 3]),
             Date_R�colte             = funlist_multi(fich_recolte$Date_R�colte, date = T) ,
             Paille_R�colte           = funlist(fich_recolte$Paille_R�colte) ,
             ObjRdt..qtx.             = funlist(fich_recolte$ObjRdt..qtx.),
             Rdt_Qtx                  = funlist(fich_recolte$Rdt_Qtx),
             Date_Fauche              = funlist(fich_recolte$Date_Fauche),# funlist_multi(fich_recolte$Date_Fauche,date=T)
             Rdt_Fauche..en.t.MS.ha.  = funlist(fich_recolte$Rdt_Fauche..en.t.MS.ha.))

############################TRAVAIL DU SOL###############################################
##Cr�ation d'un sous-jeu de donn�es correspondant aux informations sur le travail du sol

kept <- c(1:2,4,46:49)
fich_Wsol <- fich[,kept]

#1ligne = 1 parcelle

#Estimation de la profondeur du travail du sol
#####calcul de la valeur moyenne entre les deux valeurs donn�es "8_10"
#####remplace "superficiel" par 2
prof_Wsol <- funWsol(fich_Wsol$Profondeur_Tsol_.cm)

#calcul de la profondeur max du Travail du sol
prof_Wsol_max <-
  aggregate(data.frame(prof_Wsol), 
            by = list(as.character(fich_Wsol$ID_Parcelle),
                      fich_Wsol$ID_Exploitation,
                      as.character(fich_Wsol$Ann�e_SuiviParcelle)),
            fmax)[,4]

#calcul de la profondeur min du Travail du sol
prof_Wsol_min <-
  aggregate(data.frame(prof_Wsol), 
            by = list(as.character(fich_Wsol$ID_Parcelle),fich_Wsol$ID_Exploitation,
                      as.character(fich_Wsol$Ann�e_SuiviParcelle)),
            fmin)[,4]

#liste des profondeur du sol par parcelle
prof_Wsol <-
  aggregate(data.frame(prof_Wsol), 
            by = list(as.character(fich_Wsol$ID_Parcelle),
                      fich_Wsol$ID_Exploitation,
                      as.character(fich_Wsol$Ann�e_SuiviParcelle)),
            funique)[,4]

#calcul du nb de passages de Travail du sol
nb_Wsol <-
  aggregate(data.frame(fich_Wsol$Date_Tsol), 
            by = list(as.character(fich_Wsol$ID_Parcelle),
                      fich_Wsol$ID_Exploitation,
                      as.character(fich_Wsol$Ann�e_SuiviParcelle)),
            function (x) length(unique(na.omit(x))))[,4]

fich_Wsol <-
  aggregate(fich_Wsol[,c(4,7)], #:dim(fich_Wsol)[2]],
            by = list(as.character(fich_Wsol$ID_Parcelle),
                      fich_Wsol$ID_Exploitation,
                      as.character(fich_Wsol$Ann�e_SuiviParcelle)),
            function (x) unique(na.omit(x)))

colnames(fich_Wsol)[1:3] <-
  c("ID_Parcelle", "ID_Exploitation", "Ann�e_SuiviParcelle")

# ajout des variables calcul�es dans le tableau fich_Wsol
fich_Wsol$nb_Wsol       <- nb_Wsol
fich_Wsol$prof_Wsol     <- prof_Wsol
fich_Wsol$prof_Wsol_max <- prof_Wsol_max
fich_Wsol$prof_Wsol_min <- prof_Wsol_min

# convertit les dates en objets dates
fich_Wsol$Date_Tsol <- lapply(fich_Wsol$Date_Tsol, strptime, "%d/%m/%Y")

rm(nb_Wsol, prof_Wsol, prof_Wsol_max, prof_Wsol_min)

fich_Wsol <-
  data.frame(ID_Parcelle          = as.character(fich_Wsol[, 1]),
             ID_Exploitation      = as.character(fich_Wsol[, 2]),
             Ann�e_SuiviParcelle  = factor(fich_Wsol[, 3]),
             Type_Tsol            = funlist_multi(fich_Wsol$Type_Tsol),
             Date_Tsol            = funlist_multi(fich_Wsol$Date_Tsol, date = T),
             nb_Wsol              = funlist(fich_Wsol$nb_Wsol),
             prof_Wsol            = funlist(fich_Wsol$prof_Wsol),
             prof_Wsol_max        = funlist(fich_Wsol$prof_Wsol_max),
             prof_Wsol_min        = funlist(fich_Wsol$prof_Wsol_min))



############################AZOTE + EAU#######################################################
##Cr�ation d'un sous-jeu de donn�es correspondant aux informations sur les apports azot�s

kept <- c(1:2,4,50:56)
fich_Ferti <- fich[,kept]

#1ligne = 1 parcelle
fich_Ferti$Parc_Tri <- ID_Parc_Tri

####informations sur la fertilisation et l'irrigation
info_Ferti <-
  aggregate(fich_Ferti[,c(4,6,7,10)],
            by = list(as.character(fich_Ferti$ID_Parcelle),
                      fich_Ferti$ID_Exploitation,
                      as.character(fich_Ferti$Ann�e_SuiviParcelle)), 
            function (x) unique(na.omit(x)))

info_Ferti <-
  data.frame(ID_Parcelle          = as.character(info_Ferti[, 1]),
             ID_Exploitation      = as.character(info_Ferti[, 2]),
             Ann�e_SuiviParcelle  = factor(info_Ferti[, 3]),
             Irrigation           = funlist_multi(info_Ferti$Irrigation),
             Fertilisation        = funlist_multi(info_Ferti$Fertilisation),
             Produit_Ferti        = funlist_multi(info_Ferti$Produit_Ferti),
             Date_Ferti           = funlist_multi(lapply(info_Ferti$Date_Ferti,
                                                         strptime, "%d/%m/%Y"),
                                                  date = TRUE))

###V�rification que l'ensemble des valeurs soit en KG/HA, L/HA ou Unit�s/HA
cat("V�rification des doses de fertilisants:\n")
funDoseN(z = fich_Ferti$ID_Parcelle,
         x = fich_Ferti$Unit�_dose,
         y = fich_Ferti$Produit_Ferti)

#################TROUVER LES EQUIVALENTS PACK/HA
###Table de contingence
fich_Ferti <-
  xtabs(as.numeric(as.character(Dose_Ferti)) ~ ID_Parc_Tri + Produit_Ferti,
        data = fich_Ferti)

############################PHYTOSANITAIRES#########################################
##Cr�ation d'un sous-jeu de donn�es correspondant aux informations sur les
##produits phytosanitaires

kept <- c(1:2,4,10,57:62)
fich_Phyto <- fich[,kept]

#1ligne = 1 parcelle

fich_Phyto$ID_Parc_Tri <- ID_Parc_Tri
surf <-
  aggregate(fich_Phyto[,4],
            by = list(as.character(ID_Parc_Tri)),
            #   by=list(as.character(fich_Phyto$ID_Parcelle),fich_Phyto$ID_Exploitation,
            #           as.character(fich_Phyto$Ann�e_SuiviParcelle)), 
            function (x) unique(x))

#colnames(surf)[1:3]=c("ID_Parcelle","ID_Exploitation","ID_Ann�e_SuiviParcelle")
colnames(surf)[1] <- "ID_Parc_Tri"

###Herbicides
fich_Herbi <- subset(fich_Phyto, fich_Phyto$Protection_vis�e == "Herbicide")
fich_Herbi$Produit_phyto  <- fich_Herbi$Produit_phyto[, drop=T]
fich_Herbi$Unit�_dose.1   <- fich_Herbi$Unit�_dose.1[, drop=T] #peut �tre modifi�e par Unit�_dose_phyto

####informations sur les traitements hors dose
info_Herbi <-
  aggregate(fich_Herbi[, c(1, 2, 3, 8, 9, 10)], 
            by = list(fich_Herbi$ID_Parc_Tri),
            unique)

info_Herbi$Surface_ha <- surf[(surf[, 1] %in% info_Herbi[, 1]), 2]

info_Herbi <-
  data.frame(ID_Parc_Tri          = as.character(info_Herbi[, 1]),
             ID_Parcelle          = as.character(info_Herbi[, 2]),
             ID_Exploitation      = as.character(info_Herbi[, 3]),
             Ann�e_SuiviParcelle  = factor(info_Herbi[, 4]),
             Unit�_dose.1         = funlist_multi(info_Herbi$Unit�_dose.1),
             Surface_ha           = funlist(info_Herbi$Surface_ha),
             #  Surface_trait�e_ha= funlist_multi(info_Herbi$Surface_trait�e_ha,missing.data=0),
             Date_Phyto           = funlist_multi(lapply(info_Herbi$Date_Phyto, strptime,
                                                         "%d/%m/%Y"),
                                                  date=TRUE))

###V�rification que l'ensemble des valeurs soit en KG/HA ou L/HA
cat("V�rification des doses d'herbicides:\n")
funDose(z = fich_Herbi$ID_Parc_Tri,
        x = fich_Herbi$Unit�_dose.1,
        y = fich_Herbi$Produit_phyto)
#################TROUVER LES EQUIVALENTS PACK/HA POUR NOVALL et SPRINGBOK
###Tables de contingence

temp <- plyr::count(fich_Herbi, c("ID_Parc_Tri", "Produit_phyto", "Date_Phyto"))
Nb_TraitH_Rep <- xtabs(temp$freq ~ ID_Parc_Tri + Produit_phyto, data = temp)

Surf_traitH <-
  xtabs(as.numeric(as.character(Surface_trait�e_ha)) ~ ID_Parc_Tri + Produit_phyto,
        data=fich_Herbi) 

fich_Herbi <-
  xtabs(as.numeric(as.character(Dose_Phyto)) ~ ID_Parc_Tri + Produit_phyto,
        data = fich_Herbi,
        exclude = NULL,
        na.action = na.pass)  

###Insecticides
fich_Insect <- subset(fich_Phyto, fich_Phyto$Protection_vis�e == "Insecticide")
fich_Insect$Produit_phyto <- fich_Insect$Produit_phyto[, drop=T]
fich_Insect$Unit�_dose.1  <- fich_Insect$Unit�_dose.1[, drop=T] #peut �tre modifi�e par Unit�_dose_phyto

####informations sur les traitements hors dose
info_Insect <-
  aggregate(fich_Insect[, c(1, 2, 3, 8, 9, 10)], 
            by = list(fich_Insect$ID_Parc_Tri),
            unique)

info_Insect$Surface_ha <- surf[(surf[, 1] %in% info_Insect[, 1]), 2]

info_Insect <-
  data.frame(ID_Parc_Tri          = as.character(info_Insect[, 1]),
             ID_Parcelle          = as.character(info_Insect[, 2]),
             ID_Exploitation      = as.character(info_Insect[, 3]),
             Ann�e_SuiviParcelle  = factor(info_Insect[, 4]),
             Unit�_dose.1         = funlist_multi(info_Insect$Unit�_dose.1),
             Surface_ha           = funlist(info_Insect$Surface_ha),
             Surface_trait�e_ha   = funlist_multi(info_Insect$Surface_trait�e_ha,
                                                  missing.data = 0),
             Date_Phyto           = funlist_multi(lapply(info_Insect$Date_Phyto,
                                                         strptime, "%d/%m/%Y"),
                                                  date = TRUE))

###V�rification que l'ensemble des valeurs soit en KG/HA ou L/HA
cat("V�rification des doses d'insecticides:\n")
funDose(z = fich_Insect$ID_Parc_Tri,
        x = fich_Insect$Unit�_dose.1,
        y = fich_Insect$Produit_phyto)
##########"6029_IB1_2013:KARATE K"  "6029_IB1_2013:MAGEOS MD" en G/HA => dose � modifier /1000

###Tables de contingence
temp <- plyr::count(fich_Insect, c("ID_Parc_Tri", "Produit_phyto", "Date_Phyto"))
Nb_TraitI_Rep <- xtabs(temp$freq ~ ID_Parc_Tri + Produit_phyto,
                       data = temp)
Surf_traitI <- xtabs(as.numeric(as.character(Surface_trait�e_ha)) ~ ID_Parc_Tri + Produit_phyto, 
                     data = fich_Insect) 
fich_Insect <- xtabs(as.numeric(as.character(Dose_Phyto)) ~ ID_Parc_Tri + Produit_phyto,
                     data = fich_Insect,
                     exclude = NULL, na.action = na.pass)  
##########fich_Insect[,"CLAMEUR"]=fich_Insect[,"CLAMEUR"]/1000

###Fongicides
fich_Fongi <- subset(fich_Phyto, fich_Phyto$Protection_vis�e == "Fongicide")
fich_Fongi$Produit_phyto  <- fich_Fongi$Produit_phyto[, drop = T]
fich_Fongi$Unit�_dose.1   <- fich_Fongi$Unit�_dose.1[,drop = T] #peut �tre modifi�e par Unit�_dose_phyto

####informations sur les traitements hors dose
info_Fongi <- aggregate(fich_Fongi[, c(1, 2, 3, 8, 9, 10)],
                        by = list(fich_Fongi$ID_Parc_Tri),
                        unique)

info_Fongi$Surface_ha <- surf[(surf[, 1] %in% info_Fongi[, 1]), 2]

info_Fongi <-
  data.frame(ID_Parc_Tri          = as.character(info_Fongi[, 1]),
             ID_Parcelle          = as.character(info_Fongi[, 2]),
             ID_Exploitation      = as.character(info_Fongi[, 3]),
             Ann�e_SuiviParcelle  = factor(info_Fongi[, 4]),
             Unit�_dose.1         = funlist_multi(info_Fongi$Unit�_dose.1),
             Surface_ha           = funlist(info_Fongi$Surface_ha),
             Surface_trait�e_ha   = funlist_multi(info_Fongi$Surface_trait�e_ha,
                                                   missing.data = 0),
             Date_Phyto           = funlist_multi(lapply(info_Fongi$Date_Phyto,
                                                         strptime, "%d/%m/%Y"),
                                                  date=TRUE))

###V�rification que l'ensemble des valeurs soit en KG/HA ou L/HA
cat("V�rification des doses de fongicides:\n")
funDose(z = fich_Fongi$ID_Parc_Tri,
        x = fich_Fongi$Unit�_dose.1,
        y = fich_Fongi$Produit_phyto)
## "PACK/14HA" "PACK/7HA" 
## "11075_A122_2015:PROPULSE"  "9874_A187_2015:PICTOR PRO"

###Table de contingence
temp <- plyr::count(fich_Fongi, c("ID_Parc_Tri","Produit_phyto", "Date_Phyto") )
Nb_TraitF_Rep <- xtabs(temp$freq ~ ID_Parc_Tri + Produit_phyto,
                       data=temp)
Surf_traitF <- xtabs(as.numeric(as.character(Surface_trait�e_ha)) ~ ID_Parc_Tri + Produit_phyto, 
                     data = fich_Fongi) 
fich_Fongi <-  xtabs(as.numeric(as.character(Dose_Phyto)) ~ ID_Parc_Tri + Produit_phyto,
                     data = fich_Fongi,
                     exclude = NULL, na.action = na.pass)
##########

#http://www.lbagri.fr/client/document/propulse_61.pdf
