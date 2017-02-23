# Code pour calculer des Doses, IFT, NODU...
#
# Crée le 2 aout 2016 par Sabrina Gaba
# Dernières modifications: le 10 aout 2016  par Sabrina
#
##############################################################

# setwd("~/Donnees/Chize-Enquetes/Referentiel")
##setwd("~/BaseDonnees/Herbicides")
dose_recommandee <- read.table("data/dose-reference-ift-grande-culture.csv",
                               h = T,
                               sep = ";",
                               na.strings = "",
                               encoding = "latin1")
ephy <- read.csv("~/downloads/usages_des_produits_autorises_v2_utf8-26012017.csv",
                sep = ";", encoding = "utf8")
# setwd("~/Donnees/Chize-Enquetes/Prog")

Intensite_Traitement <- function(tab          = fich_Herbi,
                                 tab_info     = info_Herbi,
                                 surf.trait   = Surf_traitH,
                                 Nb_Trait_Rep = Nb_TraitH_Rep,
                                 type         = "HERBICIDE",
                                 crop         = "Colza")
{
  # -- arguments ---------------------------------------------------------------
  # tab           = tableau de contingence créé avec load_enquete.R exemple avec
  #                 fich_Herbi
  # tab_info      = tableau créé avec load_enquete.R contenant les éléments
  #                 relatifs au traitement (ID_parcelle, ID_exploitant, Date,
  #                 Surface...)
  # surf.trait    = tableau de contingence créé avec load_enquete.R avec les
  #                 valeurs de surfaces traitées. Ex. Surf_traitH
  # Nb_Trait_Rep  = tableau de contingence contenant le nb de fois qu'un produit
  #                 a été utilisé dans une parcelle
  # type          = catégorie des produit: AUTRE, FONGICIDE, HERBICIDE ou
  #                 INSECTICIDE / ACARICIDE
  # crop          = type de cultures
  
  # remplacer les "." par des " "
  colnames(tab) <- gsub(x = colnames(tab), pattern = "\\.", replacement = " ")
  
  ##Calcul de la dose
  dose <- rowSums(tab)
  hist(dose, main = "Histogramme des doses (KG/HA)", breaks <- 20)
  
  ##Calcul IFT
  #####Sélection des doses recommandées par type et culture
  # récupére la dose recommandée pour le type de produit demandé
  DoseReco <- subset(dose_recommandee, grepl(type, dose_recommandee$Catégorie.de.produit))
  DoseReco$Culture <- DoseReco$Culture[,drop=T]
  DoseReco$Nom.du.produit <- DoseReco$Nom.du.produit[,drop=T]

  # Sélection des produits de DoseReco qui sont dans tab
  DoseReco_tab <- DoseReco[DoseReco$Nom.du.produit %in% colnames(tab),]
  DoseReco_tab <- droplevels(DoseReco_tab)

  # s'il y a des herbicides qui ne sont pas dans dose_recommandee
  if (length(unique(DoseReco_tab$Nom.du.produit)) < length(colnames(tab)))
  {
    # pesticides qui sont dans tab mais pas dans DoseReco_tab
    sel <- setdiff(colnames(tab), levels(DoseReco_tab$Nom.du.produit))

    # sélection uniquement des produits qui sont dans dose_recommandee pour
    # éviter que le 'if' plante parce que la valeur est nulle
    sel_in <- sel[sel %in% dose_recommandee$Nom.du.produit]
    sel_out <- sel[!(sel %in% dose_recommandee$Nom.du.produit)]
    cat("\nLes pesticides suivants ne sont pas dans le tableau 'dose_recommandee':\n")
    print(sel_out)

    # vérification des correspondances partielles
    cat("\nVérification de correspondances partielles entre produits...\n")
    dr_pdts <- as.character(dose_recommandee$Nom.du.produit)
    for (i in sel_out) {
      if (sum(grepl(i, dr_pdts)) > 0) {
        print(paste(i, ": il y a", dr_pdts[grepl(i, dr_pdts)][1], "dans le tableau"))  # 
      }
    }

    # vérifier dans la base de données EPHY
    cat("\nVérification de correspondances partielles dans la BDD EPHY...\n")
    ephy_pdts <- as.character(ephy$X.produit..nom.produit)
    for (i in sel_out) {
      if (sum(grepl(i, ephy_pdts)) > 0) {
        print(paste(i, ": il y a", ephy_pdts[grepl(i, ephy_pdts)][1], "dans EPHY"))
      }
    }

    x <- NULL

    cat("\nVérification du type de pesticide...\n")
    for (i in levels(as.factor(sel_in)))
    {
      # récupérer la catégorie du produit dans le tableau dose_recommandee,
      # séparation dans une variable intermédiaire pour que la condition du 'if'
      # soit plus claire. Et utilisation de 'grepl' pour permettre une
      # correspondance partielle entre 'INSECTICIDE' et 'INSECTICIDE /
      # ACARICIDE'
      cat_produit <- dose_recommandee$Catégorie.de.produit[dose_recommandee$Nom.du.produit==i]

      if(grepl(type, cat_produit))
      {
        print(i)
        x <- c(x,i)
      }
      else {
        print(paste("A corriger",i,"est un",
                    dose_recommandee$Catégorie.de.produit[dose_recommandee$Nom.du.produit==i][1],
                    sep = " "))
      }
    }
  }


  ##Calcul des doses de reférence pour la culture cible
  ##Si la culture n'est pas présente dans le référentiel, on prend le min des doses
  DoseRef <- NULL
  temp <- unique(DoseReco_tab$Nom.du.produit)
  DoseReco_tab$Dose <- as.numeric(as.character(DoseReco_tab$Dose))

  for (i in levels(temp))
  {
    if (crop %in% DoseReco_tab$Culture[DoseReco_tab$Nom.du.produit==i])
    {
      DoseRef <-
        c(DoseRef,
          DoseReco_tab$Dose[DoseReco_tab$Nom.du.produit==i & DoseReco_tab$Culture==crop][1])
    }
    else {
      DoseRef <-
        c(DoseRef,
          min(DoseReco_tab$Dose[DoseReco_tab$Nom.du.produit==i][1]))
    }
  }

  DoseRef <- data.frame(DoseRef)
  DoseRef$Nom.du.produit <- temp
  rm(temp)
  ordering <- colnames(tab)[colnames(tab) %in% DoseRef$Nom.du.produit]
  DoseRef <- DoseRef[order(DoseRef$Nom.du.produit, ordering), ]

  ##Calcul de l'IFT produit
  ###prise en compte des produits appliqués à plusieurs dates (somme dans tab)
  MDoseRef <- t(Nb_Trait_Rep)*DoseRef$DoseRef
  MDoseRef <- apply(MDoseRef, 1, funRatio)
  MDoseRef[MDoseRef=="Inf"] <- 0
  ####rapport doses apportées sur doses recommandées (à ajouter année)
  ## Ratio=t(tab)*(1/DoseRef$DoseRef)
  Ratio <- tab*MDoseRef
  ####prise en compte de la surface traitée
  R1 <- surf.trait/tab_info$Surface_ha
  Ratio <- Ratio * R1
  IFT <- rowSums(Ratio,na.rm=T)

  ###Fichier sortie
  tab_IFT <-
    data.frame(ID_Parc_Tri      = tab_info$ID_Parc_Tri,
               ID_Parcelle      = tab_info$ID_Parcelle,
               ID_Exploitation  = tab_info$ID_Exploitation,
               Surface_ha       = tab_info$Surface_ha,
               Culture          = rep(crop, length(IFT)),
               Protection_visée = rep(type, length(IFT)),
               NbProd           = as.vector(apply(tab, 1, funSum)),
               Dose             = dose,
               IFT              = IFT)
  
  return(tab_IFT)

}
