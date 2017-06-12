# ------------------------------------------------------------------------------
# projet  : Stage M2 - INRA de Dijon
# auteur  : Xavier Laviron <xavier.laviron@gmx.fr>
# date    : 01/02/2017 - 16/06/2017
# ------------------------------------------------------------------------------

# Description

Ce dossier contient toutes les données et analyses effectuées lors de mon stage
de master 2 à l'INRA de Dijon. Ce stage était encadré par Sabrina Gaba et Joël
Chadoeuf. Deux questions principales ont été abordées dans ce stage :
  - Estimation d'abondance totale de communautés adventices à partir de relevés
    semi-quantitatifs
  - Détection de patrons de co-occurence dans les communautés adventices par une
    méthode d'analyse multivariée probabiliste
    rapport (lecture seule) -> https://www.overleaf.com/read/tgspwgjtrrnt


# Architecture du projet

Le projet est divisé en 5 dossiers principaux. Cela permet de séparer plus
facilement les sous-projets. Les sous-projets peuvent donc être utilisés
indépendamment les uns des autres.
Les fichiers sont décrit brièvement ici. Pour plus d'informations sur ce qu'ils
font, suffisamment de commentaires ont du être ajoutés directement dans les
codes.

.
├── abundance/
│   ├── data -> ../data/
│   ├── estim_ab.R
│   ├── flora_summary.R
│   ├── functions/
│   │   ├── abundance.R
│   │   └── verif_ab.R
│   ├── graphs_report.R
│   ├── graphs_slides_03052017_zapvs.R
│   ├── graphs_slides_29052017_gestad.R
│   ├── reporting.pdf
│   ├── reporting.Rmd
│   └── verif_ab.R
├── data/
│   ├── generated/
│   └── raw -> /home/xavier/documents/data/stage2017/
├── gen_data/
│   ├── data -> ../data/
│   ├── functions/
│   │   ├── format_flora.R
│   │   └── gen_data_infos.R
│   ├── gen_data_abundance.R
│   ├── gen_data_infos.R
│   ├── gen_data_lda.R
│   └── util/
├── inquiries/
│   ├── data -> ../data/
│   ├── functions/
│   │   ├── clean_df.R
│   │   ├── db_convert.R
│   │   └── doseN.R
│   └── load_data.R
├── lda/
│   ├── data -> ../data/
│   ├── functions/
│   │   └── lda.R
│   ├── graphs_report.R
│   ├── lda_analyses.R
│   ├── lda_fitting.R
│   ├── reporting.pdf
│   ├── reporting.Rmd
│   └── slides.Rmd
└── README.txt

## Dossier 'data'

Répertoire central pour toutes les données. Les dossiers 'data' dans les autres
dossiers du projet sont des liens sumboliques pointant vers ce dossier.

### Données brutes
Le dossier 'data/raw/' contient tous les fichiers de données brutes utilisés
pour les analyses (lien symbolique vers un dossier de données centralisé).
Les fichiers nécessaires sont :

BDD_dec2016_culture.csv
BDD_robin_full.csv
BD_Fertilizers_dec2016.csv
dose-reference-ift-grande-culture.csv
Engrais_DB_Rui280317.csv
flore_tot_per1.csv
jauz_20170403_sp_with_traits.csv
manip_ble_2014_p1.csv
manip_ble_2014_p2.csv
manip_ble_2014_p3.csv
monitoring2006.csv
monitoring2007.csv
monitoring2008.csv
monitoring2009.csv
monitoring2010.csv
monitoring2011.csv
monitoring2013.csv
monitoring2014.csv
monitoring2015.csv
monitoring2016.csv
parcelles_2014_geom2.dbf
parcelles_2014_geom2.prj
parcelles_2014_geom2.shp
parcelles_2014_geom2.shx
usages_des_produits_autorises_v2_utf8-26012017.csv

### Données générées
Le dossier 'data/generated' contient tous les fichiers de données générés par
les scripts R (nettoyage ou mise en forme de données).

Les fichiers 'weeds*.csv' contiennent les matrices site x espèces année par
année.
Les fichiers 'transpose_abondance_per_*.csv' contiennent les matrices site x
espèces année par année transposés et avec des lignes vides pour les espèces non
présentes. Ce sont les fichiers utilisés en entrée d'estimation.

Les fichiers 'abond_per_plot_*.csv contiennent les abondances estimées par loi
binomiale négative année par année.

Les fichiers 'lda_all_years*.Rdata' sont les modèles LDA fittés sur toutes les
années groupées avec différents nombres de groupes. Ce sont les représentations
binaires des objets R obtenus avec 'save()', ils peuvent donc être chargés
directement dans un environnement R avec la commande 'load()'
Les fichiers 'lda_*_percent*.Rdata' contiennent les modèles LDA fittés sur
toutes les années groupées mais avec une présélection des données sur un certain
seuil d'occurence.
Les fichiers 'lda_year*.Rdata' contiennent les modèles LDA fittés année par
année.


## Dossier 'gen_data'

Tous les scripts permettant de générer des données transformées ou nettoyées
sont regroupés ici. Ils génèrent les données pour plusieurs sous-projets mais
c'était plus simple de les regrouper car plusieurs sous-porjets peuvent utiliser
les mêmes données générées. Il est important de lancer ces scripts afin d'avoir
les données nécessaires aux analyses présentes dans les autres sous-projets.

### 'gen_data_abundance.R'
Génère tous les fichiers nécessaires pour les estimations d'abondances. Il
nécessite les scripts présents dans le dossier 'util', qui sont les scripts
réalisés par les autres personnes ayant travaillé sur les données adventices. Je
n'ai pas apporté de modifications majeures à ces scripts, à part l'ajout de la
fonction 'transpose_df' (dans le fichier 'functions/format_flora.R') pour
générer les tableaux transposés nécessaires pour les estimations. Cette fonction
reprend ce qui était fait à l'origine dans ces scripts en corrigeant quelques
petites erreurs et en allant beaucoup plus vite.

### 'gen_data_infos.R'
Génère quelques fichiers pour récupérer les informations sur les parcelles
(cultures) et valeurs de traits fonctionnels des espèces adventices. Nécessite
les fonctions présentes dans 'functions/gen_data_infos.R'.

### 'gen_data_lda.R'
Génère les tableaux de données nécessaires aux analyses LDA (seulement le
tableau avec toutes les années groupées).


## Dossier 'abundance'

Contient tous les scripts relatifs aux estimations d'abondance des communautés
adventices.

### 'verif_ab.R'
Analyses des vérifications d'abondances sur les données de comptage exactes de
2013. Il nécessite les fonctions dans le fichier 'functions/verif_ab.R' ainsi
que 'functions/abundance.R'.

### 'flora_summary.R'
Petites analyses descriptives des données de flore, fichier finalement non
utilisé.

### 'estim_ab.R'
Conient les codes pour effectuer les estimations sur toutes les années de
relevés. Il nécessite les fonctions présentes dans le fichier
'functions/abundance.R', fonctions principales pour toutes les estimations
d'abondance suivant les différentes méthodes.

### 'graph*.R'
Ces fichiers générent les graphiques et tableaux utilisées dans les
présentations et le rapport final.

### 'reporting.Rmd'
Fichier Rmarkdown pour générer un rapport en pdf sur l'avancée du travail.


## Dossier 'lda'

Les analyses des modèles nécessitent d'avoir au préalable lancé les scripts
d'estimation des abondances pour avoir accès aux matrices sites x espèces
nécessaires.

### 'lda_fitting.R'
Code pour fitter les différents modèles LDA. Nécessite les fonctions présentes
dans 'functions/lda.R'

### 'lda_analyses.R'
Code pour analyser les résultats des modèles LDA. Nécessite les fonctions
présntes dans 'functions/lda.R'

### 'reporting.Rmd' 'slides.Rmd'
Fichier Rmarkdown pour générer un rapport en pdf sur l'avancée du travail. Il
faut d'abord lancer le script 'lda_analyses.R' afin d'avoir accès à
l'environnement de travail contenant les analyses des modèles.
Le fichier 'slides.Rmd' est pareil mais génère des slides (en html).

### 'graphs_report.R'
Génère les graphiques et les tableaux nécessaires pour le rapport final.


## Dossier 'inquiries'

Ce contient les débuts d'analyses portant sur les enquêtes de la ZAPVS. Ce sont
surtout les scripts permettant de nettoyer et convertir les anciennes bases de
données. Ils ne devraient donc plus être utiles.
