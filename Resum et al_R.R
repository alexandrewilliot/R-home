
# Collapse All - Alt+O
# Expand All - Shift+Alt+O

# R function work with matrix (can contain only numbers) /as.matrix ! not df

#In chunk 1, I can write run_6<-FALSE ; then in chunk 6 {r, eval=select_P_inter, include=FALSE} to select/run a chunk or not
# to clear object in my global environnement : rm(list=ls())

# integer (0) : pense à vérifier 'orthographe'

##### update R ####
install.packages("installr")
require(installr)
updateR()

# possible que package s'update et donc besoin d'updater R aussi (R, Rstudio, packages)

#### info packages / chemin ##

.libPaths() # faire dans console # trouve chemin vers dossier des packages
library() # dans console et ouvre un fichier répertoriant les packages installés
search() # dans console # Get all packages currently loaded in the R environment 

#library("package Name", lib.loc = "path to library")

#devtools::install_github("r-lib/devtools")

# d?sinstaler un package
#withr::with_libpaths(data.table)
#uninstall(pkg = "devtools")
remove.packages("XXX") # désintaller un package probl?matique avant mise à jour
remove.packages("blogdown")


# pour mettre à jour juste reinstall
#version package # pour mettre à jour juste reinstall
packageVersion('reshape2')
packageVersion('reshape')


# (re)installs the latest compatible version of Rtools
install.Rtools(check = F)
install.Rtools(check = TRUE, check_r_update = TRUE, GUI = TRUE, ...)

#-------------------------------------Resum et al_R----------------------------------------------
# possible de consulter les scripts de Field
#site avec donn?es et script 
#https://studysites.sagepub.com/dsur/study/default.htm


#? faire:
#  - lecture ''Brief Introduction to dplyr'' PAGE 29 de Irizarry
#  - chapitre ANOVA mixte Field
#2 - tester filtre RT
#3 - tester donner avec projet ideas; cr?ation de colonne comme mes besoins



#----------------------------------------chapitre 3-----------------------------------------
#Field

getwd() # v?rifier le working directory 
setwd("C:/Users/utilisateur/Desktop/R") # pour rediriger le working directory
# setwd("C:/Users/utilisateur/Documents") # utiliser pour PC maison car packages semblent ?tre dedans

#object <-c créé à partir d'une fonction / c pour grouper


filename <- "C:/Users/utilisateur/Desktop/R/data.xlsx" #simplement changer le nom de fichier ici


library(readxl) #lire Excel/ouverture du chemin/capable de trouver feuille excel avec sheet =""
data <- read_excel(file.choose(filename), sheet="BRUTES (5 blocs)")
#pour un chargement direct sans menu
data <- read_excel((filename), sheet="BRUTES (5 blocs)")

View(data)

filename_SPSS <- "C:/Users/utilisateur/Dropbox/2020/1 - Postdoc U Laval/CSSS/Analyses/Procedings A.M_1/CSSS_Donn?es_procedings_articleA.M_AW.sav"

library(haven) #lire données SPSS
CSSS_Données_procedings_articleA_M_AW <- read_sav(filename_SPSS)
View(CSSS_Données_procedings_articleA_M_AW)


#install.packages("Rcmdr", dependencies = TRUE) # dependencies est une fonction qui appelle tous les besoins du package ? charger
library(Rcmdr)

#install.packages("tcltk", dependencies = TRUE) # non dispo pour V4.0.0
#install.packages("Tktable", dependencies = TRUE)



#------------------------------organisation des données--------------exemple------------
#id?alement faire cr?ation des variables, puis dans dataframe

names <-c ("Jean", "Jacques", "Albert", "Pat") #création de variable string
age<-c(22, 33, 34, 54)

perso<-data.frame(nom=names, age) # donne un tableau
perso # voir le tableau
perso$nom # sélectionner une variable existante au sein d'un jeu de donn?es
TR<-c(1200, 1300, 300, 400) ; perso$TR<-c(1200, 1300, 300, 400) #ajout d'une variable au sein du jeu de donn?es
names(perso) # donne le nom des variables du jeu de donn?es | fonction names
# perso<-list(names, age) # obtenir les variables en liste/ligne
# perso<-cbind(names, age) # permet de traiter toute les donn?es en texte

#------------------------------quelques opérateurs

#==   exactement égale
#!=   pas égale
#!x   pas x
#x|y  x ou y
#x&y  x et y
#is True(x)   test if x is true

#----------------calculer de nouvelles variables a partir d'existante------
#page83 Field

child_age<-c(10,11,12,13) ; perso$child_age<-c(10,11,12,13) # création variable #dans matrice perso

perso$diffage<-age-child_age

perso$diffage

#date_naissance   <-as.Date(c("

perso$date_naissance<-as.Date(c("1983-06-21", "1925-06-21","1966-06-21","1986-06-21"))
perso$child_date_naissance<-as.Date(c("1993-06-21", "1995-06-21","1996-06-21","1996-06-21"))
perso$diffdate<-perso$date_naissance-perso$child_date_naissance
perso$diffdate


#définir la valeur 1 ou 2 pour des valeurs d'une autre colonne
DataThese_1Ideo <- DataThese_1Ideo %>% mutate(CB = case_when(Id %in% c(1,2,5,6,10,14,17,18,21,25,30,33,34,37,42,50,53,57,58,62,66,74,85) ~ 1, Id %in% c(3,7,8,12,15,16,19,20,23,24,35,36,39,40,43,44,48,51,52,56,60,64,67,72,75,79,83,84,87) ~ 2, TRUE ~ NA_real_))


#avoir colonne avec valeur selon valeur seuil
library("dplyr")

DataThese_1Ideo <- DataThese_1Ideo %>% 
  mutate(Ideo_dist_2 = (Ideo_dist > 1.8)*1) # au dessus de 1.8 = 1 sinon 0

# somme de variables
data_1$P.01_BISS2_sum <- rowSums(data_1[ , c("P.01_BISS2_1", "P.01_BISS2_2")]) # Sum of multiple columns



# ----- boucle for --------

#tuto: https://statisticsglobe.com/for-loop-in-r

# changement de valeur pour identifier facilement les 5 blocs
# cr?er une colonne selon 2 autres (participant dans session)
# boucle for

for(p in unique(data_1$participant)) {                         
  pSessions <- unique(data_1$session[data_1$participant==p])
  #cat(p, pSessions,"\n")
  data_1$Blocs5[data_1$participant==p] <- match(data_1$session[data_1$participant==p], pSessions)
}; rm(p, pSessions)



#--------------------création de facteur | variable num en nominal; ex: var groupe----
#Field page 89


job<-c(1,1,2,2)  #création de sa variable indep
perso$job<-c(1,1,2,2) #création de sa variable dans matrice perso
perso$job

group<-c(1,1,1,2)  ; perso$group<-c(1,1,1,2) #créer sa variable #puis ajouter dans matrice

perso$vol<-c (rep(3,4)) # ajout nouvelle variable vol avec valeur 3 répété 4 fois dans matrice perso

# attention a var seul et var dans matrice à quelle diff
group <-factor(group, levels = c(1,2), labels = c("control","exp")) 
perso$group <-factor(group, levels = c(1,2), labels = c("control","exper"))


#----------transformation d'une variable en variable de codage (nominal) = faire des groupes

job<-factor(job, levels = c(1:2), labels = c("?tudiants", "travailleur"))



#---------- tr variable num en facteur--

#exemple hors fichier
data_test$group<-as.factor(data_test$group)
#possible d'utiliser as.numeric

#----------fonction gl() pour création de facteur général # nb groupe, nb cas par groupe, labels----

perso$Var_groupe<-gl(4,1, labels = c("groupeA","groupeB","groupeC","groupeD"))
#connaitre les niveaux de variable
levels(perso$Var_groupe)
#renommer de façon automatique
levels(perso$Var_groupe)<-c("medical étu", "medical trav")

#changer l'ordre des groupes
Var_groupe<-c(1,2,3,3)
Var_groupe<-gl(4,1, labels = c("groupeA","groupeB","groupeC","groupeD"))
Var_groupe

Var_groupe<-factor(Var_groupe, levels = levels(Var_groupe)[c(4,1,2,3)])
Var_groupe


#Créer variable numérique     #Field page 91

perso$friends<-c(9,8,7,6)
friends<-c(9,8,7,6)
data_frame2<-data.frame(names, age, friends)

#valeures manquantes
mean(perso$age, na.rm = TRUE)
#na.rm = ignorer valeur manquante avant le calcul de la moyenne


#Sauvergarder les donn?es #cheker getwd()
getwd()
write.csv2(perso, "test_perso_R.csv")


#sélectionner des variables spécifiquement #aucune ligne spécifié alors toutes
newDataframe<-perso [,c("age", "group", "friends" )]

#sélectionner des lignes en lien à un groupe
étu<-perso [Var_groupe=="groupeD",]
étu

# créer des groupes à partir de valeur d'une autre colonne

# création de groupe Scantracker (1) et controle (0)
data_c$Group <- even(data_c$participant_id) # True pour nb paire
data_c$Group<-as.character(data_c$Group)
data_c$Group[data_c$Group == 'TRUE'] <- '1'
data_c$Group[data_c$Group == 'FALSE'] <- '0'
data_c$Group<-as.factor(data_c$Group)


# remplissage d'une colonne avec valeur 1
data_1$Blocs8<-seq(1,1,1) 
data_SPSS$Blocs8<-seq(1,20,1) # from 1 to 1 by 1 


# Sélectionner les données avec la fonction subset() ----

#sélection de colonne pour raccourcir database
data_percent_2<-subset(data_percent, select = c(alert,time_to_respond_ms, detection, Blocs5, Group, participant.x, session))

#Field page 105

selection <- subset(perso, Var_groupe=="groupeA")
selection

selection_2<- subset(perso, age>25)
selection_2
selection_3<- subset(perso, age>25,select = c("nom", "job", "TR"))
selection_3

#sélectionner plusieurs rangées de ligne avec toutes les colonnes # ligne hors test (pas rapport au fichier)
select<-data_2[c(1:131, 132:145), ]

#sélectionner par position colonne
selection <- subset(data_6, select = c(81:107))


#exclusion de données (lignes):
data <- data[!data$nomcolonne == 2001, ]


library(dplyr)
data <- data %>% filter(participantequipe != 2009)

data<-data[-c(2,3,4),] # si ordre change alors s?lection aussi ...
myData = myData[myData$colonne > 4,] # exclusion quand valeur inf ? 4 dans colonne
myData <- subset(myData, id!= 6)


#utilisation de matrice # car parfois des fonctions roulent uniquement avec une matrice

newmatrix<- as.matrix(newDataframe)
newmatrix_2<- as.matrix(newDataframe [age>33, c("friends", "age")])


# afficher des colonnes ou lignes de dataframe ####

#colonnes
data[c('temps', 'feminisme_temps')]

#lignes
data_5[5, ]

#ligne spécifique
subset (data_5, Duration__in_seconds_ == 501 )

###############################fichier de données Honeymoon Period.dat

# Identifier position de colonnes #####
which(colnames(data_2)=="incident_description") #15
which(colnames(data_2)=="id_reports") #23
which(colnames(data_2)=="detection_based_on_column") #24
which(colnames(data_2)=="Group") #3

#nb de fois qu'une donnée 8211 est présente dans colonne
which(data_2$participant.x == 8211)

length (data_2$participant.x)

nrow(incident) #connaitre nb ligne/row

#rename nom col session_id en session
#library (tidyverse)
names(data_E)[names(data_E) == "session_id"] <- "session"

#rename facteur dans colonne

library(plyr)
revalue(x, c("beta"="two", "gamma"="three"))
data_1_long$Blocs5 <-revalue(data_1_long$Blocs5, c("Inspection_Lag_B1"="1", "Inspection_Lag_B2"="2","Inspection_Lag_B3"="3", "Inspection_Lag_B4"="4","Inspection_Lag_B5"="5"))

# nb col

ncol(data_1)


# Remplacer une valeur dans une colonne ####

###FA
#identifier les 0
which (data_M$FA ==0) ; summary(data_M$FA)
#remplacer par 0.001
#install.packages("anchors")
library(anchors)
data_M<- replace.value(data_M,c("FA"),0,0.001)
#vérif
which (data_M$FA ==0.001)

#trouver NA - find
which(is.na(data_10$VD))

#trouver position colonne # donne toutes les colonnes du même nom
grep("CollBelow", colnames(DataThese_1Ideo)) 



# Removing Rows with Some NAs Using na.omit() Function | Supprimer les lignes contenant les NA 

data1 <- na.omit(data) # Supprime toutes les lignes avec des NA
data3 <- data[rowSums(is.na(data)) == 0, ] # this R code can easily be modified to retain rows with a certain amount of NAs. 
#For instance, if you want to remove all rows with 2 or more missing values, you can replace “== 0” by “>= 2”.

data_2<-data_1[!is.na(data_1$display_time_percent), ] # supprime lignes avec des NA dans une colonne

NaN
#NaN (“Not a Number”) means 0/0

# Ajouter une ligne/row dans df ####


#library (tydiverse)
data <- df %>% add_row(tibble_row(x = 4, y = 0))
data_1 <- data_1 %>% add_row(Group=1, id_rapport = 999, Blocs5 = 5,participant_id = 8220, 
                             participant_id_2 = 8220, .after= 210) #after/before correspond au num de ligne du df


# Réorganiser les données----
#page 107 Field

#-----------aller chercher le fichier
setwd("C:/Users/utilisateur/Desktop/R/R Data Files Field")
satisfaction_data = read.delim("Honeymoon Period.dat", header=TRUE)
View(satisfaction_data)
str(satisfaction_data)

#page 107 de Field 
#wide format: unstack / cast ; long format: stack / melt

#stack produces a data frame with two columns: value $ ind

#avoir tout en num?rique avant changement
#restructuration des donn?es #passage de 4 colonnes ? 1
#select = c() is optional, but is a way to select a subset of variables that you want to stack. So, for the current data, we want to stack only the life satisfaction scores
restructured_data <-stack(satisfaction_data, select = c ("Person","Satisfaction_Base","Satisfaction_6_Months", "Satisfaction_12_Months", 
                                                         "Satisfaction_18_Months"))
restructured_data

#newDataFrame<-unstack(oldDataFrame, scores ~ columns)
unrestructured_data <- unstack(restructured_data, values~ind) # retour à 4 colonnes

#melt & cast
#install.packages("reshape", dependencies = TRUE) # pour restructurer des données   | page 111 Field
install.packages("reshape")
library(reshape)
install.packages("reshape2")
library(reshape2)
# fonction melt() pour le format long et cast() pour autre format 


data_1_long <- melt (data_1, id.vars = c("participant", "Group"), measure.vars = c ("un","deux","trois", "quatre", "cinq"))

# restructured_data_2 <- melt (satisfaction_data, id = c("Person", "Gender"), measured = c ("Satisfaction_Base","Satisfaction_6_Months",
#                                                                 "Satisfaction_12_Months", "Satisfaction_18_Months"))
restructured_data_2
#newData<-cast(moltenData, variables coded within a single column ~ variables coded across many columns, value = "outcome variable")

cast_data<-cast (restructured_data_2, Person + Gender ~ variable, value = "value" )

newdata<-cast(moltendata, variable coded within single column ~ variables coded across many column, value = "outcome variable")

newdata<-cast(data_4, participant ~ size_incident, value = "time_to_respond_ms")
newdata<-cast(data_4, participant ~ VI, value = "VD")

# aggréger/TCD excel # avoir nb participant dans tableau a
a<-aggregate( Blocs5 ~ participant.x, data_5, length) #tableau avec nb de participants et nb bloc
a

# possible d'aggréger des données 
# se fait bien pour données en long
# faire une aggregation des données par num_équipe # exemple hors de son contexte
library (tidyverse)
data_z2<-aggregate(data_z1,
                   by=list(data_z1$num_?quipe),
                   FUN=mean)
table(data_z2$Group)
str(data_z2)


#moyenne par groupe avec dplyr mais possible aussi avec aggregate

library(dplyr)  
data_all %>%                                        # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(accuracy),              # Specify column
               list(name = mean))               # Specify function

aggregate(x = data_all$accuracy,                # Specify data column
          by = list(data_all$Group),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

aggregate(accuracy ~ Group, data_all, mean) #nb enfant moyen selon prof

boxplot(accuracy ~ Group, data_all)

# transposer les données

Data_ideoClean <- as.data.frame(t(Data_ideo[,-1]))



#Field

# score Z ####

#score Z
DataThese_1Ideo$Z_perf <- scale(DataThese_1Ideo$Perform)  


# traitement/nettoyage/cleaning data ####

# https://www.youtube.com/watch?v=mC61jNlH5R0


# traitement en lien ? du texte
# https://regexr.com/

#code Eudes
# B\\w+ ?a veut dire prend moi tout les mots qui commence par B
pattern_nvx_etudes_bac <- "B\\w+.+|b\\w+.+|.b\\w+.++|.B\\w+.+|.+.B\\w+|.+.b\\w+|DEUG|Pas encore de dipl?me"
R_T1$AUTRE.DIPLOME_rec <- str_replace_all(R_T1$AUTRE.DIPLOME_rec,
                                          pattern= pattern_nvx_etudes_bac,
                                          replace = "Bac")


# aide ? organiser les donn?es textes
#https://stringr.tidyverse.org/reference/str_split.html




# remove/supress variable/column with specific letter ####

library(tidyverse) 
data_all <- data_all %>% select(-contains("X1."))

# merge de données l'une sur l'autre ; besoin de noms de colonnes similaires
data <- rbind(data_2, data_2b)

# merge de données l'une à côté de l'autre 
data <- cbind(data_2, data_2b)

# Fusion de données et sauvegarde Excel ####
data_merge <- merge(data_1, data_session, by=c("session"))


data_merge_1 <- merge(data_long_fem, data_long_Image_C, by=c("participant", "P_Age", "P_sexe", "temps"))
data_merge_2 <- merge(data_long_Prejuges, data_long_comp_alim_1, by=c("participant", "P_Age", "P_sexe", "temps"))
data_merge_3 <- merge(data_merge_1, data_long_comp_alim_2, by=c("participant", "P_Age", "P_sexe", "temps"))
data_merge_4 <- merge(data_merge_2, data_merge_3, by=c("participant", "P_Age", "P_sexe", "temps"))



# merge multiple dataframe
data_list <- list(data_long_fem, data_long_Image_C, data_long_Prejuges, data_long_comp_alim_1,data_long_comp_alim_2)  

data_list %＞% reduce(inner_join, by = c("participant", "P_Age", "P_sexe", "temps"))          # Apply reduce function of tidyverse



# Merge of all csv in one folder !!
library("dplyr")                                                  # Load dplyr package
library("plyr")                                                   # Load plyr package
library("readr")                                                  # Load readr package

setwd("C:/Users/utilisateur/Desktop/R/CSSS IDEAS/Milestone 3/merge_test")

all_files <- list.files("C:/Users/utilisateur/Desktop/R/CSSS IDEAS/Milestone 3/merge_test", pattern = "*.csv", full.names = TRUE)
data_all <- read_csv2(all_files[1])
for(i in 2:length(all_files)) {
  data_i <- read_csv2(all_files[i])
  data_all <- merge(data_all, data_i, by = c("participant","Group"))
}

write.csv2(data_all, "merge statistics_globe.csv")



#sauvegarde Excel
data_a<-data[,-c(14)] # supression d'une colonne

data_raw<-subset(data_1, select = - c(participant, session))  # supression col par nom

getwd()
write.csv2(data_a, "donn?es finales Team QUASA.csv")

#install.packages("xlsx")
#library(xlsx)
#install.packages("openxlsx") 
library(openxlsx)
write.xlsx(data_merge, 'data_merge_à_vérif.xlsx', asTable = FALSE)


# Run all Rmd ####
#knit all
setwd("C:/Users/utilisateur/Desktop/R/CSSS IDEAS/Milestone 3/Rmd mars 2021/test t")
getwd()

files <- list.files(pattern = "[.]Rmd$")

for (f in files) rmarkdown::render(f)

# run 1 Rmd
#rmarkdown::render("IDEAS M3 NASA Eval_Select.Rmd", html_document())

rmarkdown::render("IDEAS M3 NASA Eval_Select.Rmd", 
                  output_file = "IDEAS M3 NASA Eval_Select.html")

#autoriser plusieurs mêmes chunk 
options(knitr.duplicate.label = "allow")

# stop the script between 2 codes inside a chunk ####
#will produce an error

{
  if (TRUE) {stop("The value is TRUE, so the script must end here")}
  
  print("Script did NOT end!")
}


#-------------------------------------fin chapitre 3-----------------------------------------

#################################################################################################
#################################################################################################

#-------------------------------------livre de Irizarry (2017)----------------------
# quelques fonctions

install.packages("devtools", dependencies = TRUE)
library(devtools)
devtools :: install("your package/url")



#Brief Introduction to dplyr
library(dplyr) # if dplyr receives a data.frame it will return a data.frame.


#lapply applique une fonction sur tous les ?l?ments d'un vecteur ou d'une liste et retourne le r?sultat sous forme de liste.
#sapply retourne le r?sultat sous forme de vecteur simple ou de matrice, lorsque c'est possible.
#install.packages("lapply") # pas v 4
#installed.packages("sapply") # pas v 4
library(lapply)
library(sapply)




# tableau data - moy par variable####


#r?cap complet
tableau<-table(data_4$participant.x,data_4$alert)
print(tableau)
addmargins(tableau)

# La fonction prop.table donne les pourcentages (globaux, en ligne ou en colonne).
# 
# #Pourcentage total
# prop.table(tableau)
# 
# #Pourcentage en ligne
# prop.table(tableau,1)
# 
# #Pourcentage en colonne
# prop.table(tableau,2) 


#moy par groupe
library(dplyr)
a <- data_all %>% 
  group_by(Group) %>% 
  summarise(perceived_acc = mean(perceived_acc),
            accuracy  = mean(accuracy))
a

b<-data_all %>%                                        # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(perceived_acc),              # Specify column
               list(name = mean))               # Specify function

#moyenne avec plusieurs variables

library(reshape2)
dcast(data, Blocs5 ~ Group, value.var = "VD", fun.aggregate = mean)


#-------------------------------------Graphe Chapitre 4-------------------------------------------------
#Field


#install.packages("ggplot2", dependencies = TRUE) 
library (ggplot2)


filename_graphe <- "C:/Users/utilisateur/Desktop/R/d?penses pub.sav"

library(haven) #lire donn?es SPSS
prep_graphique <- read_sav(filename_graphe) 

prep_graphique$gender<-gl(2,100, labels = c("femme","homme")) #cr?ation colonne genre

mygraph <- ggplot (prep_graphique, aes(Sales, Adverts, colour = gender)) #aes contr?le apparence
# pas possible de voir le graphique ? cet ?tape

mygraph+geom_point() #possible de voir le graphique ici

#box-plot
mygraph+geom_boxplot()

#sauvegarde du graphe
ggsave("mygraph.png") #,width = 2, height = 2)

#--------------------------------histogramme----
#page 152 Field

setwd("C:/Users/utilisateur/Desktop/R/R Data Files Field")
Chik_data = read.delim("ChickFlick.dat", header=TRUE)
View(Chik_data)

library (ggplot2)

bar<-ggplot(Chik_data, aes(film, arousal, fill=gender)) #aes (titre x, titre y, s?parer selon VI)
bar+geom_point()
bar+
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.95), width = 0.2) + 
  labs(x = "film", y = "mean arousal", fill = "gender") + 
  scale_fill_manual("Gender", values = c ("Female" = "White","Male" = "Black"))
# dodge signifie d'avoir le genre cote à cote + ajout barres d'erreur + labels


#--------------------------------graphique en ligne avec IC---- 
#voir page 158 Field
#non termin?, la s?paration des groupes se fait pas bien
line<-ggplot(Chik_data, aes(film, arousal, fill=gender)) #aes (titre x, titre y, s?parer selon VI)
line+
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group="Female"), colour = "Blue", linetype = "dashed") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "film", y = "mean arousal", fill = "gender") 


#Field
#------------------------------------- Graphique esquisse -----------------------------------------
# https://github.com/dreamRs/esquisse
install.packages("esquisse")
library(esquisse)
# Launch addin via RStudio menu or with:
esquisse::esquisser()


library(dplyr)
library(ggplot2)

ggplot(data_all) +
  aes(x = Group, y = accuracy) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()




# --- visualize data ####

> dataframe %>%
  + select(var1, var2, var3, everything()) %>%
  + view()
#Including the everything() will simply reorder your columns, so if you only want to visualize 
#specific columns, drop the everything() and only list what you want to visualize.

# Reorder Columns of Data Frame by Index - changer position
data_1<-data_1[ , c(1:12,83, 13:82 )]


#reorder/sort value in column
incident3<-incident2[order(incident2$detection), ]

#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),]


# ---- Graph -------------------------

#voir jitter pour super graphe : https://statisticsglobe.com/jitter-r-function-example/


#https://www.data-to-viz.com/
#aide ? trouver le code selon le graphe que tu veux avec arbre d?cisionnel
#https://www.r-graph-gallery.com/violin
#graphe en violon ...

#ligne avec moyenne des conditions
ggplot(data) +
  aes(x = Blocs5, y= VD, fill = VD, group = Group,color = Group) +
  stat_summary(fun=mean, geom="line", position = "dodge") 



#nuage de points des moyennes des variables
ggplot(data_all) +
  aes(x = mean(perceived_acc), y = mean(accuracy), group = Group,color = Group) +
  geom_point(size = 2.42, colour = "#0c4c8a") +
  theme_minimal()

#nuage de points avec diff couleur par Groupe
ggplot(data_all) +
  aes(x = perceived_acc, y = accuracy, color = Group) +
  geom_point(size = 2.42) +
  theme_minimal()

#histo 
ggplot(data) +
  aes(x = alert, y= VD, fill = VD) +
  stat_summary(fun=mean, geom="bar", position = "dodge") +
  scale_fill_gradient() +
  theme_minimal()

#boxplot
ggplot(data) +
  aes(x = alert, y = VD, group = alert) +
  geom_boxplot(size = 1L, colour = "black") +
  theme_minimal()

#violon
ggplot(data) +
  aes(x = alert, y = VD, group = alert) +
  geom_violin(adjust = 1L, scale = "area", fill = "#0c4c8a") +
  theme_minimal()

#violon + boxplot - color darkred
ggplot(data, aes(x=alert, y=VD, group = alert, fill=alert)) + 
  geom_violin(trim=FALSE, fill='grey', color="grey")+
  geom_boxplot(width=0.1, fill='white', color="black")+
  theme_classic()

#violon + moyenne and SD
#The function mean_sdl is used. mean_sdl computes the mean plus or minus a constant times the standard deviation. In the R code below, the constant is specified using the argument mult (mult = 1). By default mult = 2. The mean +/- SD can be added as a crossbar or a pointrange :

ggplot(data, aes(x=alert, y=VD, group = alert, fill=alert)) + 
  geom_violin(trim=FALSE, fill='grey', color="grey")+
  theme_classic() + stat_summary(fun.data = "mean_sdl")

#violon + boxplot + moyenne
ggplot(data, aes(x=alert, y=VD, group = alert, fill=alert)) + 
  geom_violin(trim=FALSE, fill='grey', color="grey")+
  geom_boxplot(width=0.1, fill='white', color="black")+
  theme_classic() + stat_summary(fun.data = "mean_sdl",mult=2,color="black")


#2 graphes side by side
#voir aussi patchwork : https://fahimahmad.netlify.app/posts/combining-multiple-plots-using-patchwork/

#install.packages("cowplot")
library(cowplot)

plot_grid(plot1, plot2, labels = "AUTO")



# nuage de points avec histogramme sur les côtés
# semble non fonctionnel 
#install.packages("ggExtra")
library(ggExtra)
library(ggplot2)
#idéalement avec x et y en continu
ggp <- ggplot(data_PR, aes(variable, value)) + geom_point()
ggp 

ggMarginal(ggp, type = "densigram")





# ----- barre d'erreur | Denis Cousineau ####
# https://dcousin3.github.io/superb/



# ------- Taille d'effet ####

library(rstatix) # Calcul d de Cohen
# d= |moy G1- moy G2|/?cart-type mis en commun des deux groupes
# 0,2 (petit effet), 0,5 (effet mod?r?) et 0,8 (grand effet) (Cohen 1998

#pour test t
df %>% cohens_d(VD ~ VI, var.equal = TRUE)
# option var.equal = TRUE, alors le SD group? est utilis? pour calculer le d 

#pour test t appari? # d = moyenne de la diff / Sd de la diff?rence
df %>% cohens_d(len ~ supp, paired = TRUE)



#install.packages("MOTE")
library (MOTE)
# peut obtenir omega et taille d'effet d

# d for Between Subjects with Pooled SD Denominator
d.ind.t(m1, m2, sd1, sd2, n1, n2, a = 0.05)

# d from t for Between Subjects # semble similaire ? df %>% cohens_d(VD ~ VI, var.equal = TRUE)
d.ind.t.t(t, n1, n2, a = 0.05)






# --------------------Analyse de puissance/power ####

#https://www.statmethods.net/stats/power.html

#install.packages("pwr")
library(pwr)

#laisser 1 null pour l'avoir en fonction des autres
pwr.t.test(n = NULL, d = NULL, sig.level = 0.05, power = NULL, 
           type = c("two.sample", "one.sample", "paired"),
           alternative = c("two.sided", "less", "greater"))

# groupe n ?gaux
pwr.t.test(n = , d = , sig.level = , power = , type = c("two.sample", "one.sample", "paired"))

#groupe n in?gaux
pwr.t2n.test(n1 = , n2= , d = , sig.level =, power = )

#anova
pwr.anova.test



# ---------------- Correlation -------------------------------------------


## corrélation (bien à utiliser)
round(cor(data_corr, method = "pearson"),2 )
round(cor(data_corr, method = "spearman"),2 )



#Field - page 216

install.packages("Hmisc")
library(Hmisc) # pour rcorr


#tr en num pour rcorr
data<-data.matrix(data, rownames.force = NA)

#library(Hmisc)
rcorr(data, type=c("pearson"))
rcorr(data, type=c("spearman"))

rcorr(as.matrix(data[, c ("mean_pupil_diameter_before_incident","detection", "time_to_respond_ms")]),type=c("pearson"))
rcorr(as.matrix(data[, c ("mean_pupil_diameter_before_incident","detection", "time_to_respond_ms")]),type=c("spearman"))


#rester en dataframe # juste pour 2 corrélations
cor.test(data$detection, data$mean_pupil_diameter_before_incident, method = "pearson")
cor.test(data$detection, data$mean_pupil_diameter_before_incident, method = "spearman")
cor.test(data$detection, data$mean_pupil_diameter_before_incident, method = "kendall")

# matrice sans valeur p
cor(data, use = "complete.obs", method = "pearson")
cor(data, use = "complete.obs", method = "spearman")

#---

## Graph of pupil dilatation & accuracy | pupil dilatation & RT
a<-ggplot(data) +
  aes(x = mean_pupil_diameter_before_incident, y = detection) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 1L) +
  theme_minimal()
b<-ggplot(data) +
  aes(x = mean_pupil_diameter_before_incident, y = time_to_respond_ms) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 1L) +
  theme_minimal()

library(cowplot)
plot_grid(a, b, labels = "AUTO")


## Pearson correlation between pupil dilatation - accuracy - RT
rcorr(as.matrix(data[, c ("mean_pupil_diameter_before_incident","detection", "time_to_respond_ms")]),
      type=c("pearson"))
## Spearman correlation between pupil dilatation - accuracy - RT
rcorr(as.matrix(data[, c ("mean_pupil_diameter_before_incident","detection", "time_to_respond_ms")]),
      type=c("spearman"))

# p227 field
## Bootsrapping correlations - pearson pupil & accuracy
boot_cor<- function(data,i)cor(data$mean_pupil_diameter_before_incident[i], data$detection[i], use = "complete.obs", method="pearson")

library(boot)
boot_pearson<-boot(data, boot_cor, 2000)
boot_pearson

boot.ci(boot_pearson)





#### Regression ####
#Field - page 253

#install.packages("QuantPsyc")
library(boot) # bootstrapping
library(car) # regression diag
library(QuantPsyc) # coeff standard


#voir aussi pour préd en continu
regression <- lm(time_to_respond_ms ~ duration + size_incident + categ_split + type_incident + spatial_displacement, data = data_1) 

#anova(regression) # donne F mais pas nécessaire pour regression standard

summary(regression)
#summary.lm(regression)

# to obtain Beta values
lm.beta(regression)
# to obtain CI
confint(regression)


## possible de comparer 2 modéles de régression # le 2e model doit être le même que le premier + autres variables
anova(model_1, model_2,...)


## outliers et cas influents # mettre dans df pour analyse ensuite
data_1$residu <- resid(regression) # outlier

data_1$residu_std <- rstandard(regression) # outlier
data_1$studendized_r <- rstudent(regression) 
data_1$cook <- cooks.distance(regression) # cas influent # valeur doit être inf à 1
data_1$dfbeta <- dfbeta(regression) # cas influent
data_1$dffit <- dffits(regression) # cas influent
data_1$leverage <- hatvalues(regression) # cas influent # nb de prédi+1 div par n et vérif que pas de valeur de plus de 2 fois la valeur calculé
data_1$cov <- covratio(regression) # cas influent 

#Mahalanobis # valeur seuil selon nb de prédicteur à partir table de Chi-2
data_1$Maha <- mahalanobis(df,colMeans(df),cov(df),decreasing=TRUE)

# standardized_DFBETA : si sup à 1 alors valeur influence le modéle = les coeff

# DFFIT : si 0 alors pas d'influence sur modéle




# identification des cas > 2 | TRUE=1 et FALSE =0 donc somme possible
# pas plus de 5% au dessus de 2
data_1$large_resid_std <- data_1$residu_std > 2 | data_1$residu_std < -2 
# nb de cas sup à 2
sum (data_1$large_resid_std)
# identification des cas
select <- subset(data_1, data_1$large_resid_std=="TRUE")
select_2 <- subset(data_1, data_1$cook > 1)


# indépendance des erreurs # correcte entre 1 et 3
# p value calculé par bootstrapp donc peut changer
durbinWatsonTest(regression)

# Multicolinéarité
vif(regression) # plus grand VIF inf à 10 alors ok
1/vif(regression) # si inf à 0.2 alors pb possible
mean (vif(regression)) # si plus grand que 1 alors probléme

# Graphe résidu
data_1$fitted <- regression$fitted.values # valeur prédite
hist(data_1$studendized_r)
scatter <- ggplot(data_1, aes(fitted, studendized_r))
scatter + geom_point()+geom_smooth(method = "lm", colour ="Blue") + labs(x="fitted values", y ="studentized resid")


#bootstrapping

bootReg <- function (formula, data, indices)
{
  d <- data [indices,]
  fit <- lm (formula, data = d)
  return (coef(fit))
}

Boot_res <- boot(statistic = bootReg , formula = time_to_respond_ms ~ duration + size_incident + categ_split + type_incident + spatial_displacement, data = data_1, R = 2000)

boot.ci(Boot_res, type = "bca", index = 1) # CI pour intercept
boot.ci(Boot_res, type = "bca", index = 2) # CI pour var 1
boot.ci(Boot_res, type = "bca", index = 3) # CI pour var 2
boot.ci(Boot_res, type = "bca", index = 4) # CI pour var 3
boot.ci(Boot_res, type = "bca", index = 5) # CI pour var 4
boot.ci(Boot_res, type = "bca", index = 6) # CI pour var 5



## dummy variable

library(fastDummies)
library(recipes)

#To create a dummy variable in R you can use the ifelse() method:
#df$Male <- ifelse(df$sex == 'male', 1, 0) df$Female <- ifelse(df$sex == 'female', 1, 0)

dataf <- read.csv('https://vincentarelbundock.github.io/Rdatasets/csv/carData/Salaries.csv')
# création manuelle
dataf$Disc_A <- ifelse(dataf$discipline == 'A', 1, 0)
dataf$Disc_B <- ifelse(dataf$discipline == 'B', 1, 0)


#création automatique
dataf <- dummy_cols(dataf, select_columns = 'rank')

# Make dummy variables of two columns:
dataf <- dummy_cols(dataf, select_columns = c('rank', 'discipline'))


# solution de statistic globe
dummy2 <- as.data.frame(model.matrix(~ dataf$rank - 1))       # Applying model.matrix function
dummy2                                                  # Print dummy


# http://www.sthda.com/english/articles/40-regression-analysis/163-regression-with-categorical-variables-dummy-coding-essentials-in-r/

res <- model.matrix(~rank, data = dataf)
head(res[, -1])

library(car)
model2 <- lm(salary ~ yrs.service + rank + discipline + sex,
             data = dataf)
Anova(model2)


# discipline B (applied departments) is significantly associated with an average increase of 13473.38 in salary compared to discipline A
summary(model2) # permet d'avoir contrats suite à la variable dummy


#### Régression log ####

#Field (p325)

install.packages("mlogit")

library(car)
library (mlogit)

# possible de changer binomial pour Gaussian (pour régression linéaire)
model<-glm (detection ~ categ_split + type_incident + size_incident + spatial_displacement + incident_duration, data = data_3, family = binomial()) 
summary(model)
# null deviance is a model with no predictors
model.0<- glm(detection ~ 1, data= data_3, family = binomial()) # constante
summary(model.0)
# residual deviance is a model with the predictors (on veut valeur + petite que null deviance)
## voir model chi-square statistic | valeur à raporter | signif = model fit aux données
modelChi<- model$null.deviance - model$deviance
modelChi
#ddl | nb de variable dans model +1
chidf<- model$df.null - model$df.residual
chidf

# valeur p
chisq.prob<- 1 - pchisq (modelChi, chidf)
chisq.prob


## R2
#R2 de Hosmer & Lemeshow
R2.HL<- modelChi/model$null.deviance
R2.HL


# nb de ligne pour n Cox et Naglekerke
modelN <-  length(model$fitted.values)
modelN


# Cox & Snell / n
#1- exp ( -(nullDev - dev) / modelN)
R.CS<- 1- (exp(-(model$null.deviance - model$deviance)/modelN))
R.CS

# R2 de Nagelkerke
R.N<-R.CS/(1-(exp(-(model$null.deviance/modelN))))
R.N


## function pour avoir directement les 3 R2 ##
# This section creates a function called      #
# logisticPseudoR2s().  To use it             #
# type logisticPseudoR2s(myLogisticModel)     #

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
#End of function 
logisticPseudoR2s(model)


#Compute odds ratio
model$coefficients
exp(model$coefficients)
exp(confint(model)) # IC du odd ratio | 1 étant valeur seuil



#compare model1 and model 2
modelChi <- model$deviance - model.2$deviance
chidf <- model.1$df.residual - model.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(model.1, model.2) # comparaison model 1 et 2


#Diagnostics for model | résidu

data_3$predicted.probabilities<-fitted(model)
data_3$standardized.residuals<-rstandard(model)
data_3$studentized.residuals<-rstudent(model)
data_3$dfbeta<-dfbeta(model)
data_3$dffit<-dffits(model)
data_3 $leverage<-hatvalues(model)

# predicted.prob => quand personne dans groupe 0 sur VI alors prob pour VD = 1
head(data_3[, c("categ_split", "type_incident", "detection", "predicted.probabilities")])
data_3[, c("leverage", "studentized.residuals", "dfbeta")]



#----- Testing multicollinearity 

vif(model)
1/vif(model)

cor(model[, c("categ_split", "type_incident", "detection")])


#----- Testing the linearity of the logit 

#Create the interaction of PSWQ with log(PSWQ)
data_3$logPSWQInt<-log(data_3$PSWQ)*data_3$PSWQ

#Create the interaction of Anxious and Previous with their logs

penaltyData$logAnxInt<-log(penaltyData$Anxious)*penaltyData$Anxious
penaltyData$logPrevInt<-log(penaltyData$Previous + 1)*penaltyData$Previous

head(penaltyData)



penaltyTest.1 <- glm(Scored ~ PSWQ +
                       Anxious + 
                       Previous +
                       logPSWQInt +
                       logAnxInt +	
                       logPrevInt, 
                     data=penaltyData, family=binomial())
summary(penaltyTest.1)





#-------------------------------------ANOVA--------------------------------------------------
#Field | CH10

contr.poly() # Contrastes polynomiaux

getwd()
setwd("C:/Users/utilisateur/Desktop/R/R Data Files Field")
viagra = read.delim("Viagra.dat", header=TRUE)
View(viagra)



# install.packages("pastecs")     # stat descriptives
# install.packages("car")         # test de Levene
# install.packages("compute.es")  # taille d'effet
# install.packages("multcomp")    # test post-hoc
# install.packages("WRS")     # voir pr?p install ci-dessous



#---------------------------------prép installation WRS - Field page 202-------------------------------
# optionnel : aller sur le site de Rand Wilcox, voir si mise ? jour : https://dornsifelive.usc.edu/labs/rwilcox/software/
# copier coller script (remis ci-dessous) provenant de : https://github.com/nicebread/WRS
# v?rifier installation avec library (WRS)

# first: install dependent packages
#install.packages(c("MASS", "akima", "robustbase"))

# second: install suggested packages
#install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "mc2d", "psych", "Rfit", "DepthProc", "class", "fda"))

# third: install an additional package which provides some C functions
#install.packages("devtools")
library("devtools")
#install_github("mrxiaohe/WRScpp")

# fourth: install WRS
#install_github("nicebread/WRS", subdir="pkg")
###


library(pastecs); library(car); library(compute.es);library(multcomp); library(WRS)


#-------------------entrer manuelle des données-------------------------


libido <-c (3, 2, 1, 1, 4, 5, 2, 4, 2, 3, 7, 4, 5, 3, 6)
dose <- gl (3,5, labels = c ("placebo","low dose", "High Dose")) #fonction gl() pour cr?ation de facteur g?n?ral # nb groupe, nb cas par groupe, labels
dose<-factor(dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))

data_ANOVA <-data.frame(dose, libido)


library(Rcmdr)

summary(libido) #stat descriptives (moyenne, ...)



#--------------------ANOVA------------



Results<-aov(libido ~ dose, data = data_ANOVA) # VD ~ VI, data_frame, ajout possible de na.action = na.exclude pour exclure les NA ->données manquantes
summary(Results)

plot(Results) # voir graphe de r?sidus; 1)->homog?n?it? des variances; 2)-> normalit?

# --------- explore data

library(ggplot2)
viagra_line<-ggplot(viagra, aes(dose, libido)) #aes (titre x, titre y, s?parer selon VI)

viagra_line + 
  stat_summary(fun.y = mean, geom = "line", aes(group="1"), colour = "Blue") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.5) + 
  labs(x = "dose viagra", y = "mean libido")

#obtenir des stats descriptives pour chacun des groupes
library(pastecs)
by(viagra$libido, viagra$dose, stat.desc)

# --------------- test de Levene
library (car)
leveneTest(viagra$libido, viagra$dose, center = median) # VD,VI, mean ou median

# Test Quand variance in?gale entre les groupes;
oneway.test(libido ~ dose, data = viagra) # application F de Welch's

# -- Test robuste -- 
#Field page 441
library (WRS)

#r?organisation des donn?es en long
viagra_long<-unstack(data_ANOVA, libido~dose)
viagra_long

t1way(viagra_long, tr=.1) # t1way(viagra_long) par d?faut tr=.2 | test bas? sur moyenne coup? ''trimmed''
med1way(viagra_long) # test avec m?diane | Fm = 4.78, p=0,066
t1waybt(viagra_long, tr = .05, nboot = 2000) # test moy avec bootstrap


#--------------------------Post-hoc----------------

#t.test(x, y, paired = TRUE, alternative = "two.sided")
t.test(weight ~ group, data = my_data, paired = TRUE)

#produit un tableau avec toutes les comparaisons
t<-pairwise.t.test (data$VD,data$Group, p.adjust.method="none")
t


#Bonferroni
pairwise.t.test(data_ANOVA$libido, data_ANOVA$dose, paired = FALSE, p.adjust.method = "bonferroni") # VD,VI, pair? ou indep, method = none or bonferroni or holm or hochberg or hommel or BH or BY or fdr

#Tukey et Dunnett disponible page 449 Field

#--Post-hoc robuste
library(WRS)
lincon (viagra_long) #(dataframe, tr=.2, grp = c()) # valeurs par d?faut # IC corrig? par nb de test mais pas p
mcppb20(viagra_long) #(dataframe, tr=.2, nboot = 2000, grp = c()) # valeurs par d?faut #IC et p sont corrig? pour nb test


#---------------------------taille d'effet----------
# d 
mes(2.2, 3.2, 1.3038405, 1.3038405, 5, 5) # mes (moy G1, moy G2, SD G1, SD G2, n G1, n G2)
mes(2.2, 5, 1.3038405, 1.5811388, 5, 5)
mes(3.2, 5, 1.3038405, 1.5811388, 5, 5)

# Omega^2 # retrouver les valeurs ? partir de l'ANOVA
omega<-function(SSm, SSr, dfm, MSr) # on d?finit l'object et les valeurs qu'on va lui donner
{
  SSt = SSm + SSr
  omega = (SSm-(dfm*MSr))/(SSt+MSr)
  print(paste("Omega-Squared: ", omega))
}

omega(20.133, 23.600, 2, 1.9667)
omega(450.66,38.09, 5, 0.334)
omega(4180.6, 4356.6, 3, 167.56)



#taille d'effet pour des contrates orthogonaux
rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
print(paste("r = ", r))
}

rcontrast(2.474, 12)
rcontrast(2.029, 12)

#Field
#-------------------------------------fin chapitre 10 ANOVA------------------------------------

################################################################################################
################################################################################################


#-------------------------------------Filter RT-------------------------------------------------

#voir script Filter_RT.R

################################################################################################

#-----------------------------------ANOVA Mixte-------------------------------------------------
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/#computation
#Field

#package ez() pour ceux qui veulent approche ANOVA ou lme() pour model multiniveau

#install.packages("ez") ; install.packages("nlme"); 
library(ez); library(ggplot2); library(nlme); library (pastecs); library (reshape); library (WRS)


rt_anova = ezANOVA(
  data = data
  , dv = VD
  , wid = participant.x
  , within = Blocs5
  , between = Group
)

#Show the ANOVA and assumption tests.
print(rt_anova)


####exemple

setwd("C:/Users/utilisateur/Desktop/R/R Data Files Field")
dateData<-read.delim("LooksOrPersonality.dat", header = TRUE)
View(dateData) # VI en 9 colonnes

speedData<-melt(dateData, id = c("participant","gender"), measured = c("att_high", "av_high", "ug_high", "att_some", "av_some", "ug_some", "att_none", "av_none", "ug_none"))
View(speedData) # metter VI dans 1 colonne
#nom des colonnes attribué de façon standard, donc possible de rename
names(speedData)<-c("participant", "gender", "groups", "dateRating")

#création d'une colonne pour la premiére VI à 3 modalités # fonction gl() # 3 jeu de 60 scores 
speedData$personality<-gl(3, 60, labels = c("Charismatic", "Average", "Dullard"))

#mise en forme 2e VI ? 3 modalit?s # 3 jeu de 20 donn?es jusqu'? atteindre total de 180 (nb total de lignes)
speedData$looks<-gl(3,20, 180, labels = c("Attractive", "Average", "Ugly"))

View(speedData)


# #Enter data by hand
# 
# participant<-gl(20, 9, labels = c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20" ))
# 
# participant<-gl(20, 9, labels = c(paste("P", 1:20, sep = "_")))
# 
# 
# gender<-gl(2, 90, labels = c("Male", "Female"))
# personality<-gl(3, 3, 180, labels = c("Charismatic", "Average", "Dullard"))
# looks<-gl(3, 1, 180, labels = c("Attractive", "Average", "Ugly"))
# dateRating<-c(86, 84, 67, 88, 69, 50, 97, 48, 47, 91, 83, 53, 83, 74, 48, 86, 50, 46, 89, 88, 48, 99, 70, 48, 90, 45, 48, 89, 69, 58, 86, 77, 40, 87, 47, 53, 80, 81, 57, 88, 71, 50, 82, 50, 45, 80, 84, 51, 96, 63, 42, 92, 48, 43, 89, 85, 61, 87, 79, 44, 86, 50, 45, 100, 94, 56, 86, 71, 54, 84, 54, 47, 90, 74, 54, 92, 71, 58, 78, 38, 45, 89, 86, 63, 80, 73, 49, 91, 48, 39, 89, 91, 93, 88, 65, 54, 55, 48, 52, 84, 90, 85, 95, 70, 60, 50, 44, 45, 99, 100, 89, 80, 79, 53, 51, 48, 44, 86, 89, 83, 86, 74, 58, 52, 48, 47, 89, 87, 80, 83, 74, 43, 58, 50, 48, 80, 81, 79, 86, 59, 47, 51, 47, 40, 82, 92, 85, 81, 66, 47, 50, 45, 47, 97, 69, 87, 95, 72, 51, 45, 48, 46, 95, 92, 90, 98, 64, 53, 54, 53, 45, 95, 93, 96, 79, 66, 46, 52, 39, 47)
# 
# speedData<-data.frame(participant, gender, personality, looks, dateRating)



####Exploring Data

#bo?te ? moustache VD selon les modalit?s VI pour femmes et hommes s?par?ment
dateBoxplot <- ggplot(speedData, aes(looks, dateRating, colour = personality))
dateBoxplot + geom_boxplot() + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + facet_wrap(~gender)
#imageFile <- paste(imageDirectory,"14 Speed Date Boxplot.png",sep="/")
#ggsave(file = imageFile)

#histogramme avec IC bootstrap
looksBar <- ggplot(speedData, aes(looks, dateRating))
looksBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Attractiveness", y = "Mean Rating of Date") 
#imageFile <- paste(imageDirectory,"14 Speed Date Looks.png",sep="/")
#ggsave(file = imageFile)

#graphe en ligne
looksCharismaGender <- ggplot(speedData, aes(looks, dateRating, colour = personality))
looksCharismaGender + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= personality)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + scale_y_continuous(limits = c(0,100)) + facet_wrap(~gender)
#imageFile <- paste(imageDirectory,"14 three way interaction.png",sep="/")
#ggsave(file = imageFile)

#stat descriptives pour VI looks
by(speedData$dateRating, speedData$looks, stat.desc, basic = FALSE)
x

#-------------------------------------using ezAnova / Field------------------------------------------
# from Field's book - repeated anova
library(ez) #m?me package que ezPerm
# https://www.rdocumentation.org/packages/ez/versions/4.4-0

RES<-ezANOVA (data = data_5, dv = time_to_respond_ms, wid = participant.x, within = alert)
print (RES)


#pour contraste:
#SomevsNone<-c(1, 1, -2)
#HivsAv<-c(1, -1, 0)
#AttractivevsUgly<-c(1, 1, -2)
#AttractvsAv<-c(1, -1, 0)
#contrasts(speedData$personality)<-cbind(SomevsNone, HivsAv)
#contrasts(speedData$looks)<-cbind(AttractivevsUgly, AttractvsAv)

#options(digits = 3)

speedModel<-ezANOVA(data = speedData, dv = .(dateRating), wid = .(participant),  between = .(gender), within = .(looks, personality), type = 3, detailed = TRUE)
# dv=VD ; wid = variable identifiant les P ; between = VI Inter ; within = VI Intra ; Type = SC de type III ; detailed = permet d'avoir d?tail SC

speedModel # pour voir r?sultats
#options(digits = 7)



#------------------------------ permutation test-t indep ------------------------------------

getwd()
setwd("C:/Users/utilisateur/Dropbox/2020/1 - Postdoc U Laval/CSSS/Analyses/Team")

filename <- "C:/Users/utilisateur/Dropbox/2020/1 - Postdoc U Laval/CSSS/Analyses/Team/CSSS_Detection_team_lag5_(v2.5).xls"

library(readxl) #lire Excel/ouverture du chemin/capable de trouver feuille excel avec sheet =""
data_1 <- read_excel(file.choose(filename), sheet="TCD_percent")
View(data_1)

library(nparcomp)
library(Rcmdr)


#-------------------stat descriptives

numSummary(data_1[,"VD", drop=FALSE], data_1$Group, statistics=c("mean", "sd", "IQR", "quantiles", "skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")

#--------------------R?alisation du test de permutation avec le module nparcomp

modele_a <- npar.t.test( VD ~ Group,           # VD ~ VI
                         data        =  data_1,         # Nom de la table
                         conf.level  =  0.95,        # Intervalle de confiance
                         method      = "permu",      # Test de permutation studentis?
                         nperm       =  10000,       # Nombre de permutations
                         alternative = "two.sided" ) # Test bidirectionnel (two.sided); unidirectionel (greater OU less)
summary(modele_a)

# Il est g?n?ralement plus facile d'interpr?ter un estimateur > 0.5
#
# Codage 1, utiliser la ligne de syntaxe ci-dessous:

data_1$Group <- with(data_1, factor(Group, levels=c('C2','C3')))
#
# Codage 2, utiliser la ligne de syntaxe ci-dessous:

data_1$Group <- with(data_1, factor(Group, levels=c('C3','C2')))



##graphe/histo

a<-ggplot(data_7, aes(x=size_incident, y=VD, group =size_incident, alert = size_incident, fill=size_incident)) + 
  geom_violin(trim=FALSE, fill='grey', color="grey")+
  geom_boxplot(width=0.1, fill='white', color="black")+
  theme_classic()
b<-ggplot(data_7, aes(x=size_incident, y=VD, group =size_incident, alert = size_incident, fill=size_incident)) + 
  geom_violin(trim=FALSE, fill='grey', color="grey")+
  theme_classic() + stat_summary(fun.data = "mean_sdl")

library(cowplot)
plot_grid(a, b, labels = "AUTO")





#------------------------------ permutation test-t rep ------------------


#besoin de fichier en long et non large (d?j? le cas pour ?quivalent ? test-t)

#besoin de s?lectionner un groupe ? chaque fois
#s?lectionner des lignes en lien ? un groupe
#Group_1<-data_1 [data_1$Group=="1",]
#Group_1

#comparaison des moyennes - groupe contr?le - (moy B1 ? B4) VS B5 - n?gligence

numSummary(data_1[,"diff_G1", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles", "skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")

modele5 <- npar.t.test.paired (VD_rep_G1 ~ VI_Rep_G1, 
                                                        data_1, 
                                                        conf.level  =  0.95, 
                                                        alternative = c("two.sided"), 
                                                        nperm=10000,
                                                        rounds=3, 
                                                        info = TRUE,
                                                        plot.simci = FALSE)

summary(modele5)


#comparaison des moyennes - groupe SCANTRACKER - (moy B1 ? B4) VS B5 - n?gligence

numSummary(data_1[,"diff_G2", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles", "skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")

# Codage 2, utiliser la ligne de syntaxe ci-dessous (pour p.hat>0,5):
data_1$VI_Rep_G2 <- with(data_1, factor(VI_Rep_G2, levels=c('bloc5','4blocs')))

modele6 <- npar.t.test.paired (VD_rep_G2 ~ VI_Rep_G2, 
                               data_1, 
                               conf.level  =  0.95, 
                               alternative = c("two.sided"), 
                               nperm=10000,
                               rounds=3, 
                               info = TRUE,
                               plot.simci = FALSE)

summary(modele6)

#Groupe 2 - Scantracker---uniquement besoin de ce groupe !

quatreblocs<-data_1 [data_1$Group=="1",]
quatreblocs
summary(quatreblocs$VD_rep_G2)
numSummary(quatreblocs$VD_rep_G2, statistics=c("mean", "sd", "IQR", "quantiles", "skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")

Bloc5<-data_1 [data_1$Group=="2",]
Bloc5
summary(Bloc5$VD_rep_G2)
numSummary(Bloc5$VD_rep_G2, statistics=c("mean", "sd", "IQR", "quantiles", "skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")









#------------------------------ perm ANOVA / RVAideMemoire ----------------
# de Herv? (2020)  => Package 'RVAideMemoire'; page 110


# the final column labelled Pr (>F) indicates the likelihood of an F-ratio the size of the one obtained 
# occuring if there was no effect in the population


#install.packages("RVAideMemoire", dependencies = TRUE)
library(RVAideMemoire)

load("C:/Users/utilisateur/Desktop/R/Permutation/Donnees et syntaxe R - Permutations/DON.R.INDEP.dominance.RData")

#perm.anova(formula, nest.f2 = c("fixed", "random"), data, nperm = 999, progress = TRUE)
perm.anova(DOULEUR ~ GROUPE, nest.f2 = c("fixed"), data = txd, nperm = 1000, progress = TRUE)

#ici 2 groupes alors on peut utiliser perm.t.test

perm.t.test(DOULEUR~GROUPE,data=txd)


df<-perm.anova(VD ~ Group, nest.f2 = c("fixed"), data = data_2, nperm = 1000, progress = TRUE)
df$"F value"
df$"Pr(>F)"
df # pour afficher les r?sultats

#----------- test en ANOVA 'classique' ---------------------


library(haven) #lire donn?es SPSS

filename_SPSS <- "C:/Users/utilisateur/Downloads/resist_stress.sav"

data_test <- read_sav(filename_SPSS) # fichier.dat <- read.delim(("C:/U/R Data Files Field/Glayession.dat"))

View(data_test)


#!!!attention a avoir variable en facteur
#install.packages("tidyverse") #utile pour aggrégation des données (voir Analyse CSSS Team QUASA.R)
levels(data_test$group)
str(data_test)

data_test$group<-as.factor(data_test$group)

Results<-aov(data_test$resistance_stress ~ data_test$group, data = data_test) # VD ~ VI, data_frame, ajout possible de na.action = na.exclude pour exclure les NA ->donn?es manquantes
summary(Results)


#---

#obtenir des stats descriptives pour chacun des groupes
library(pastecs)
by(data_test$resistance_stress, data_test$group, stat.desc)

# --------------- test de Levene
library (car)
leveneTest(data_test$resistance_stress, data_test$group, center = median) # VD,VI, mean ou median

#group_2test<-gl(4,1, labels = c("groupeA","groupeB","groupeC","groupeD"))
#levels(group_2test)



# Test Quand variance in?gale entre les groupes;
oneway.test(data_test$resistance_stress ~ data_test$group, data = data_test) # application F de Welch's

# -- Test robuste -- 
#Field page 441
library (WRS)

#r?organisation des donn?es en long
data_test<-unstack(data_test, data_test$resistance_stress~data_test$group)
data_test

t1way(data_test, tr=.1) # t1way(viagra_long) par d?faut tr=.2 | test bas? sur moyenne coup? ''trimmed''
med1way(data_test) # test avec m?diane | Fm = 4.78, p=0,066
t1waybt(data_test, tr = .05, nboot = 2000) # test moy avec bootstrap


#---


#comparaison avec perm anova

#perm.anova(formula, nest.f2 = c("fixed", "random"), data, nperm = 999, progress = TRUE)

perm.anova(resistance_stress ~ group, nest.f2 = c("fixed"), data = data_test, nperm = 1000, progress = TRUE)

pa <- perm.anova(resistance_stress~group,data=data_test)
pa$"F value"
pa$"Pr(>F)"



#---------------------- MANOVA.RM ---------------------------
#sarah.friedrich@med.uni-goettingen.de
#crossdesign

#https://www.rdocumentation.org/packages/MANOVA.RM/versions/0.3.4
#https://cran.r-project.org/web/packages/MANOVA.RM/MANOVA.RM.pdf


# possible d'avoir une ANOVA mixte avec le package MANOVA.RM selon 
#frank.konietschke@charite.de

install.packages("MANOVA.RM")
library(MANOVA.RM)

# analyse à mesures répétés 
# besoin de devtools::install_github("smn74/MANOVA.RM") # plus vrai avec mise ? jour
# library(devtools)
# devtools::install_github("smn74/MANOVA.RM") # puis faire 1 #en cas de probl?me faire mise ? jours des packages

# Friedrich recommande parametric bootstrap MATS (Friedrich & Pauly, 2018)
# ANOVA répété multRM
Model_1 <- multRM(time_to_respond_ms ~ alert, data = data_5,
                  subject = "participant.x", within = "alert" , iter = 1000, alpha = 0.05, 
                  resampling = "paramBS", dec=3)
summary(Model_1)


Model_1 <- RM(time_to_respond_ms ~ Group * alert, data = data_3,
              subject = "participant.x", no.subf = 1, iter = 100, resampling = "Perm")
summary(Model_1)

Model_1 <- MANOVA(time_to_respond_ms ~ Group * alert, data = data_3,
                  subject = "participant.x", iter = 100, resampling = "paramBS")
summary(Model_1)



#### Post Hoc ####

# posthoc avec 1 seul facteur dans model
simCI(Model_1, contrast = "pairwise", type = "Tukey")

# Multivariate post-hoc comparisons and simultaneous confidence intervals for contrasts in multivariate factorial designs

simCI(object, contrast, contmat = NULL, type = NULL, base = 1, 
      interaction = TRUE, factor = NA, silent = FALSE, ...)

library(multcomp) # INFO DANS LIVRE Bretz et al (2010)

#contrMat(n, type = c("Dunnett", "Tukey", "Sequen", "AVE", "Changepoint", "Williams", "Marcus", "McDermott", "UmbrellaWilliams", "GrandMean"),
 #        base = 1)




# ---------------------- Package 'rankFD'###
# https://cran.r-project.org/web/packages/rankFD/rankFD.pdf
#crossdesign


#### Post Hoc # Bonferroni #####
#A convenience wrapper for computing post-hoc test after having calculated an ANOVA 
#https://www.rdocumentation.org/packages/DescTools/versions/0.99.39/topics/PostHocTest

library(DescTools)

PostHocTest(rt_anova, which = NULL,
            method = "hsd",
            conf.level = 0.95, ordered = FALSE)

PostHocTest(x, which = NULL,
            method = c("hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"),
            conf.level = 0.95, ordered = FALSE, ...)


#---------------------- ezperm ----------------------
# Michael A Lawrence
# m?me package que ezANOVA
# https://www.rdocumentation.org/packages/ez/versions/4.4-0


# This function provides easy non-parametric permutation test analysis of data from factorial experiments, including purely within-Ss designs 
#(a.k.a. "repeated measures"), purely between-Ss designs, and mixed within-and-between-Ss designs.

#install.packages("ez")
library(ez)

ezPerm( data , dv , wid , within = NULL , between = NULL , perms = 1e3 , 
        parallel = FALSE , alarm = FALSE)
print (ezPerm)

Comp<- ezPerm( data = data_5 , dv = time_to_respond_ms , wid = participant.x, within = alert , 
               between = NULL , perms = 100 , parallel = FALSE , alarm = FALSE)
print (Comp)
#pour afficher les r?sultats (valeur visible au complet) tester perm.anova


# Plot data from a factorial experiment
#for help : https://rdrr.io/cran/ez/man/ezPlot.html

ezPlot( data = data_5 , dv = time_to_respond_ms , wid = participant.x, within = alert
        , within_full = NULL , within_covariates = NULL , between = NULL , between_full = NULL , 
        between_covariates = NULL , alert , do_lines = TRUE , do_bars = TRUE , bar_width = NULL , 
        bar_size = NULL , split = NULL , row = NULL , col = NULL , to_numeric = NULL , x_lab = NULL , y_lab = NULL , split_lab = NULL , 
        levels = NULL , diff = NULL , reverse_diff = FALSE , type = 2 , dv_levs = NULL , dv_labs = NULL , y_free = FALSE , print_code = FALSE
)
print (ezPlot)

#within
data_5$alert<-as.factor(data_5$alert)
ezPlot( data = data_5 , 
        dv = .(time_to_respond_ms) , 
        wid = .(participant.x), 
        within = .(alert), 
        x = .(alert) , 
        x_lab = 'alert' , 
        y_lab = 'time_to_respond_ms' , 
)


#--------------------------------- MOCC R FUN ----------------

## MOOC FUN R ##

# cours 1

getwd() # v?rifier le working directory 
setwd("C:/Users/utilisateur/Desktop/R") 

library(readxl)

fichier <- read.csv2("C:/Users/utilisateur/Desktop/R/MOOC/smp1.csv")

str(fichier) #d?crire le fichier

# repr?sentation graphique

table (fichier$prof) #calcul des effectifs
barplot(table (fichier$prof)) #histogramme

pie (table(fichier$prof)) #diagramme circulaire

hist(fichier$age, col = "red", xlab="age")
boxplot(fichier$age, xlab="age")

boxplot(fichier$age~fichier$rs, main = "titre=",xlab="recherche de sensation")


#install.packages("gplots")
library (gplots)

fichier2 <- read.csv2("C:/Users/utilisateur/Desktop/R/MOOC/outils_hdrs.csv")

plotmeans (fichier2$HDRS~fichier2$VISIT, gap=0, barcol="black")

interaction.plot(fichier2$VISIT, fichier2$NUMERO, fichier2$HDRS, lty=1, legend=FALSE) # lty pour trait

# mesure de dispersion ...

summary(fichier)
str(fichier)
mean(fichier$age, na.rm = TRUE) # permet d'exclure na dans calcul

data_all_1<-na.omit(data_all) # Supprime toutes les lignes avec des NA

#fichier$age<-as.numeric(fichier$age)
#str(fichier$age)

#install.packages("prettyR") 
library (prettyR)

describe (fichier) # permet d'avoir descriptif propre

# lab1 -

x <- factor(fichier$dep.cons, levels= c(0,1), labels=c("non", "oui"))
table (x)

table(fichier$scz.cons)
y<- factor(fichier$scz.cons, labels = c("non", "oui"))
table(y)

sum(table(fichier$prof),na.rm = TRUE) # AJOUT ,na.rm = FALSE pour exclure

Z<-fichier$prof
describe(Z)
sum(table(fichier$grav.cons))
table(fichier$grav.cons)


table (fichier$age > 20) # nb de donn?e sup?rieur ? 20
table(fichier$n.enfant)
#nlevels(fichier$n.enfant)
enfant<-factor(fichier$n.enfant)
str(enfant)

levels(enfant)
levels(enfant)[4:13]<-"5+" #permet de regrouper pls categ en une seule
table (enfant)

#recoder une variable en binaire 

table(factor(fichier$n.enfant >= 5, labels=c('<5', '5+')))

barplot(prop.table(table(fichier3$n.enfant))*100, xlim = c(0,30), ylim=c(0,30), las=1)

# lab2 -

head (fichier$prof =="agriculteur") #double égal permet de cibler pour 1 variable
#donne pas tant d'info; affiche FALSE pour dire que les premiéres valeurs correspondent pas et NA

table (fichier$prof =="sans emploi") #permet de connaitre nb données qui remplissent la condition
which (fichier$prof =="agriculteur") # pour connaitre les positions des données

fichier$age [which (fichier$prof =="agriculteur")] #age des gens agriculteurs

#commande subset

subset(fichier, prof == "sans emploi", age)
subset(fichier, prof == "agriculteur", age)
subset(fichier, prof == "agriculteur", 1:5) #on a les 5 premiéres colonnes des agriculteurs
subset(fichier, prof == "agriculteur", c(age, n.enfant)) # permet de sélectionner les colonne qu'on veut voir
subset(fichier, prof == "agriculteur" & n.enfant > 2, c(age, n.enfant))
subset(fichier, prof == "agriculteur" & n.enfant > 2 & complete.cases (dr), c(age, n.enfant)) #sert à rien ici mais permet d'enlever les donn?es NA sur variable

tab<-table (enfant)
tab
sum(tab)

tab/sum(tab) 

round(tab/sum(tab),2) # round pour arrondir à 2 décimales

hist(fichier$age) # diagramme de fr?quence
hist(fichier$age, nclass=4) #r?duire histo en 4 classes
hist(fichier$age, nclass=4, prob=TRUE, las=1) #densit?
lines (density(fichier$age, na.rm=TRUE)) #obtenir une ligne dans histo

library(RVAideMemoire)
library(Rcmdr)
numSummary(fichier2[,"HDRS", drop=FALSE], fichier2$HDRS, statistics=c("mean", "sd", "IQR", "quantiles", "skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")


fichier3 <- read.csv2("C:/Users/utilisateur/Desktop/R/MOOC/satisfaction_hopital.csv")

str(fichier3) #d?crire le fichier

#exercice: pr?senter les pourcentages de sujets relevant de chacune des modalit?s.

#proportion de sujets selon les cat?gories de service # mon fichier de donn?es se nomme 'fichier3'
round(prop.table(table(fichier3$service))*100,1) # pour avoir des proportions

#proportion de sujets selon les cat?gories de sexe
round(prop.table(table(fichier3$sexe))*100,1) 
#proportion de sujets selon les professions
round(prop.table(table(fichier3$profession, useNA = "always"))*100,1)


### cours 2 ################################

#install.packages("binom")
library (binom)
binom.confint(300, 1000, method="all") #pour avoir simulation et IC


plot(jitter(fichier$age), jitter(fichier$n.enfant))

#correlation pearson
cor(fichier$age,fichier$n.enfant,use="complete.obs") #complete.obs permet de g?rer les donn?es manquantes

#odds ratio

fichier$ed.b<-ifelse(fichier$ed>2,1,0) #cr?ation d'une colonne
str(fichier)
table(fichier$ed.b, fichier$ed, deparse.level=2,useNA="always")

#install.packages("Epi")
#pour calcul odds ratio
library(Epi)
twoby2(1-fichier$ed.b,1-fichier$dep.cons) #par d?faut, la fonction twoby2() consid?re que quand une variable est cod?e 0/1, 0 signifie qu'on est malade, versus 1 : non-malade et que 0 signifie avoir le facteur de risque, versus 1 : ne pas avoir le facteur de risque, alors que dans notre jeu de donn?es c'est exactement le contraire,
twoby2(fichier$ed.b,fichier$dep.cons)

#lab 3 ---cr?ation de Markdown


fichier <- read.csv2("C:/Users/utilisateur/Desktop/R/MOOC/smp1.csv")
fichier4 <- read.csv2("C:/Users/utilisateur/Desktop/R/MOOC/smp2.csv")


which (fichier$prof == "autre")
which (fichier$prof == "ouvrier")
barplot(fichier$n.enfant)
barplot(table(fichier$n.enfant)) #distrib des effectifs avec barplot(table())
subset()

#sélection de données             # autre manip de donn?es voir 'Analyse CSSS Team QUASA.R'
selection<-subset(fichier4, dep.cons == 1, c(age, n.enfant))
#stat sur les donn?es s?lectionn?s
summary (selection)

selection_a<- subset(fichier4, age<35, select = c("duree", "prof", "age"))
summary (selection_a)

table(fichier4$suicide.past)
selection_b<- subset(fichier4, suicide.past==1,select = c("duree", "dur.interv", "age", "suicide.past"))
summary (selection_b) # moy dur.interv= 67.35

mean(fichier4[fichier4$suicide.past == 1, 'dur.interv'], na.rm=TRUE) 


summary (fichier4)
testage<-fichier4$age
summary(testage)
#levels(testage)
table(factor(testage <=28, labels=c('1')))

testage2 <- findInterval(testage, c(19,28,37,48,83))
summary(testage2)
levels((testage2))

#install.packages("questionr")
library(questionr)

# red?finire ?ge en cat?gorie
A <- cut(testage, c(18, 28, 37, 48, 83), include.lowest = TRUE)
table(A)



### cours 3 ################################

#test Chi-2

table(fichier$ed.b, fichier$dep.cons, deparse.level=2,useNA="always")
#useNA="always" dans le but de d?terminer le nombre de d?tenus ayant des donn?es manquantes
tab<-table(fichier$ed.b, fichier$dep.cons, deparse.level=2)
prop.table(tab,1) # obtenir des %
#Le nombre 1 signifie que nous souhaitons estimer le pourcentage de d?pression selon que les d?tenus ont ou n'ont pas un haut niveau d'?vitement du danger.
prop.table(tab,2) 
#le pourcentage de d?tenus ayant un haut niveau d'?vitement du danger selon que ces d?tenus sont ou ne sont pas d?prim?s.


#si tableau chi-2 trop proche de 0 ou 100% alors R donne message
chisq.test(fichier$ed.b, fichier$dep.cons, correct=FALSE)
#correct=FALSE, sinon R propose un test du chi-2 avec correction de continuit?, qui est un test plus robuste mais nettement moins puissant. 

#si Chi-2 pas utilisable alors test exact de Fisher
fisher.test(fichier$ed.b, fichier$dep.cons)

# section test-t
hist(fichier$age)
qqnorm(fichier$age) ; qqline (fichier$age)

#v?rif variance/ecart-type
by(fichier$age, fichier$ed.b, sd, na.rm=TRUE)

#test-t
t.test (fichier$age~fichier$ed.b, var.equal=TRUE)
#var.equal=TRUE, sinon, par d?faut, R va utiliser l'approximation de Welch, qui permet d'avoir un test t m?me quand les variances sont in?gales, et ?a n'est pas recommand?

wilcox.test(fichier$age~fichier$ed.b)

#test nullite coefficient de correlation

cor.test(fichier$age, fichier$rs)
cor.test(fichier$age, fichier$rs, method="spearman")

# comparaison d'une moyenne ? une r?f?rence

t.test(fichier$age, mu=24)

#test appari?s
mcnemar.test(b.debut, b.fin)
t.test(x.debut, x.fin, paired=TRUE)

#labo 4

fichier4 <- read.csv2("C:/Users/utilisateur/Desktop/R/MOOC/smp2.csv")
table (fichier4$subst.cons)

table (fichier4$subst.cons, fichier4$abus) # obtention d'un tableau de contingence
tab <- table (fichier4$subst.cons, fichier4$abus)
prop.table(tab) # proportion
prop.table(tab, margin = 1) #en ligne
prop.table(tab, margin = 2) #en colonne

xtabs(~subst.cons+abus, fichier4) # tableau avec nom des var

barplot (xtabs(~subst.cons+abus, fichier4)) 
barplot (xtabs(~subst.cons+abus, fichier4), beside =TRUE) 

chisq.test(tab) # test chi-2
res<- chisq.test(tab)
res$observed
res$expected

fisher.test(tab)

head(fichier4$age)
table (fichier4$subst.cons) # nb dans les diff cat?g d'une variable
table (fichier4$subst.cons, useNA = "always")
tapply(fichier4$age, fichier4$subst.cons, mean, na.rm=TRUE)

t.test(fichier4$age[fichier4$subst.cons==0], fichier4$age[fichier4$subst.cons==0]) #test t welch
t.test(fichier4$age[fichier4$subst.cons==0], fichier4$age[fichier4$subst.cons==0], var.equal=TRUE) # test t classique

t.test(age~subst.cons, fichier4) # test t aussi

aggregate(age~ subst.cons, fichier4, mean, na.rm=TRUE)
with(fichier4, tapply(fichier4$age, fichier4$subst.cons, mean, na.rm=TRUE)) # moy pour chaque niveau

boxplot(age~ subst.cons, fichier4)


#quiz
with(fichier4, tapply(fichier4$age, fichier4$subst.cons, mean, na.rm=TRUE)) 
tapply(fichier4$dur.interv, fichier4$dep.cons, median, na.rm=TRUE)

cor.test(fichier4$age, fichier4$dur.interv)

wilcox.test(fichier4$dur.interv~fichier4$suicide.past)


#devoir

library(questionr)
# permet recodage de variable # irec(dataframe, colonne/variable)
irec(data, recommander.b)

#chargement du fichier de donn?es ; mon jeu de donn?es se nomme alors 'data'

data <- read.csv2("C:/Users/utilisateur/Downloads/satisfaction_hopital.csv")
#visionnage des donn?es
View(data)
#obtenir un aper?u des donn?es et des types de variable (continue ou nominal...)
str(data)

#1 - tranformation de la variable 'recommander' en variable binaire
# utilisation de la library dplyr pour la fonction recode qui va suivre
library(dplyr)
## Recodage de data$recommander en data$recommander.b
data$recommander.b <- recode_factor(data$recommander,
                                    "1" = "0",
                                    "2" = "1",
                                    .default = "other"
)
#ou
data$recommander.b <- ifelse(data$recommander>1,1,0)



#inversion des scores des items 4, 7, 9, 11, 13, 17, 18, 19 et 21 

data_6$Q12___Estime_corpore_4 <- recode(data_6$Q12___Estime_corpore_4, "1=5; 2=4; 3=3;4=2; 5=1")


# autre exemple avec recode (sans _factor) :
# nouvelle colonne avec inversion des scores pour question_id 5 qui doit être inversé
#1: data_3<-data_3%>%mutate(answer_ok = ifelse(question_id == 5, 10 - answer, answer))
data_3$answer_ok <- ifelse (data_3$question_id ==5,(recode(data_3$answer,
                                                           "0" = "10",
                                                           "1" = "9",
                                                           "2" = "8",
                                                           "3" = "7",
                                                           "4" = "6",
                                                           "5" = "5",
                                                           "6" = "4",
                                                           "7" = "3",
                                                           "8" = "2",
                                                           "9" = "1",
                                                           "10" = "0",
                                                           .default = "other")),data_3$answer)

data$i24_suspect_police <- recode_factor(data$i24_suspect_police,
                                         "1 - pas n?cessaire/pas grave" = "1",
                                         "2" = "2",
                                         "3" = "3",
                                         "4" = "4",
                                         "5" = "5",
                                         "6" = "6",
                                         "7 - n?cessaire/grave" = "7",
                                         .default = "other")
data$i24_suspect_police<-as.numeric(data$i24_suspect_police)


#2 - force de l'association entre ? recommander.b ? et ? sexe ?
#utilisation de la library Epi pour la fonction twoby2 qui va suivre
library(Epi)
## twoby2 pour tableau avec odds-ratio (1,0337) similaire au risque relatif (1,0232)
## avec intervalle de confiance ? 95% du odds-ratio de 0,67 (borne inf?rieur) et 1,59 (borne sup?rieur)
twoby2(data$sexe,data$recommander.b) 
## Cela signifie que les recommandations du service ? son entourage ne diff?rent pas selon qu'on soit un homme ou une femme

#3 - corrélation (de Pearson) entre ? score.relation ? et ? age ?
#vérification de la normalité de la variable age avec un histogramme 
barplot(table (data$age))

#test nullit? coefficient de correlation
cor.test(data$score.relation, data$age)
# L'ampleur de la correlation est de 0,096, c'est ? dire que la force de la relation entre les variables est assez faible
# la correlation est non significative (p=0,073); il y a moins de 95 chances sur 100 que cette correlation soit comprise entre -0,009 et 0,19

#4 - Comparaison de moyenne homme VS femme sur la variable score de relation

#v?rification des conditions de validit? du test
#n>30 
table (data$sexe) #nous avons bien des effectifs sup?rieur ? 30 ici pour les 2 groupes
#v?rification de la normalit? de la variable score de relation avec un histogramme 
barplot(table (data$score.relation)) # distribution non normal
qqnorm(data$score.relation); qqline (data$score.relation) # distribution non normal
#v?rif variance/ecart-type
by(data$score.relation, data$sexe, sd, na.rm=TRUE) # variance similaire ici

# r?alisation du test de wilcoxon car la normalit? n'est pas pr?sente dans les donn?es
wilcox.test(data$score.relation~data$sexe)
#conclusion: diff?rence non significative entre hommes et femmes sur score de relation










### cours 4 ##########
#RLS, RLM, Reg log

data <- read.csv2("C:/Users/utilisateur/Downloads/smp2.csv")
View(data)
str(data)

plot(data$age, data$dur.interv)
#jitter pour voir max de donn?es
plot(jitter(data$age), jitter(data$dur.interv, factor = 4))
#factor=4 pour augmenter l'intensit? du bruit.

#droite avec nuage de point abline # lm pour calcule des coordonn?es
#lwd=2 pour ?paisseur droite
abline(lm(data$dur.interv~data$age), lwd=2)

#obtenir coeff du model/de la droite
model<-lm(data$dur.interv~data$age)
summary(model)

#m?me p pour correl sauf que RLS, valeur b s'interpr?te mieux
cor.test(data$dur.interv,data$age)

#RLM

model1<- lm (dur.interv~age+dep.cons+subst.cons+scz.cons,data=data)
summary(model1)

model2<- lm (dur.interv~age+dep.cons+subst.cons+scz.cons+prof,data=data)
summary(model2)


#choisir son groupe de r?f?rence avec relevel n?cessitant que la variable soit de type factor
data$prof<-as.factor(data$prof)
data$prof<-relevel(data$prof, ref="ouvrier")
model3<- lm (dur.interv~age+dep.cons+subst.cons+scz.cons+prof,data=data)
summary(model3)

#mod?le plus g?n?ral pour les modalit?s
drop1(model3,.~.,test="F")


#interaction # attention de bien mettre variable en catég !!
model3<- lm (dur.interv~age+dep.cons*subst.cons+scz.cons+prof,data=data)
summary(model3)


#ANOVA
model4<-lm(dur.interv~prof, data=data)
summary(model4)
drop1(model4,.~.,test="F")

#v?rification de la normalit? des r?sidus
model3<- lm (dur.interv~age+dep.cons+subst.cons+scz.cons,data=data)
hist(resid(model3), col="grey", main= "")

#r?gression logistique

modela<-glm(suicide.hr~abus, data=data, family="binomial")
summary(modela)
exp(0.7688) 

library(Epi)
twoby2(1-data$suicide.hr, 1-data$abus)

modelb<-glm(suicide.hr~abus+discip+duree, data=data, family="binomial")
summary(modelb)

#lab5

data <- read.csv2("C:/Users/utilisateur/Downloads/smp2.csv")
View(data)
str(data)

head(subset (data, prof=="sans emploi" | prof=="prof.intermediaire" | prof=="cadre", c(age,n.enfant, prof)))

data_a<-(subset (data, prof=="sans emploi" | prof=="prof.intermediaire" | prof=="cadre", c(age,n.enfant, prof)))
data_a$prof<-factor(data_a$prof)
summary (data_a)
table(data$prof)
table (data_a$prof)

aggregate(n.enfant ~ prof, data_a, mean) #nb enfant moyen selon prof
boxplot(n.enfant ~ prof, data_a)

m<-lm (n.enfant ~ prof, data_a) # resultat dans m
m

drop1(m, test="F") #tableau analyse de variance, VI prof

z<-lm(n.enfant~age, data_a)
z
summary(z) # pour avoir test et tableau de r?gression

w<-lm (n.enfant~age, data, subset = prof=="sans emploi" | prof=="prof.intermediaire" | prof=="cadre")
w
summary(w)

coef(w) # pour obtenir les coeff
coef(w)[2] #place du coeff qu'on veut
coef(w) ["age"] # ou nom

confint(w) # pour IC 95%
anova(w) # tableau anova
 
#pour pr?d ac IC ? 95%
predict(w, data.frame(age=c(20,30,40)), interval="confidence")

#regl log
data$n.enfant.bin<- ifelse(data$n.enfant>2,1,0) # si valeur sup ? 2 alors valeur 1 sinon 0
table(data$n.enfant)
table(data$n.enfant.bin)

h<-glm(n.enfant.bin ~ age, data=data, family=binomial("logit"))
summary(h)

#quiz
#pour recoder en 3 classes avec anova
n.fratrie.c <- cut(data$n.fratrie, breaks = c(0, 2, 4, 21), include.lowest = TRUE)
anova(lm(age ~ n.fratrie.c, data = data))

m <- glm(separation ~ age, data = data, family = binomial)
confint(m)
exp(confint(m))




### cours 5 #### données censurées, survie #####

alc<-read.csv2("C:/Users/utilisateur/Downloads/alcool.csv")
str(alc)
#install.packages("survival")
library(survival)

#courbe de Kaplan Meier # plot(survfit(Surv(dur?e,?v?nement)~1)) 
plot(survfit(Surv(alc$t, alc$SEVRE)~1), mark.time=TRUE, main="courbe de maintien dans l'asbtinence")

#possible de voir courbe selon sexe
plot(survfit(Surv(t, SEVRE)~SEXE, data=alc),col=c("black", "red"), mark.time=TRUE, main="courbe de maintien dans l'asbtinence")

#obtenir m?diane de survie
survfit(Surv(alc$t, alc$SEVRE)~1)

#test du log-rank
survdiff(Surv(t,SEVRE)~SEXE, data=alc) # nb obs trop petit, 3 femmes


#test de Cox pour association entre var quantit et survie
coxph(Surv(t,SEVRE)~AGE, data=alc)
#avec plusieurs variables
mod<-coxph(Surv(t,SEVRE)~AGE+SEXE+EDVNEG, data=alc)
mod
exp(coef(mod)) #L'exponentielle du coefficient vaut 0.64, nous avons donc, 36% de chances de moins pr?senter un risque de rechute ? un instant donn?. Ce 0.64 correspond ? un "hazard ratio" ou "rapport des risques instantan?s de d?c?s".
#36% de chances en moins de faire une rechute de la maladie alcoolique quand on a eu des ?v?nements de vie plut?t que quand on n'en a pas eu


#condition d'application
par(mfrow=c(2,2))#pour voir pls graphes en m?me temps
plot(cox.zph(mod)) #nous devons obtenir trois courbes en traits continus qui sont le plus horizontal possible.


####statistiques exploratoire multidimensionelle####

#correlation option sur données manquantes
#use="complete.obs" quand vous enlevez tous les sujets qui ont au moins une donn?e manquante, et puis l'autre option, plus laxiste : use="pairwise.complete.obs".
data<-read.csv2("C:/Users/utilisateur/Downloads/smp1.csv")
#stockage des variables dans un objet
var<-c("age","n.enfant","scz.cons","dep.cons", "grav.cons", "rs", "ed", "dr")
#pour arrondir les valeurs
round(cor(data[,var], use="complete.obs"), digits=3)

round(cor(data), digits = 2) 

#graphiqe
#install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1)) # ne pas fragmenter le graphique
#correlation avec cercle
corrplot(cor(data[,var],use="complete.obs"), method="circle")


###ACP####

var<-c("age","n.enfant","scz.cons","dep.cons", "grav.cons", "rs", "ed", "dr")
#install.packages("psy")
library(psy) # pour fonction mdspca
mdspca (data[,var]) # Il y a toujours une marge d'interpr?tation, mais encore une fois ce n'est pas grave, il s'agit d'une m?thode exploratoire multidimensionnelle, il ne s'agit pas de faire des inf?rences tr?s solides comme on l'a fait avec des tests d'hypoth?se
#les % additionn?s du graphique donne le % de variance  de la matrice de corrélation

#représentation sphérique d'une matrice de corrélation
sphpca(data[,var]) 
#rotation de la sph?re
sphpca(data[,var],v=55)

#analyse en composante principale focalis?e
#Cette m?thode est en outre particuli?rement adapt?e aux situations o? vous avez une variable ? expliquer et des variables explicatives.
expliquer<-"grav.cons"
explicatives<-c("age","n.enfant","scz.cons","dep.cons", "rs", "ed", "dr")
fpca(data=data, expliquer, explicatives, partial="No")




###devoir####

#chargement des donn?es
data <- read.csv2("C:/Users/utilisateur/Desktop/R/MOOC/satisfaction_hopital.csv")
str(data)

# 1 -  mod?le de r?gression lin?aire expliquant la variable ? score.relation ? par les variables ? age ?, ? sexe ?, ? score.information ?, ? amelioration.sante ?, ? amelioration.moral ?, ? profession ?,  ? service ?.

modele<- lm (score.relation~age+sexe+score.information+amelioration.sante+amelioration.moral + profession+ service,data=data)
modele
summary (modele)
# on constate que les pr?dicteurs significatifs de score.relation sont age,score.information et amelioration.moral
# v?rification de la normalit? des r?sidus # semble pas si mal
hist(resid(modele), col="grey", main= "")

# 2 - mod?le de r?gression logistique expliquant la variable ? recommander.b ? par les variables ? age ?, ? sexe ?, ? score.information ?, ? amelioration.sante ?, ? amelioration.moral ?, ? profession ?,  ? service ?. 

#recodage de la variable
data$recommander.b <- ifelse(data$recommander>1,1,0)
#vérification recodage
table(data$recommander)
table(data$recommander.b)

#modéle de régression logistique
regression<-glm(recommander.b ~ age+sexe+score.information+amelioration.sante+amelioration.moral+profession+service, data=data, family=binomial("logit"))
summary(regression)
#on constate que les prédicteurs significatifs sont:score information, amelioration sante et amelioration moral
#donc le service sera recommand? selon la qualit? de l'information re?u le premier jour, l'am?lioration sant? et morale

#v?rification des conditions avec 5 ?v?nements par variable explicative,
#on respect


### classification ascendante hiérarchique ####
data<-read.csv2("C:/Users/utilisateur/Downloads/smp1.csv")

var<-c("age","n.enfant","scz.cons","dep.cons","grav.cons", "rs", "ed", "dr")

cha<-hclust(dist(t(scale(data[,var]))),method="ward.D")
#la fonction t(), c'est ?a qui va d?terminer que la classification portera sur des variables et non des sujets.

plot(cha,xlab="", ylab="", main="Classification hi?rarchique")

#matrice de correlation ou les correlations sont symbolisées par des couleurs avec un gradient de noir et de blanc.
obj<-cor(data[,var], use="pairwise.complete.obs")
heatmap (obj, col=gray(seq(1,0,length=16)))

#quiz

alc<-read.csv2("C:/Users/utilisateur/Downloads/alcool.csv")
str(alc)
View(alc)

alc$AGE.b <- ifelse(alc$AGE>50,1,0)

library(Epi)
twoby2(alc$EDVNEG,alc$AGE.b) 
twoby2(1-alc$EDVNEG,1-alc$AGE.b) 

modele<-glm(alc$EDVNEG~alc$SEXE+alc$AGE+alc$SEXE*alc$AGE, data=alc, family="binomial")
summary(modele)


### heatmap ####

#correlation


cormat <- round(cor(data_corr),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


a<-ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
a

ggsave("myplot.png")



### alpha ####

install.packages("ltm")
library(ltm)#alpha

#calculate Cronbach's Alpha
cronbach.alpha(data)


### Omega mcdonald ####

#install.packages(c("psych","GPArotation"),dependencies=TRUE) #required for factor analysis
# library(GPArotation)
# library(lavaan)

# ???
library(psych)#omega
omega(Data_fact_analysis, 2)




### indice de fiabilité inter-juge (kappa) ####

# avoir des colonnes/variables différentes pour chaque codeur: Rater1_Anxiety, Rater2_Anxiety
# kappa must be computed separately for each variable (Hallgreen (2012))
# question sur ce qu'on va faire aprés le Unit

https://www.datanovia.com/en/lessons/inter-rater-reliability-analyses-quick-r-codes/
# Cohen’s Kappa: It can be used for either two nominal or two ordinal variables. 
#It accounts for strict agreements between observers. It is most appropriate for two nominal variables.
# Intraclass correlation coefficient (ICC) for continuous or ordinal data
# Weighted Kappa: It should be considered for two ordinal variables only. It allows partial agreement.

library(irr)
data("data_1", package = "irr")
head(data_1,2)

#Cohen’s Kappa: two raters ; catégoriel
kappa2(data_1[, c("Eval_1_perf", "Eval_2_perf")], weight = "unweighted")

#Intraclass correlation coefficients: continuous scales
#ICC Interpretation : Koo and Li (2016) gives the following suggestion for interpreting ICC (Koo and Li 2016):
#  below 0.50: poor ; between 0.50 and 0.75: moderate ; between 0.75 and 0.90: good ; above 0.90: excellent
#Two way car Un ensemble de k évaluateurs sont choisis au hasard, puis chaque individu est mesuré par le même ensemble de k évaluateurs ayant des caractéristiques similaires. 
# Unit: la fiabilité peut être estimée pour une évaluation unique (single rating) ou pour la moyenne des k évaluation (average k ratings). Le choix entre “single” et “average” dépend de 
#la manière dont le protocole de mesure sera effectué dans l’application réelle (Koo and Li 2016). Par exemple:    Si l’on envisage d’utiliser la valeur moyenne de k évaluateurs comme base 
#de l’évaluation, le plan expérimental de l’étude de fiabilité doit impliquer 3 évaluateurs, et le type “moyenne de k évaluateurs” (average of k raters en anglais) doit être choisi.    
# Inversement, si l’on envisage d’utiliser la mesure d’un seul évaluateur comme base de la mesure réelle, le type “évaluateur unique” (single rater en anglais) doit être considéré même si 
#l’analyse de concordance implique 2 // If the unit of analysis is a mean of several ratings, unit should be changed to "average" (http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/).
icc(
  data_1, model = "twoway", 
  type = "agreement", unit = "single"
)


# Coefficient de Corrélation Intra-classe (autre façon)
library(psych)
ICC(data_1)
#Les lignes du tableau correspondent respectivement aux ICC suivants : ICC1, ICC2, ICC3, ICC1k, ICC2k et ICC3k. Dans notre exemple, nous allons considérer le type ICC2.
#ICC1, ICC2 et ICC3 pour préciser la fiabilité d’une analyse “single rating” (évaluation unique); et
#ICC1k, ICC2K et ICC3K pour concevoir la fiabilité de la moyenne des k évaluateurs.



#Weighed kappa: ordinal scales
kappa2(anxiety[, c("rater1", "rater2")], weight = "equal")

#Light’s kappa: multiple raters
kappam.light(diagnoses[, 1:3])

## Kappa pondéré et non pondéré ##

# Données de démonstration
diagnoses <- as.table(rbind(
  c(7, 1, 2, 3, 0), c(0, 8, 1, 1, 0),
  c(0, 0, 2, 0, 0), c(0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 4)
))
categories <- c("Depression", "Personality Disorder",
                "Schizophrenia", "Neurosis", "Autre")
dimnames(diagnoses) <- list(Doctor1 = categories, Doctor2 = categories)
diagnoses


library("vcd")
# Calculer kapa
res.k <- Kappa(diagnoses) # ASE est l’erreur type asymptotique de la valeur kappa
res.k
# Intervalles de confiance
confint(res.k)





### Multilevel ####
#Field-chapitre 19

library(foreign)
library(car) # to recode variables
library(nlme) # multilevel
library(ggplot2)
library(reshape)
library(Rcmdr)

#Graphique de la relation entre qualité de vie avant et aprés chir pour les deux conditions de chir, séparément pour les 10 cliniques
pgrid <- ggplot(data_1, aes(Base_QoL, Post_QoL)) + ggtitle("Quality of Life Pre-Post Surgery at 10 Clinics")
pgrid + geom_point(aes(colour = Surgery_Text)) + geom_smooth(aes(colour = Surgery_Text), method = "lm", se = F) + facet_wrap(~Clinic, ncol = 5) + labs(x = "Quality of Life (Baseline)", y = "Quality of Life (After Surgery)")

#Run an ANOVA ; for demonstration ; Considérons qu’on s’intéresse à l’effet de la chirurgie sur qualité de vie postopératoire. On peut examiner ça avec une ANOVA
surgeryANOVA<-aov(Post_QoL~Surgery, data = data_1)
summary(surgeryANOVA)

# run the same model but using the linear models command
surgeryLinearModel<-lm(Post_QoL~Surgery, data = data_1)
summary(surgeryLinearModel)


#Run an ANCOVA ; for demonstration
surgeryANCOVA<-aov(Post_QoL~Base_QoL + Surgery, data = data_1)
summary(surgeryANCOVA)
Anova(surgeryANCOVA, type = "III")

# run the same model but using the linear models command
surgeryLinearModel<-lm(Post_QoL~Surgery + Base_QoL, data = data_1)
summary(surgeryLinearModel)




#vérifier variation entre les contextes/niveaux sinon faire simplement ANOVA/régression
# besoin de comparer un modéle de base avec l'intercept et un modéle avec l'intercept qui varie; si différence alors multilevel

#Fit model with intercept only
# 1 is for intercept only ; ML to compare models
interceptOnly <-gls(Post_QoL~1, data = data_1, method = "ML")
summary(interceptOnly)

#Fit model allowing intercepts to vary by clinic
# random = ~1|Clinic for the contextual variable
randomInterceptOnly <-lme(Post_QoL~1, data = data_1, random = ~1|Clinic, method = "ML")
summary(randomInterceptOnly)

###
#possible de comparer les 2 modéles à l'aide des valeurs AIC et BIC (petit BIC meilleur fit) ou avec loglik function
###

logLik(interceptOnly)*-2
logLik(randomInterceptOnly)*-2

# pour comparer les 2 logLik par un test de chi2 (si signif alors multilevel)
anova(interceptOnly, randomInterceptOnly)



#Add surgery as a predictor ; ajout d'un prédicteur avec intercept qui vari selon clinique
randomInterceptSurgery <-lme(Post_QoL~ 1+Surgery, data = data_1, random = ~1|Clinic, method = "ML", na.action = na.exclude)
summary(randomInterceptSurgery)


#Fit effect of surgery and baseline QoL- random intercepts across clinics
# autre prédicteur ajouté
randomInterceptSurgeryQoL <-lme(Post_QoL~Surgery + Base_QoL, data = data_1, random = ~1|Clinic, method = "ML", na.action = na.exclude)
summary(randomInterceptSurgeryQoL)

# ANOVA pour voir si modéle signifi entre eux
# L.ratio : changement de -2LL ; chi2= L.ratio
anova(randomInterceptOnly, randomInterceptSurgery, randomInterceptSurgeryQoL)



#modéle précédent: modéle 2 prédicteurs avec intercept variant selon contexte
#randomInterceptSurgeryQoL <-lme(Post_QoL~ 1+Surgery + Base_QoL, data = data_1, random = ~1|Clinic, method = "ML", na.action = na.exclude)


# modéle permettant à l'effet de la VI de varier selon le contexte 

##Fit effect of surgery and baseline QoL- random intercepts and slopes (for VI) across clinics
# random slope added for VI
addRandomSlope<-lme(Post_QoL~ 1+Surgery + Base_QoL, data = data_1, random = ~Surgery|Clinic, method = "ML")
summary(addRandomSlope)
anova(randomInterceptSurgeryQoL,addRandomSlope)
#Modéle d'intercept et slope, pour relation entre VI et VD (en controlant pour cov/Base_Qol) varie significativement à travers les différentes cliniques 



#interaction term
#ajout d'une variable 'raison de la chirurgie' et ajout interaction avec groupe de chirurgie

##Fit effect of surgery and baseline QoL, Reason and Reason*Surgery Interaction- random slopes and intercepts across clinics

addReason<-lme(Post_QoL~ 1+Surgery + Base_QoL + Reason, data = data_1, random = ~Surgery|Clinic, method = "ML")
#fonction update pour ajout à un moéle précédent: addReason<-update(addRandomSlope, .~. + Reason)
summary(addReason)


finalModel<- update(addReason, .~. + Reason:Surgery)
summary(finalModel)
intervals(finalModel, 0.95)

anova(addRandomSlope, addReason, finalModel)



#décomposition interaction
##Fit effect of surgery and baseline QoL seperately for the two Reason groups.

#identification de 2 jeux de données avec TRUE
physicalSubset<- data_1$Reason==1 
cosmeticSubset<-data_1$Reason==0

#physicalSubset (chirurgie pour raison physique)
physicalModel <- lme(Post_QoL~ 1+Surgery + Base_QoL, data = data_1, random = ~Surgery|Clinic, subset=physicalSubset, method = "ML")
summary(physicalModel)

#cosmeticSubset (chirurgie pour raison cosmetique)
cosmeticModel <- lme(Post_QoL~ 1+Surgery + Base_QoL, data = data_1, random = ~Surgery|Clinic, subset=cosmeticSubset, method = "ML")
summary(cosmeticModel)


#### Growth Model ####

library(foreign)
library(car) # to recode variables
library(nlme) # multilevel
library(ggplot2)
library(reshape2) # melt

#objectif est d'examiner le changement d'une variable à travers le temps
# en faisant correspondre un modéle polynomial avec des données, on cherche à savoir quelle tendance décrit l'évolution d'une variable à travers le temps

# on peut faire correspondre (nb de point dans le temps - 1) degré polynomial


filename_2 <- 
  "C:/Users/utilisateur/Desktop/R/R Data Files Field/Honeymoon Period.dat"

data_2 <- read.delim(filename_2,  header = TRUE)

# besoin d'un format de données en long
data_long<-melt(data_2, id = c("Person", "Gender"), measured = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))
names(data_long)<-c("Person", "Gender", "Time", "Life_Satisfaction")

# besoin de données numérique pour VI
data_long$Time <- with(data_long, ifelse(Time == "Satisfaction_Base", 0, 
                                         ifelse(Time == "Satisfaction_6_Months", 1,
                                                ifelse(Time == "Satisfaction_12_Months", 2,
                                                       ifelse(Time == "Satisfaction_18_Months", 3, NA)))))



#print(data_long)
#data_long.sorted<-data_long[order(data_long$Person),]



#important de voir la covariance car on travaille avec un changement dans le temps
# ici on considére structure de covariance autorégressive de premier ordre (données proche dans le temps sont sensés corrélées plus que les données éloignées entre elles)

# modéle baseline avec intercept seulement
intercept <-gls(Life_Satisfaction~1, data = data_long, method = "ML", na.action = na.exclude)
#summary(intercept)

# modéle avec intercept changeant selon contexte/personne (random = ~1|Person)
randomIntercept <-lme(Life_Satisfaction ~1, data = data_long, random = ~1|Person, method = "ML",  na.action = na.exclude, control = list(opt="optim"))
# control = list(opt="optim") : set the optimizer that is used (si modéle échou à converger alors trouver un autre optimizer) ; possible aussi d'utiliser maxlter pour 
#le nombre d'itération
# control = list (maxlter = 2000, opt="optim")

anova(intercept, randomIntercept)



### intercept
summary(intercept)
summary(randomIntercept)

anova(intercept, randomIntercept)


### Time as a fixed effect (random intercept)
timeRI<-update(randomIntercept, .~. + Time) # RI random intercept
summary(timeRI)



### Time as a random slopes
timeRS<-update(timeRI, random = ~Time|Person) # intercept et effet du temps vari à travers les personnes
summary(timeRS)


### Covariance
# maintenant que nous avons les effets basiques aléatoires, on peut ajouter un terme pour examen de la structure de la covariance des erreurs
# corAR1 : structure de covariance autorégressive de premier ordre, quand même distance entre les mesures de temps
# corCAR1 : quand mesures de temps pas espacé de façon similaire

# idée est que l'erreur d'un temps de mesure n'est pas sensé corrélé avec erreur d'un autre temps de mesure (les données peuvent corrélées mais pas l'erreur)
# on peut assumer que erreur corréle ; les données peuvent être prédites à partir de données du temps précédent mais aussi des erreurs des temps précédents 
#alors : moving average (MA) model

# Un modéle peut combiner autoregression et average : ARMA
# corARMA

ARModel<-update(timeRS, correlation = corAR1(0,form = ~ Time|Person))

#comparaison des modéles
anova(intercept, randomIntercept, timeRI, timeRS, ARModel)

# final model
summary(ARModel)
intervals(ARModel)

# effet du temps, value = -0.87 signifie que VD change au cours du temps


### higher-order polynomials (corrélés)
#quadratic, cubique mais avec des prédicteurs corrélés sinon utiliser poly()

timeQuadratic<-update(ARModel, .~. + I(Time^2))
summary(timeQuadratic)

timeCubic <-update(timeQuadratic, .~. + I(Time^3))
summary(timeCubic)
#timeCubic <-update(ARModel, .~. + I(Time^3))
#summary(timeCubic)


anova(ARModel, timeQuadratic, timeCubic)
summary(timeCubic)
intervals(timeCubic)



### higher-order polynomials (non corrélés)

polyModel<-update(ARModel, .~ poly(Time, 3))
summary(polyModel)
intervals(polyModel)






#### Latent model ####

# tidyLPA
# https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html

# site web
# https://willhipson.netlify.app/post/latent-profile/latent-profile/

# youtube
# https://www.youtube.com/watch?v=17JRfEtte58
# https://www.youtube.com/watch?v=Dkii5Wc8g9Q




### interpolation ####

# https://statisticsglobe.com/approx-approxfun-interpolation-functions-r

x1 <- c(0, 5)                        # Create first vector
x1                                   # Print first vector

y1 <- c(0, 10)                       # Create second vector
y1                                   # Print second vector

data_approx1 <- approx(x1, y1)       # Apply approx function
data_approx1                         # Return output of approx function


plot(data_approx1$x,                 # Draw output of approx function
     data_approx1$y)
points(x1, y1,
       col = "red",
       pch = 16)



#### lecture PDF ####
# https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
# permet de connaitre la liste des mots pr?sents un nb de fois




## EQS ####
# livre de Kline 2016 : page 104 ; package REQS pour lire syntaxe et données logiciel EQS et importer résultats dans R aprés calcul dans EQS

# Lavaan package pour EQS
#info dans resum et al



# https://stats.oarc.ucla.edu/r/seminars/rsem/#s2h
# http://rstudio-pubs-static.s3.amazonaws.com/11011_caa8639a12cb488aba3df58c696626bb.html







## Power Analysis for Structural Equation Models: semPower Manual

# cite as : Moshagen, M., & Erdfelder, E. (2016). A new strategy for testing structural equation models. 
#Structural Equation Modeling, 23, 54–60. https://doi.org/10.1080/10705511.2014.950896

#devtools::install_github("moshagen/semPower")

library(semPower)


# df: NRE-S 
# (nb de variables=k ; NRE=k(k+1) / 2 ) ; S = 2K +R ; R: nb lien estimé


# semPower.aPriori: determine required sample size (N), given alpha, beta or power, effect, and df 
# semPower.postHoc: determine achieved power (and beta), given alpha, N, effect, and df 
# semPower.compromise: determine alpha and beta, given the alpha/beta ratio, N, effect, and df

ap <- semPower.aPriori(effect = .05, effect.measure = 'RMSEA', alpha = .05, power = .80, df = 100)
summary(ap)
# results show that a sample size of N = 164 yields a power of approximately 80% to reject a wrong model (with df = 100) 
#with an amount of misspecification corresponding to RMSEA = .05 on alpha = .05.


# A-Priori avec nb de variable obs p = 

ap2 <- semPower.aPriori(effect = .05, effect.measure = 'RMSEA', alpha = .05, power = .80, df = 100, p = 2)
summary(ap2)


# Power Plots

semPower.powerPlot.byN(effect = .05, effect.measure = 'RMSEA', alpha = .05, df = 100, power.min = .05, power.max = .99)


# Covariance Matrix input
# the effect can also be determined by specifying the population (Σ) and the model-implied (ˆΣ) covariance matrices directly

semPower.aPriori(alpha = .05, power = .80, df = 100, Sigma = Sigma, SigmaHat = SigmaHat)
semPower.postHoc(alpha = .05, N = 1000, df = 100, Sigma = Sigma, SigmaHat = SigmaHat)
semPower.compromise(abratio = 1, N = 1000, df = 100, Sigma = Sigma, SigmaHat = SigmaHat)
semPower.powerPlot.byN(alpha = .05, df = 100, power.min = .05, power.max = .99, Sigma = Sigma, SigmaHat = SigmaHat)

# This feature is particularly useful when used in conjunction with some other SEM software, which is used to generate the 
# population and the model-implied covariance matrix (note that a proper definition of the latter usually requires fitting the 
#model to the population data).





#### site web ####

# aide: https://audhalbritter.com/wp-content/uploads/2016/12/Github-%E2%80%93-R-studio-Cheat-Sheet.pdf


#install.packages("blogdown")
library(blogdown)
#blogdown::install_hugo() # besoin de hugo
blogdown::hugo_version()

setwd("C:/Users/utilisateur/Desktop/R/R website/website_3")

blogdown::new_site() # création nouveau site
blogdown::new_post()

blogdown::serve_site()


#voir html sur mon ordi dans chunk
htmltools::includeHTML("size-incident-comparison.html")


# voir html en ligne
https://www.finex.co/how-to-display-html-in-github/
  
https://alexandrewilliot.github.io/website_with_Rmd/index.html

#### image dans Rmd html ####

library(knitr) 
library(ggplot2)
library(png) 

library(imager)
myimg <- load.image("C:/Users/utilisateur/Desktop/R/Helen project/indicents.png")
plot(myimg)

#hors chunk
![txt] (https://www.dropbox.com/s/3ns3you6l5xz1tv/indicents.png?dl=0)

#### arbre généalo ####
#https://stackoverflow.com/questions/11280343/how-to-plot-family-tree-in-r

# Tape network analysis tutorial on internet
# https://ggraph.data-imaginist.com/articles/Edges.html

