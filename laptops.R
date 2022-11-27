#Exercice 1 :

#1) Télécharger le fichier de données laptops depuis Classroom
#2) Charger le jeu de données
?readr
library(readr)
laptops <- read.csv('Book & Cie/CodingTime/R/Noa Ottman/laptops.csv',sep=';')

#3) Installer les packages suivants :
#• library(car)
#• library(ggplot2)
#• library(lm.beta)
#À quoi servent ces packages ?

a = c("car","ggplot2","lm.beta")

install.packages(a)

library(car)
library(ggplot2)
library(lm.beta)
library(dplyr)

?car
?ggplot2
?lm.beta

#4) Visualiser le contenu d'un tableau de données à l'aide de la fonction View.

View(laptops)
#Quelles sont les variables dans ce jeu de données ?

answer = colnames(laptops)
answer

#5) Afficher les 6 premières lignes de l'objet "laptops"

head(laptops)

#6) Attribuer aux colonnes du dataset les noms suivants : 
#"Prix", "Taille","RAM", "Disque", "Ports", "Marque", "Poids"

laptops2 = laptops

b= c("Prix", "Taille","RAM", "Disque", "Ports", "Marque", "Poids")
colnames(laptops2) = b

View(laptops2)

#7) Afficher les mesures statistiques de base pour le dataset à l’aide de la
#commande summary.

summary(laptops2)

#8) Quel est le prix maximal et minimal d’un laptop en $
attach(laptops2)
max(Prix)

#9) Quel est le nombre minimal de ports qu’on peut trouver dans un laptop de
#l’échantillon étudié

min(Ports)

#10) Quelle est la taille moyenne d’un laptop

?gsub
colnames(laptops[2])
Taille = as.numeric(gsub(",", ".", Taille)) #substitution des "," en "." pour 
#convertir char class en num
View(laptops$Taille)
sapply(laptops2, mode) #classe des variable de laptops2

laptops2

mean(Taille)

#11) Quelle est la valeur médiane du poids en onces

Poids = as.numeric(gsub(",",".",Poids))
median(Poids)

#12) Quels sont les caractéristiques de la marque (longueur, classe, mode) ?

summary(Marque)

#13) On propose d’expliquer le Prix ($) en fonction de la taille du disque (Giga).
#Afficher le nuage de point à l’aide de ggplot2. Que remarquez-vous ?


#Nous allons tracer un nuage de points dont les couleurs varient en fonction de la marque pour vérifier 
#si la dépendance des deux variables l'une de l'autre est vraiment inenvisageable.

library(RColorBrewer)
myColors <- brewer.pal(5,"Set2") 
names(myColors) <- levels(Marque)
colScale <- scale_colour_manual(name = "Marque",values = myColors)


ggplot(laptops2,aes(Disque, Prix,colour = Marque)) + geom_point() +
colScale

#Bien que l'ensemble des points soit éparpillé, les variables Prix et Disque
#semblent suivre une corrélation positive, de plus , les variables d'une même marque semblent  avoir une relation plus forte, 
#il existe donc une composante d'échelle dans la relation observée.


#14) Peut-on établir un modèle linéaire pour représenter la relation entre les
#variables Prix et Disque. Si oui, ajouter la droite de régression linéaire.

model = lm(Prix~Disque)
model
plot(Disque, Prix, las = 1,
xlab = "Mémoire morte en giga", ylab = "Prix en dollars")
reg = model
abline(reg,col="green")

#La droite est centrée, elle semble répartir les données en deux.


#15)Evaluer la qualité du modèle à partir du graphique.

#La droite semble cohérante.

#16) Déterminer la covariance entre les variables Prix et Disque. Interpréter.

cov(Disque, Prix)

#31090.16 => relation positive

#17) Déterminer le coefficient de corrélation. Interpréter

cor(Prix, Disque, method = "pearson")
# r = 0.2627363
cor(Prix, Disque, method = "spearman")
#rs = 0.3230357

# rs > r => variables expriment une relation monotone non-linéaire

cor.test(Prix, Disque, method= "spearman")

# rs > p-value = 0.009814 

filter = dplyr::filter #s'assurer que la fonction filter utilisée soit bien celle de dplyr

#ou
#detach("package:dplyr") #get rid of packages conflicts
#library(dplyr)

cor(laptops2 %>% select(Prix) %>% filter(Marque == "HP"), laptops2 %>% select(Disque) %>% filter(Marque == "HP"), method = "spearman")

#HP r = 0.6568518

cor(laptops2 %>% select(Prix) %>% filter(Marque == "Toshiba"), laptops2 %>% select(Disque) %>% filter(Marque == "Toshiba"), method = "spearman")

#Toshiba r = Prix 0.5359697

cor(laptops2 %>% select(Prix) %>% filter(Marque == "Apple"), laptops2 %>% select(Disque) %>% filter(Marque == "Apple"), method = "spearman")

#Apple r = Prix 0.7129935

#18) Déterminer le coefficient de détermination. Interpréter


cor(Prix, Disque, method = "pearson")**2

#R2 = 0.06903039 =>la force d’adéquation entre le modèle de cette régression et les données collectées
#assez faible 

#19) Afficher les mesures statistiques pour le modèle linéaire obtenu.

summary(model)


#20) Analyser le modèle linéaire sur le plan inférentiel en interprétant la valeur
#de p-value.

#alpha = 0.5824 , p-value = 0.0375 
#alpha > p-value => Prix dépend probablement de Disque

#21) On propose de prédire le Prix en fonction de Disque, Taille, Ports, Poids et
#RAM)

model2 =  lm(formula = Prix~Disque + Taille + Ports + Poids + RAM)
model2
plot(Disque, Prix, las = 1,
     xlab = "Disque, Taille, Ports, Poids, RAM", ylab = "Prix en dollars")
reg2 = model2
abline(reg2,col="orange")

#22) Afficher les mesures statistiques de base pour le modèle à l’aide de la
#commande summary.

summary(model2)

#la p-value est toujours inférieur à la valeur absolue des coefficients de pente
#Donc les variables sont toutes liées.

#l'écart type le moins prononcé est celui de Disque


#23) Déterminer la valeur de VIF (variance d’inflation factor) à l’aide de la
#fonction vif afin de tester la multi-colinéarité.

vif(model2)

#24) Déterminer la matrice de corrélation du dataset privé de la variable
#qualitative (marque3). Vérifier la corrélation entre Poids et taille
NoApple = laptops2 %>% select(lapt) %>% filter(Marque != Marque[3])
NoApple
NoBrand = NoApple %>% select("Prix", "Taille","RAM", "Disque", "Ports", "Poids") 
M = cor(NoBrand)
M

#La corrélation entre Taille et Poids est parfaite.

#25) On propose de corriger le modèle en ignorant la variable poids.
#Etablir le nouveau modèle linéaire (model4). Est-ce le modèle a été
#amélioré ?

model4 = lm(formula = Prix~Disque + Taille + Ports + RAM)

plot(Disque, Prix, las = 1,
     xlab = "Disque, Taille, Ports, RAM", ylab = "Prix en dollars")
reg4 = model4
abline(reg4,col="purple")

summary(model4)

#Oui la p-value est passée de 0.1998 à 0.1319


#26) On propose de corriger le modèle en ignorant la variable RAM.
#Etablir le nouveau modèle linéaire (model5). Est-ce le modèle a été
#amélioré ?

model5 = lm(formula = Prix~Disque + Taille + Ports)

plot(Disque, Prix, las = 1,
     xlab = "Disque, Taille, Ports", ylab = "Prix en dollars")
reg5 = model5
abline(reg5,col="blue")

summary(model5)

#Oui la p-value est passée de  0.1319
# à 0.0695

#27) Déterminer le VIF du modèle 5. Interpréter

vif(model5)
# Les Variables Disque Taille et Ports font
#respectivement varier respectivemetn l'écart type d'erreur de 1.587791 1.897265 et 1.837158

#28) Déterminer les coefficients centrés réduits (les β) à l’aide de la fonction
#lm.beta

?lm.beta

lm.beta(model5)

#29) Utiliser des intervalles de confiance à l’aide de la fonction confint pour
#voir pour chaque β, Interpréter."

confint(model5)
