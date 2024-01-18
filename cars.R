#TP noté

#1) Charger le dataset

mtcars

#2) Consulter la documentation de ce dataset pour comprendre sa structure.

?mtcars

#3) Afficher les noms des variables considérées. Y a-t-il des variables
#qualitatives dans ce jeu de données ?
  

summary(mtcars)

#Il n'y a pas de variable qualitative dans ce jeu de données.

#4) Afficher les 6 premières lignes de ce jeu de données

head(mtcars)

#5) Afficher le nombre de lignes et de colonnes.

nrow(mtcars)
ncol(mtcars)

#6) Afficher les mesures statistiques de base à l’aide de la commande
#summary.

summary(mtcars)

#7) Quel est le nombre minimal de cylindres qu’on peut trouver dans une
#voiture de l’échantillon étudié
attach(mtcars)
min(cyl)

#8) Quelle est la valeur médiane du poids en lbs

median(wt)

#9) Quel est le nombre moyen de carburateurs

mean(carb)

#10)Extraire de la base :
#(a) la 5-ème ligne,

mtcars[5,]

#(b) la 5-ème colonne

mtcars[,5]

#(c) les lignes 1, 6 et 10 avec une seule commande

mtcars[c(1,6,10),]

#(e) tout sauf les deux dernières colonnes

length(mtcars)
mtcars[,1:9]


#(f) tous les enregistrements ayant un nombre de carburateur >1.

library(dplyr)

mtcars %>% filter(carb > 1)

#11) Y’a-t-il des valeurs manquantes dans ce jeu de données ? Donner
#l’instruction qui permet de créer un nouveau jeu de données
#correspondant à la collection mtcars privée des valeurs manquantes

sum(is.na(mtcars))

sapply(mtcars, gsub(na))

library(tidyr)

mtcars %>% drop_na()

#12) On propose d’étudier la consommation du carburant exprimée par le
#nombre milles par gallon, représentée par la variable mpg en fonction de
#la puissance brute, représentée par la variable hp. Représenter le nuage de
#point correspondant à l’aide du package ggplot 2. Interpréter

library(ggplot2)

plot(hp, mpg, las = 1,
     xlab = "hp", ylab = "mpg")

#Il semble en effet il y avoir une corrélation  négative entre les deux variables

#13) Peut-on établir un modèle linéaire pour représenter la relation entre les
#variables mpg et hp. Si oui, ajouter la droite de régression linéaire.
#Interpréter (Est-ce qu’il y a une corrélation forte entre les données ?)

model = lm(mpg~hp)
model
plot(hp, mpg, las = 1,
     xlab = "hp", ylab = "mpg")
reg = model
abline(reg,col="blue")

#Les valeurs n'étant pas très éloignées de la droite, l'on peut affirmer qu'il existe une corrélation forte entre les données

#{14) Donner l’équation de la droite avec les valeurs estimées des coefficients
#inconnus (à l’aide de la méthode MCO).}

summary(model)

bêta = cov(hp,mpg)/var(hp)             #Estimate|hp

alpha = mean(mpg) - bêta*mean(hp)      #Estimate|(Intercept)

#Yi = alpha + bêta*xi = droite de régression

#15)Evaluer la qualité du modèle à partir du graphique.

#Le modèle semble cohérent

#16) Déterminer la covariance entre les variables mpg et hp, Interpréter.

cov(hp,mpg)

#17) Déterminer le coefficient de corrélation. Interpréter

#la covariance est négative comme le coefficient directeur de la droite => l'une des variables hp baisse lorsque mpg monte

#18) Déterminer le coefficient de détermination. Interpréter

summary(model)

#0.5892 => 60% des données peuvent être prédient par notre modèle

#19) Afficher les mesures statistiques pour le modèle linéaire obtenu.

summary(model)

#20) Analyser le modèle linéaire sur le plan inférentiel en interprétant la valeur
#de p-value.

# p-value = 1.788e-07 => Ho peut être rejetée
