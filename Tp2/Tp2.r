
########################     Règles d'association       ###############################
#####                        Chargement et préparation des données      #####
#####                        Statistiques descriptives             #####
#####                        Règles d’association                  #####

##### 

rm(list = ls(all = TRUE))

##### Les Librairies necessaire

library("readr")
library("stringr")
library("plyr")
library("arules")
library("arulesViz")
library("plotly")
require("stringr")

##### Chargement de la base de données store 

# la base de données store contient la liste des produit que les clients achetes ensembles
store <- read.csv("data/store_data.csv", header = TRUE)
head(store)
dim(store)
names(store)
ncol(store)


#############    les règles d’association avec l’algorithme apriori
# Tout d'abord on commence par la recherche et suppresion des virgules
# qui vont causer des problèmes au niveau de la conversion des données
# au format basket.
count <- 0
for (val in 1:ncol(store)) {
  if (str_count(store[[val]], ',') != 0) count = count + 1
}
print(count) ##### ICi on a count = 0 , donc on a aucune , qui peut causer des problème

########## Statistiques descriptives
# Elle permet de bien comprendre les données et de voir les valeurs ou les colonnes a eliminer.
# cette etape est interessant dans les etapes avenir.Avec le dataset store on peut passer directement 
# a la transformation des données en <<basket>>

########## Traitememt des données avant la transformation      #########


################## Transformation des données en données <<basket>>
# Pour pouvoir utiliser la fonction arules qui permet de calculer les règles d’association, nous devons 
# mettre nos données au format transactions. Et l’étape préliminaire pour le faire et de transformer nos 
# données en données « basket » ou « panier ».

##### On transforme donc nos données et on exporte le résultat dans un fichier csv

store_itemList <- ddply(store, c("shrimp"), function(df1) paste(df1$almonds, collapse = ","))
head(store_itemList, 1)
store_itemList <- store_itemList$V1

write.csv(store_itemList, "ItemList.csv", quote = FALSE, row.names = TRUE)
head(store_itemList)

txn = read.transactions(file = "ItemList.csv", rm.duplicates = TRUE, format = "basket", sep = ",", cols = 1);
test <- head(txn, 2)
test
dim(test)
dim(txn)
rules <- apriori(txn, parameter = list(sup = 0.11, conf = 0.8, target = "rules"));
head(rules, 1)
inspect(rules)
class(rules)
inspect(head(rules, 5))

################  Description des regles obtenu avec l'exemple de la regle 11
# Les clients qui on acheter "ground beef" ont egalement acheter "spaghetti".On va le support ,
# la confiance et le lift pour evaluer la qualite de la regle.
##### Support : le support represente la fiabilité de la regle.Il permet de
#####           de fixer un seuil en dessous duquel les regles ne sont pas considerées comme fiable.
#####           par exemple avec la regle 11 on a 14 client qui ont acheter "ground beef" et "spaghetti"
#####           Avec ce nombre  cette regle est fiable.
##### La confianve : represente la precision de la regle.Une meilleur regle d'association a une confiance élévée.
##### Le lift : caractérise l'intéret de la règle.Plus le lift est supperieur a 1 plus il exite des lien
#####           entre les elements.Une règle ne sert absolument à rien qi elle  a un lift égal  à un. 

#####  Statistique descriptive des règles obtenu AVANT élimination des règles pas interessante.

summary(rules)

##### Elimination des règles incorrectes ou pas interessante.

# L'incovénients de la mesure de confiance est qu'elle tend à deformer l'importance d'une association.Une règle peut avoir
# une confiance élévée mais une frequence  une apparision frequente dans les transactions peut signifier que l'association
# pourrait etre simplement un hasard.Aussi plus la valeur de lift est superieur a 1 plus il existe des lien 
# entre les éléments.Dans notre cas lift minimum est de 1.751.pour garder les regles vraiment trés interessante on peut 
# eliminer toute les regles qui ont un lift inferieur a 2 .

filtered_rules <- subset(rules, subset = lift > 2)
inspect(head(filtered_rules, 5))
#####  Statistique descriptive des règles obtenu apres Elimination des regles pas interessante

summary(filtered_rules)
# maintenant le lift min est 2.014

##### Elégation des regles d'association 

rules_lift <- sort(rules, by = 'lift')
rules_pruned <- rules_lift[!is.redundant(rules_lift, measure = "lift")]
inspect(head(rules_pruned, 5))

# On peut voir avec la premiere régle que les clients qui ont acheté {ground beef, olive oil} ont egalement acheter du {pepper}
# ce qui logique pour faire des boulettes ou des sandwich.

#### Representation par graphe

plot(rules_pruned[1:10], method = "graph", engine = "interactive")
plot(rules_pruned[1:10], method = "graph", control = list(type = "items"), cex = 0.7)

# Le graphe montre une regle tres interessante avec un support et un lift éléve.
# Les clients qui achete des oeufs on plus de chance d'acheter du poivre que de 
# l'eau minerale.On remarque aussi deux groupes de produit ont tendance à amener 
# les client à acheter du poivre comme {eggs,spaghetti,olive oil} et {ground beef et olive oil}.
# Aussi la règle " {ground beef,olive oil} => {pepper} " a plus de chance de se produire
#### Répresentation type scatter plot


plot(rules_pruned, measure = c("support", "confidence"), shading = "lift")
rules