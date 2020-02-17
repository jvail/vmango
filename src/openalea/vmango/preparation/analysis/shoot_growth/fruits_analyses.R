#### Scripts pour récupérer les données concernant les fruits (poids, sucre, acidité,...)
#### fruits d'une même branche fruitière identiques (à adapter dans le cas contraire)
setwd("C:/Users/persello/Documents/Develop/mangosim/src/vplants/mangosim/shoot_growth/fruitmodel-output-cycle-3-fdist-4")
# cycle et fdist à modifier suivant le cycle et le nombre d'UCs par branche fruitière qu'on souhaite

library(Rwinsteps)  # permet d'utiliser list.files
library(dplyr)      #                   group_by et summarise

################################# récupération des données ####################################################
## détermine les chemins de tous les csv à étudier ##
filenames <- list.files(path = "C:/Users/persello/Documents/Develop/mangosim/src/vplants/mangosim/shoot_growth/fruitmodel-output-cycle-3-fdist-4", pattern = "meanfruit")
filepath <- "C:/Users/persello/Documents/Develop/mangosim/src/vplants/mangosim/shoot_growth/fruitmodel-output-cycle-3-fdist-4/"
all_names <- paste(filepath, filenames, sep = "")

## initiation de la boucle ##
#lis le premier csv
tables = read.table(all_names[1], sep=",", dec=".", header=T)
#rajoute une colonne n° branche fruitière
tables = cbind(branche_fruitiere = rep(1, nrow(tables)), tables)

## concatène tous les csv ##
for (i in 2:length(all_names)) 
{
table = read.table(all_names[i], sep=",", dec=".", header=T )
table$branche_fruitiere = rep(i, nrow(table))
tables = merge(tables, table, all = TRUE)
}

#View(tables)

#################################### analyses des données ######################################################
## calcule le poids frais, la concentration en sucres solubles et l'acidité des fruits par branche fruitière ##
# (les fruits d'une même branche fruitière ont les mêmes caractéristiques)
by_branche <- group_by(tables, branche_fruitiere)
resum_result <- summarise(by_branche, max_X = max(X)-1,poids_frais_grammes = max(Masse_Fruit), sucres_solubles = sucres_solubles[max(X)-1], acides_organiques = acides_organiques[max(X)-1])
# transforme le tableau (variable locale) en data frame
df_resum_result <- as.data.frame(resum_result)

## calcule le rendement ##
fruitstructure = read.csv("fruitstructure.csv", sep="\t", dec=".", header = F)
# supprime la ligne des noms (plus de colonnes que de noms de colonnes)
# dans l'ordre: nb inflos, nb fruits, nom du fichier, UCs qui ont fructifié (Id) et nb de fruits/inflo
fruitstructure = fruitstructure[-1,]
# récupère le nombre de fruits par branche fruitière
nb_fruits = as.numeric(fruitstructure[,2])
# récupère le nombre de fruits d'inflorescences par branche fruitière
nb_inflo = as.numeric(fruitstructure[,1])
# rajoute la colonne rendement, le nombre de fruits et le nombre d'inflos par branche fruitière
resultats = cbind(rdt_branche_grammes = df_resum_result$poids_frais*nb_fruits, ratio_sucre_acide = df_resum_result$sucres_solubles/df_resum_result$acides_organiques, nb_fruits = nb_fruits, nb_inflo = nb_inflo, df_resum_result)
# change l'ordre des colonnes
resultats = resultats[,c("branche_fruitiere","nb_inflo","nb_fruits","poids_frais_grammes","rdt_branche_grammes","sucres_solubles", "acides_organiques","ratio_sucre_acide")]
# fait la somme de chaque colonne
rdt_cycle <- apply(resultats, 2, sum) # 2 pour colonne
# rajoute le rendement total,  au tableau
resultats = cbind(resultats, rendement_total_kg = rep(NA, nrow(resultats)), ratio_moyen = rep(NA, nrow(resultats)))
resultats$ratio_sucre_acide = resultats$sucres_solubles/resultats$acides_organiques
resultats$rendement_total_kg[1] = rdt_cycle[5]/1000
resultats$ratio_moyen[1] = mean(resultats$ratio_sucre_acide)

write.csv(resultats, file="data_analyses.csv")

