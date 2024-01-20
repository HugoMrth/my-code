library(stats)




# Vecteur des p_valeurs à ajuster
# La méthode du package stats ajuste les p-valeurs plutôt que le alpha
# L'interprétation de la significativité à 5% reste donc la même
pvals <- vecteurs_pvals_tests




#Correction de Bonferroni
pvals_ajust <- round(p.adjust(pvals, method = "bonferroni"), 5)

#Correction de Benjamini Hochberg
pvals_ajust <- round(p.adjust(pvals, method = "BH"), 5)
