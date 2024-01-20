# Packages de gestion de valeurs manquantes
library(naniar)
library(mice)

# Pourcentage de complete case
pct_complete_case()
pct_miss_case()
# Meme chose en colonnes
pct_complete_var()

# Test valeurs manquantes MAR vs MCAR
mcar_test()

# Imputation multiple
mice::mice()