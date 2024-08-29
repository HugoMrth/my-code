library(dplyr)

# id est l'identifiant des individus
# X la variable temporelle
# Y la variablke sur laquelle on effectue le spread


DATA_spread <- DATA %>%
  dplyr::select(id, X, Y) %>%
  group_by(id, X) %>%
  spread(X, Y) %>%
  data.frame
