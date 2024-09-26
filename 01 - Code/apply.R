# Conversion de toutes les colonnes integer en numerique
data[, unlist(lapply(data, is.integer))] <- apply(data[, unlist(lapply(data, is.integer))], 2, as.character)

# element wise max of two vector 
pmax(x, y)
