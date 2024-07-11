# Supprimmer les cffires
gsub("[0-9]+", "", my_string)


# Nombre de fois qu'un caractere apparit dansn une chaine de caracteres
seq <- "ABABBCBD"
vi <- rep(NA, nchar(seq))
for (i in 1:nchar(seq)) {
  vi[i] <- length(lapply(gregexpr(substr(seq, i, i), seq), attributes)[[1]]$match.length)
}
vi
