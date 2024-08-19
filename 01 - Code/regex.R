# Supprimmer les cffires
gsub("[0-9]+", "", my_string)


# Nombre de fois qu'un caractere apparait dans une chaine de caracteres
seq <- "ABABBCBD"
vi <- rep(NA, nchar(seq))
for (i in 1:nchar(seq)) {
  vi[i] <- length(lapply(gregexpr(substr(seq, i, i), seq), attributes)[[1]]$match.length)
}
vi


# Tous les caractères AVANT l'occurence du premier espace
sub("\\ .*", "", x)
# Tous les caractères APRES l'occurence du premier espace
sub("^[^_]* ", "", x)


# Detection d'un patterne avec un chiffre variable : \\d
str_detect(colnames(data), "C\\d_ON")
# Avec un chiffre ou un nombre : \\d+
str_detect(colnames(data), "C\\d+_ON")
