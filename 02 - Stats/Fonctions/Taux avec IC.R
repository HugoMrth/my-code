X <- cbind(c(78, 125, 24, 186, 32, 65, 85, 198, 12), 
           rep(315248, 9))
X

library(epiR)

epiR::epi.conf(X, 
               ctype = "inc.rate",  
               method = "exact") * 100000 



library(epitools)

epitools::pois.exact(X[, 1], X[, 2]) * 100000



