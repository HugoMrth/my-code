library(dplyr)

system.time({
  for (i in 1:10000) {
    A <- iris %>%
      dplyr::select(Species)
  }
})

system.time({
  for (i in 1:10000) {
    A <- iris[, "Species"]
  }
})

# Init
n_iter1 <- 50
n_iter2 <- 50
t.ratio <- numeric(n_iter1)

for (j in 1:n_iter1) {
  t.dplyr <- system.time({
    for (i in 1:n_iter2) {
      A <- iris %>%
        dplyr::select(Species)
    }
  })
  
  t.natif <- system.time({
    for (i in 1:n_iter2) {
      A <- iris[, "Species"]
    }
  })
  
  # Ratio des deux temps
  t.ratio[j] <- t.dplyr[3] / t.natif[3]
}

t.ratio
mean(t.ratio)