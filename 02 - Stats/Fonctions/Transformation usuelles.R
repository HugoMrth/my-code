# data <- data.frame(
#   Y_logits = Y_logits,
#   X_duree_rea = X_duree_rea,
#   X_nb_blocs = X_nb_blocs
# )

# A CHANGER
path <- "C:/Users/h.marthinet/Documents/34 - R/Codes & Scripts/Rdata/"


path <- paste0(path, "Tests transformation usuelles.Rdata")
load(path)

library(ggplot2)
ggplot(data, aes(Y_logits, X_duree_rea)) +
  geom_point() + geom_smooth(method = "loess")

ggplot(data, aes(Y_logits, X_nb_blocs)) +
  geom_point() + geom_smooth(method = "loess")


library(bestNormalize)
bestNormalize(data$X_duree_rea)
bestNormalize(data$X_nb_blocs)