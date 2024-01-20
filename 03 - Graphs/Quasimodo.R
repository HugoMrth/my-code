
Var <- "Muscle"
EFF <- data.frame(
  Var = as.character(unique(data[, Var])),
  Effectifs = rpois(length(unique(data[, Var])), 8)^2
)
data$Effectif <- NA
for (i in 1:nrow(EFF)){
  data$Effectif[data[, Var] == EFF$Var[i]] <- EFF$Effectifs[i]
}

data$Effectif <- data$Effectif

bdm <- sf_polygon(data,
                        x = "x",
                        y = "y",
                        polygon_id = "Id",
                        keep = TRUE)
bdm_w <- cartogram_cont(bdm, weight = "Effectif", itermax = 5)


# ggplot(data = data, aes(x, -y, group = Id)) + geom_bdgramr()
# par(mfrow = c(3, 3))
# mf_map(
#   x = bdm,
#   type = "choro",
#   var = "Effectif",
#   pal = rev(c("#BF0416", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5")),
#   breaks = seq(0, max(data$Effectif)*1.05, length.out = 9),
#   border = "#6b4266",
#   lwd = 0.5
# )
mf_map(
  x = bdm_w,
  type = "choro",
  var = "Effectif",
  #pal = "white",
  pal = rev(c("#BF0416", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5")),
  breaks = seq(0, max(data$Effectif)*1.05, length.out = 9),
  border = "#6b4266",
  lwd = 0.5,
  leg_pos = NA
)



