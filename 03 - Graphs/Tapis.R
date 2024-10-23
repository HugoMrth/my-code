#### TAPIS ####
dataTapis <- apply(data[, c("date_index", "date_fin_1st_TREF", "date_1st_reprise_Tref",
                                      "date_deb_1st_tswi_niv1", "date_deb_1st_tswi_niv2",
                                      "date_dc", "date_fin_suivi")], 2, 
                   function(x) {as.Date(x, format = "%Y-%m-%d")})
dataTapis <- apply(dataTapis, 2, function(x) {
  floor((x - dataTapis[, "date_index"]) / 7)
})
dataTapis[, "date_index"] <- 0

dataTapis <- dataTapis %>%
  as.data.frame() %>%
  group_by(date_index, date_fin_1st_TREF, date_1st_reprise_Tref, date_deb_1st_tswi_niv1, date_deb_1st_tswi_niv2, date_dc, date_fin_suivi) %>%
  summarise(n = n())


dataTapisSeq <- t(apply(dataTapis, 1, function(x) {
  #x <- dataTapis[1,]
  y <- x[8]
  x <- as.numeric(x)[-8]
  names(x) <- colnames(dataTapis)[-8]
  x <- diff(sort(x[!is.na(x)]))
  as.character(c(y,
    rep(c("date_index", names(x)[1:(length(x)-1)]), x),
    rep("date_dc", 52-sum(x))))
}))

dataTapisSeq[dataTapisSeq == "date_1st_reprise_Tref"] <- "TREF"
dataTapisSeq[dataTapisSeq == "date_dc"] <- "DC"
dataTapisSeq[dataTapisSeq == "date_deb_1st_tswi_niv1"] <- "TSWI"
dataTapisSeq[dataTapisSeq == "date_fin_1st_TREF"] <- "PSTT"
dataTapisSeq[dataTapisSeq == "date_deb_1st_tswi_niv2"] <- "TSWI"
dataTapisSeq[dataTapisSeq == "date_index"] <- "TREF"

rm(list = c("dataTapis"))

seq <- seqdef(data = dataTapisSeq[, 2:53],
              weights = as.numeric(dataTapisSeq[, 1]))
seqdplot(seq, border = NA,
         cpal = c("lightgray", "lightcoral" ,"cornflowerblue", "mediumseagreen"))
















#### Double Tapis + classification ####
#### Import & Libs ####

rm(list = ls())

library(plotly)
library(stringr)
library(TraMineR)
library(seqhandbook)
library(plotly)
library(dplyr)
library(stringr)
library(TraMineR)
library(seqhandbook)
library(tidyr)
library(ggseqplot)
library(ggplot2)
library(colorspace)
library(ggh4x)
library(patchwork)
library(ggpubr)
library(cluster)
library(factoextra)


plotSankey <- function(Nodes, Links) {
  # Adding transparency to the links
  # Conversion to rgba character string
  Links$color <- apply(grDevices::col2rgb(Links$color), 2, function(x) {
    paste0("rgba(", x[1], ",", x[2], ",", x[3], ",0.4)")
  })
  
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    alpha_stroke = 0.2,
    node = list(
      label = Nodes$label,
      color = Nodes$color,
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = Links$source,
      target = Links$target,
      value =  Links$value,
      color = Links$color
    )
  )
  fig <- fig %>% layout(font = list(size = 14, color = "black", weight = "bold"))
  fig
}






# plotSankey <- function(Nodes, Links) {
#   Links$source <- Links$source[c(1, 2, 3, 4,
#                                  
#                                  5, 6, 7, 8,
#                                  13, 14, 15, 16,
#                                  9, 10, 11, 12,
#                                  17, 18, 19, 20,
#                                  
#                                  21, 22, 23, 24,
#                                  29, 30, 31, 32,
#                                  25, 26, 27, 28,
#                                  33, 34, 35, 36,
#                                  
#                                  37, 38, 39, 40,
#                                  45, 46, 47, 48,
#                                  41, 42 ,43 ,44,
#                                  49, 50, 51, 52)]
#   
#   
#   Nodes$label[Nodes$label == "Traitement de référence"] <- "TREF"
#   if (ATC_name == "C03EA04") {
#     Nodes$label[Nodes$label == "Traitement de substitution"] <- "PSTT"
#     Nodes$label[Nodes$label == "Pas de traitement"] <- "TSUB"
#     Links$color <- c(rep("cornflowerblue", 4),
#                      rep(c(rep("cornflowerblue", 4), rep("mediumseagreen", 4), rep("lightcoral", 4), rep("lightgray", 4)), 3))
#     
#   }
#   if (ATC_name == "C07FB03") {
#     # Nodes$label[Nodes$label == "Traitement de substitution"] <- "TSUB"
#     # Nodes$color[Nodes$label == "TSUB"] <- "green"
#     # Nodes$label[Nodes$label == "Pas de traitement"] <- "PSTT"
#     # Nodes$color[Nodes$label == "PSTT"] <- "red"
#     Nodes$label[Nodes$label == "Traitement de substitution"] <- "PSTT"
#     Nodes$label[Nodes$label == "Pas de traitement"] <- "TSUB"
#     Links$color <- c(rep("cornflowerblue", 4),
#                      rep(c(rep("cornflowerblue", 4), rep("lightcoral", 4), rep("mediumseagreen", 4), rep("lightgray", 4)), 3))
#     
#     #Nodes[14:16, ] <- Nodes[c(16, 14, 15),]
#     }
# 
#   Nodes$label[Nodes$label == "Décès"] <- "DC"
# 
#   fig <- plot_ly(
#     type = "sankey",
#     orientation = "h",
#     node = list(
#       label = Nodes$label,
#       color = Nodes$color,
#       pad = 15,
#       thickness = 20,
#       line = list(color = "black", width = 0.5)
#     ),
#     link = list(
#       source = Links$source,
#       target = Links$target,
#       value =  Links$value,
#       color = Links$color
#     )
#   )
#   fig <- fig %>% layout(font = list(size = 14, color = "black", weight = "bold"))
#   fig
# }
#### SANKEY  ####


#### _C03EA04 ####
ATC_name <- "C03EA04"
ATC_path <- paste0("00 - Data/", ATC_name, "/")


plotSankey(
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyNodesTem")])),
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyLinksTem")])) 
)

plotSankey(
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyNodesExp")])),
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyLinksExp")])) 
)



#### _C07FB03 ####
ATC_name <- "C07FB03"
ATC_path <- paste0("00 - Data/", ATC_name, "/")


plotSankey(
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyNodesTem")])),
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyLinksTem")])) 
)

plotSankey(
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyNodesExp")])),
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyLinksExp")])) 
)



#### _C09CA03 ####

ATC_name <- "C09CA03"
ATC_path <- paste0("00 - Data/", ATC_name, "/")


plotSankey(
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyNodesTem")])),
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyLinksTem")])) 
)

plotSankey(
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyNodesExp")])),
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyLinksExp")])) 
)


#### _C09DA03 ####

ATC_name <- "C09DA03"
ATC_path <- paste0("00 - Data/", ATC_name, "/")


plotSankey(
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyNodesTem")])),
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyLinksTem")])) 
)

plotSankey(
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyNodesExp")])),
  read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "sankeyLinksExp")])) 
)


#### TAPIS ####


#### _C03EA04 ####
ATC_name <- "C03EA04"
ATC_path <- paste0("00 - Data/", ATC_name, "/")

#### __Exposés ####
dataTapisExp <- read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "tapisExp")]))[,-1]
seqExp <- seqdef(data = dataTapisExp[, 1:13],
                 weights = dataTapisExp[, 14],
                 cpal = c("#0f766e", "#1C4073" , "#AAC0AF", "#B28B84"),
                 cnames = 0:12)

# Exemple with fixed costs equals to 2
couts <- seqsubm(seqExp, method = "CONSTANT", cval = 2)
# Distance matrix
# Insertion Deletion costst also constan and equal to 1
seq.om <- seqdist(seqExp, method = "OM", indel = 1, sm = couts)
# sequences HC
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
#plot(as.dendrogram(seq.dist), leaflab = "none")

# inertia plot
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

summary(silhouette(cutree(seq.dist, 2), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 3), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 4), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 5), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 6), as.dist(seq.om)))$avg.width


# 5 classes clustering
nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
                                        #"Reprise", 
                                        #"Substitution", "Référence"
                                        "Référence", "Substitution"))
seq_cl_exp_4cl <- seq.part
# seqIplot(seqExp, group = seq.part, space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, dataTapisExp[, 14]))), 
#          sortv = "from.start")
# seqdplot(seqExp, border = NA,
#          main = "Répartition des états au cours du temps (exposés)")
# seqIplot(seqExp,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")





#### __Témoins ####
dataTapisTem <- read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "tapisTem")]))[,-1]

seqTem <- seqdef(data = dataTapisTem[, 1:13],
                 weights = dataTapisTem[, 14],
                 cpal = c("#0f766e", "#1C4073" , "#AAC0AF", "#B28B84"),
                 cnames = 0:12)


# Exemple with fixed costs equals to 2
couts <- seqsubm(seqTem, method = "CONSTANT", cval = 2)
seq.om <- seqdist(seqTem, method = "OM", indel = 1, sm = couts)
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

summary(silhouette(cutree(seq.dist, 2), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 3), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 4), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 5), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 6), as.dist(seq.om)))$avg.width


# 5 classes clustering
# nbcl <- 5
# seq.part <- cutree(seq.dist, nbcl)
# seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
#                                         "Reprise", "Référence", 
#                                         "Substitution"))
# seq_cl_tem_5cl <- seq.part
# seqIplot(seqTem, group = seq.part, 
#          space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, pull(dataTapisTem[, 1])))), 
#          sortv = "from.start")
# seqdplot(seqTem, border = NA,
#          main = "Répartition des états au cours du temps (témoins)")
# seqIplot(seqTem,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")



nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", "Référence", 
                                        "Substitution"))
seq_cl_tem_4cl <- seq.part
# seqIplot(seqTem, group = seq.part, 
#          space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, pull(dataTapisTem[, 14])))), 
#          sortv = "from.start")




#### Tapis commun ####

seq_cl_exp_4cl <- factor(as.character(seq_cl_exp_4cl),
                         levels = levels(seq_cl_tem_4cl),
                         labels = levels(seq_cl_tem_4cl))
# 
# write.csv2(seq_cl_tem_4cl, file = "30 - Res/tem_cl.csv")
# write.csv2(seq_cl_exp_4cl, file = "30 - Res/exp_cl.csv")


levels(seq_cl_tem_4cl) <- paste0(levels(seq_cl_tem_4cl), " (N=",table(rep(seq_cl_tem_4cl, dataTapisTem[, 14])), ")")
levels(seq_cl_exp_4cl) <- paste0(levels(seq_cl_exp_4cl), " (N=",table(rep(seq_cl_exp_4cl, dataTapisExp[, 14])), ")")



ggarrange(ggseqiplot(seqTem, 
                     group = seq_cl_tem_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(rep(seq_cl_tem_4cl, dataTapisTem[, 14]))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ggseqiplot(seqExp, 
                     group = seq_cl_exp_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(rep(seq_cl_exp_4cl, dataTapisExp[, 14]))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ncol = 2, nrow = 1, 
          common.legend = T, legend = "bottom")





#### _C07FB03 ####
ATC_name <- "C07FB03"
ATC_path <- paste0("00 - Data/", ATC_name, "/")

#### __Exposés ####
dataTapisExp <- read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "tapisExp")]))[,-1]
seqExp <- seqdef(data = dataTapisExp[, 1:13],
                 weights = dataTapisExp[, 14],
                 cpal = c("#0f766e", "#1C4073" , "#AAC0AF", "#B28B84"),
                 cnames = 0:12)

# Exemple with fixed costs equals to 2
couts <- seqsubm(seqExp, method = "CONSTANT", cval = 2)
# Distance matrix
# Insertion Deletion costst also constan and equal to 1
seq.om <- seqdist(seqExp, method = "OM", indel = 1, sm = couts)
# sequences HC
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
#plot(as.dendrogram(seq.dist), leaflab = "none")

# inertia plot
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

summary(silhouette(cutree(seq.dist, 2), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 3), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 4), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 5), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 6), as.dist(seq.om)))$avg.width


# 5 classes clustering
nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
                                        #"Reprise", 
                                        #"Substitution", "Référence"
                                        "Référence", "Substitution"))
seq_cl_exp_4cl <- seq.part
# seqIplot(seqExp, group = seq.part, space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, dataTapisExp[, 14]))), 
#          sortv = "from.start")
# seqdplot(seqExp, border = NA,
#          main = "Répartition des états au cours du temps (exposés)")
# seqIplot(seqExp,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")





#### __Témoins ####
dataTapisTem <- read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "tapisTem")]))[,-1]

seqTem <- seqdef(data = dataTapisTem[, 1:13],
                 weights = dataTapisTem[, 14],
                 cpal = c("#0f766e", "#1C4073" , "#AAC0AF", "#B28B84"),
                 cnames = 0:12)


# Exemple with fixed costs equals to 2
couts <- seqsubm(seqTem, method = "CONSTANT", cval = 2)
seq.om <- seqdist(seqTem, method = "OM", indel = 1, sm = couts)
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

summary(silhouette(cutree(seq.dist, 2), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 3), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 4), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 5), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 6), as.dist(seq.om)))$avg.width


# 5 classes clustering
# nbcl <- 5
# seq.part <- cutree(seq.dist, nbcl)
# seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
#                                         "Reprise", "Référence", 
#                                         "Substitution"))
# seq_cl_tem_5cl <- seq.part
# seqIplot(seqTem, group = seq.part, 
#          space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, pull(dataTapisTem[, 1])))), 
#          sortv = "from.start")
# seqdplot(seqTem, border = NA,
#          main = "Répartition des états au cours du temps (témoins)")
# seqIplot(seqTem,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")



nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", "Référence", 
                                        "Substitution"))
seq_cl_tem_4cl <- seq.part
# seqIplot(seqTem, group = seq.part, 
#          space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, pull(dataTapisTem[, 14])))), 
#          sortv = "from.start")




#### Tapis commun ####

seq_cl_exp_4cl <- factor(as.character(seq_cl_exp_4cl),
                         levels = levels(seq_cl_tem_4cl),
                         labels = levels(seq_cl_tem_4cl))
# 
# write.csv2(seq_cl_tem_4cl, file = "30 - Res/tem_cl.csv")
# write.csv2(seq_cl_exp_4cl, file = "30 - Res/exp_cl.csv")


levels(seq_cl_tem_4cl) <- paste0(levels(seq_cl_tem_4cl), " (N=",table(rep(seq_cl_tem_4cl, dataTapisTem[, 14])), ")")
levels(seq_cl_exp_4cl) <- paste0(levels(seq_cl_exp_4cl), " (N=",table(rep(seq_cl_exp_4cl, dataTapisExp[, 14])), ")")



ggarrange(ggseqiplot(seqTem, 
                     group = seq_cl_tem_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(rep(seq_cl_tem_4cl, dataTapisTem[, 14]))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ggseqiplot(seqExp, 
                     group = seq_cl_exp_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(rep(seq_cl_exp_4cl, dataTapisExp[, 14]))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ncol = 2, nrow = 1, 
          common.legend = T, legend = "bottom")





#### _C09CA03 ####
ATC_name <- "C09CA03"
ATC_path <- paste0("00 - Data/", ATC_name, "/")

#### __Exposés ####
dataTapisExp <- read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "tapisExp")]))[,-1]
seqExp <- seqdef(data = dataTapisExp[, 1:13],
                 weights = dataTapisExp[, 14],
                 cpal = c("#0f766e", "#1C4073" , "#AAC0AF", "#B28B84"),
                 cnames = 0:12)

# Exemple with fixed costs equals to 2
couts <- seqsubm(seqExp, method = "CONSTANT", cval = 2)
# Distance matrix
# Insertion Deletion costst also constan and equal to 1
seq.om <- seqdist(seqExp, method = "OM", indel = 1, sm = couts)
# sequences HC
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
#plot(as.dendrogram(seq.dist), leaflab = "none")

# inertia plot
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

summary(silhouette(cutree(seq.dist, 2), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 3), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 4), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 5), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 6), as.dist(seq.om)))$avg.width


# 5 classes clustering
nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
                                        #"Reprise", 
                                        #"Substitution", "Référence"
                                        "Référence", "Substitution"))
seq_cl_exp_4cl <- seq.part
# seqIplot(seqExp, group = seq.part, space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, dataTapisExp[, 14]))), 
#          sortv = "from.start")
# seqdplot(seqExp, border = NA,
#          main = "Répartition des états au cours du temps (exposés)")
# seqIplot(seqExp,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")





#### __Témoins ####
dataTapisTem <- read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "tapisTem")]))[,-1]

seqTem <- seqdef(data = dataTapisTem[, 1:13],
                 weights = dataTapisTem[, 14],
                 cpal = c("#0f766e", "#1C4073" , "#AAC0AF", "#B28B84"),
                 cnames = 0:12)


# Exemple with fixed costs equals to 2
couts <- seqsubm(seqTem, method = "CONSTANT", cval = 2)
seq.om <- seqdist(seqTem, method = "OM", indel = 1, sm = couts)
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

summary(silhouette(cutree(seq.dist, 2), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 3), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 4), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 5), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 6), as.dist(seq.om)))$avg.width


# 5 classes clustering
# nbcl <- 5
# seq.part <- cutree(seq.dist, nbcl)
# seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
#                                         "Reprise", "Référence", 
#                                         "Substitution"))
# seq_cl_tem_5cl <- seq.part
# seqIplot(seqTem, group = seq.part, 
#          space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, pull(dataTapisTem[, 1])))), 
#          sortv = "from.start")
# seqdplot(seqTem, border = NA,
#          main = "Répartition des états au cours du temps (témoins)")
# seqIplot(seqTem,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")



nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", "Référence", 
                                        "Substitution"))
seq_cl_tem_4cl <- seq.part
# seqIplot(seqTem, group = seq.part, 
#          space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, pull(dataTapisTem[, 14])))), 
#          sortv = "from.start")




#### Tapis commun ####

seq_cl_exp_4cl <- factor(as.character(seq_cl_exp_4cl),
                         levels = levels(seq_cl_tem_4cl),
                         labels = levels(seq_cl_tem_4cl))
# 
# write.csv2(seq_cl_tem_4cl, file = "30 - Res/tem_cl.csv")
# write.csv2(seq_cl_exp_4cl, file = "30 - Res/exp_cl.csv")


levels(seq_cl_tem_4cl) <- paste0(levels(seq_cl_tem_4cl), " (N=",table(rep(seq_cl_tem_4cl, dataTapisTem[, 14])), ")")
levels(seq_cl_exp_4cl) <- paste0(levels(seq_cl_exp_4cl), " (N=",table(rep(seq_cl_exp_4cl, dataTapisExp[, 14])), ")")



ggarrange(ggseqiplot(seqTem, 
                     group = seq_cl_tem_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(rep(seq_cl_tem_4cl, dataTapisTem[, 14]))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ggseqiplot(seqExp, 
                     group = seq_cl_exp_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(rep(seq_cl_exp_4cl, dataTapisExp[, 14]))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ncol = 2, nrow = 1, 
          common.legend = T, legend = "bottom")







#### _C09DA03 ####
ATC_name <- "C09DA03"
ATC_path <- paste0("00 - Data/", ATC_name, "/")

#### __Exposés ####
dataTapisExp <- read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "tapisExp")]))[,-1]
seqExp <- seqdef(data = dataTapisExp[, 1:13],
                 weights = dataTapisExp[, 14],
                 cpal = c("#0f766e", "#1C4073" , "#AAC0AF", "#B28B84"),
                 cnames = 0:12)

# Exemple with fixed costs equals to 2
couts <- seqsubm(seqExp, method = "CONSTANT", cval = 2)
# Distance matrix
# Insertion Deletion costst also constan and equal to 1
seq.om <- seqdist(seqExp, method = "OM", indel = 1, sm = couts)
# sequences HC
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
#plot(as.dendrogram(seq.dist), leaflab = "none")

# inertia plot
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

summary(silhouette(cutree(seq.dist, 2), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 3), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 4), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 5), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 6), as.dist(seq.om)))$avg.width


# 5 classes clustering
nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
                                        #"Reprise", 
                                        #"Substitution", "Référence"
                                        "Référence", "Substitution"))
seq_cl_exp_4cl <- seq.part
# seqIplot(seqExp, group = seq.part, space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, dataTapisExp[, 14]))), 
#          sortv = "from.start")
# seqdplot(seqExp, border = NA,
#          main = "Répartition des états au cours du temps (exposés)")
# seqIplot(seqExp,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")





#### __Témoins ####
dataTapisTem <- read.csv(paste0(ATC_path, list.files(ATC_path)[str_detect(list.files(ATC_path), "tapisTem")]))[,-1]

seqTem <- seqdef(data = dataTapisTem[, 1:13],
                 weights = dataTapisTem[, 14],
                 cpal = c("#0f766e", "#1C4073" , "#AAC0AF", "#B28B84"),
                 cnames = 0:12)


# Exemple with fixed costs equals to 2
couts <- seqsubm(seqTem, method = "CONSTANT", cval = 2)
seq.om <- seqdist(seqTem, method = "OM", indel = 1, sm = couts)
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

summary(silhouette(cutree(seq.dist, 2), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 3), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 4), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 5), as.dist(seq.om)))$avg.width
summary(silhouette(cutree(seq.dist, 6), as.dist(seq.om)))$avg.width


# 5 classes clustering
# nbcl <- 5
# seq.part <- cutree(seq.dist, nbcl)
# seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
#                                         "Reprise", "Référence", 
#                                         "Substitution"))
# seq_cl_tem_5cl <- seq.part
# seqIplot(seqTem, group = seq.part, 
#          space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, pull(dataTapisTem[, 1])))), 
#          sortv = "from.start")
# seqdplot(seqTem, border = NA,
#          main = "Répartition des états au cours du temps (témoins)")
# seqIplot(seqTem,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")



nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", "Référence", 
                                        "Substitution"))
seq_cl_tem_4cl <- seq.part
# seqIplot(seqTem, group = seq.part, 
#          space = 0, border = NA, yaxis = FALSE,
#          ylab = paste("N =", table(rep(seq.part, pull(dataTapisTem[, 14])))), 
#          sortv = "from.start")




#### Tapis commun ####

seq_cl_exp_4cl <- factor(as.character(seq_cl_exp_4cl),
                         levels = levels(seq_cl_tem_4cl),
                         labels = levels(seq_cl_tem_4cl))
# 
# write.csv2(seq_cl_tem_4cl, file = "30 - Res/tem_cl.csv")
# write.csv2(seq_cl_exp_4cl, file = "30 - Res/exp_cl.csv")


levels(seq_cl_tem_4cl) <- paste0(levels(seq_cl_tem_4cl), " (N=",table(rep(seq_cl_tem_4cl, dataTapisTem[, 14])), ")")
levels(seq_cl_exp_4cl) <- paste0(levels(seq_cl_exp_4cl), " (N=",table(rep(seq_cl_exp_4cl, dataTapisExp[, 14])), ")")



ggarrange(ggseqiplot(seqTem, 
                     group = seq_cl_tem_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(rep(seq_cl_tem_4cl, dataTapisTem[, 14]))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ggseqiplot(seqExp, 
                     group = seq_cl_exp_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(rep(seq_cl_exp_4cl, dataTapisExp[, 14]))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ncol = 2, nrow = 1, 
          common.legend = T, legend = "bottom")





