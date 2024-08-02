
sum(dataTapisExp[, 1])
dataTapisExp <- dataTapisExp[, c(1, seq(1, 365, 30)+1)]

dataTapisExp <- dataTapisExp %>%
  group_by_all() %>%
  summarise(V1 = sum(V1))
sum(dataTapisExp[, 1])

seqExp <- seqdef(data = dataTapisExp[, 2:14],
                 weights = pull(dataTapisExp[, 1]),
                 cpal = c("lightgray", "lightcoral" ,"cornflowerblue", "mediumseagreen"),
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

# 5 classes clustering
nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
                                        #"Reprise", 
                                        "Substitution", "Référence"))
seq_cl_exp_4cl <- seq.part
seqIplot(seqExp, group = seq.part, space = 0, border = NA, yaxis = FALSE,
         ylab = paste("N =", table(rep(seq.part, pull(dataTapisExp[, 1])))), 
         sortv = "from.start")
seqdplot(seqExp, border = NA,
         main = "Répartition des états au cours du temps (exposés)")
seqIplot(seqExp,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")














sum(dataTapisTem[, 1])
dataTapisTem <- dataTapisTem[, c(1, seq(1, 365, 30)+1)]

dataTapisTem <- dataTapisTem %>%
  group_by_all() %>%
  summarise(V1 = sum(V1))
sum(dataTapisTem[, 1])

seqTem <- seqdef(data = dataTapisTem[, 2:14],
                 weights = pull(dataTapisTem[, 1]),
                 cpal = c("lightgray", "lightcoral" ,"cornflowerblue", "mediumseagreen"),
                 cnames = 0:12)


# Exemple with fixed costs equals to 2
couts <- seqsubm(seqTem, method = "CONSTANT", cval = 2)
seq.om <- seqdist(seqTem, method = "OM", indel = 1, sm = couts)
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", "Référence", 
                                        "Substitution"))
seq_cl_tem_4cl <- seq.part
seqIplot(seqTem, group = seq.part, 
         space = 0, border = NA, yaxis = FALSE,
         ylab = paste("N =", table(rep(seq.part, pull(dataTapisTem[, 1])))), 
         sortv = "from.start")








levels(seq_cl_tem_4cl) <- paste0(levels(seq_cl_tem_4cl), " (N=",table(rep(seq_cl_tem_4cl, pull(dataTapisTem[, 1]))), ")")
levels(seq_cl_exp_4cl) <- paste0(levels(seq_cl_exp_4cl), " (N=",table(rep(seq_cl_exp_4cl, pull(dataTapisExp[, 1]))), ")")

ggarrange(ggseqiplot(seqTem, 
                     group = seq_cl_tem_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(seq_cl_tem_4cl)) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ggseqiplot(seqExp, 
                     group = seq_cl_exp_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(seq_cl_exp_4cl)) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ncol = 2, nrow = 1, 
          common.legend = T, legend = "bottom")
