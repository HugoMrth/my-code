library(TraMineR)
library(cluster)
library(seqhandbook)
library(questionr)

### LINK
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html

#### Data ####

get_script_file_path <- function() {
  dp <- rstudioapi::getSourceEditorContext()$path
  vec_slash <- gregexpr("/", dp)[[1]]
  return(substring(dp, 1, vec_slash[length(vec_slash)] - 1))
}

load(paste0(get_script_file_path(), "/Sequences Analysis.Rdata"))
data$generation <- factor(data$generation, labels = c("1930-38", "1939-45", "1946-50"))
str(data)

# State creation
labels <- c("agric", "acce", "cadr", "pint", "empl", "ouvr", "etud", "inact", "smil")
seq <- seqdef(data[, 1:37], states = labels)



#### Optimal matching & clustering ####


# Exemple with fixed costs equals to 2
couts <- seqsubm(seq, method = "CONSTANT", cval = 2)
# Distance matrix
# Insertion Deletion costst also constan and equal to 1
seq.om <- seqdist(seq, method = "OM", indel = 1, sm = couts)


# sequences HC
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
plot(as.dendrogram(seq.dist), leaflab = "none")

# inertia plot
plot(sort(seq.dist$height, decreasing = TRUE)[1:20], type = "s", xlab = "nb de classes", ylab = "inertie")

# 5 classes clustering
nbcl <- 5
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = paste("classe", 1:nbcl, sep = "."))


#### Plots ####

# chronogrammes / state distribution plots) 
# Transversal cuts serie
# proportion of people in each state at each age
seqdplot(seq, group = seq.part, xtlab = 14:50, border = NA)

# Tapis / Index plots
# Every sequences of the different groups
seqIplot(seq, xtlab = 14:50, space = 0, border = NA, yaxis = FALSE)
seqIplot(seq, group = seq.part, xtlab = 14:50, space = 0, border = NA, yaxis = FALSE)

# Sorted index plots (multidimensionnal scaling)
ordre <- cmdscale(as.dist(seq.om), k = 1)
seqIplot(seq, group = seq.part, sortv = ordre, xtlab = 14:50, space = 0, border = NA, yaxis = FALSE)


# Index plot of every sequences ordered according t the dendrogram
seq_heatmap(seq, seq.dist, labCol = 14:50)



#### Additionnal plots ####

# average distance of the sequcens from the center of the group
aggregate(disscenter(as.dist(seq.om), group = seq.part), list(seq.part), mean)

# Most common sequences in each group
seqfplot(seq, group = seq.part)

# Most represented state at each age for each group
seqmsplot(seq, group = seq.part, xtlab = 14:50, main = "classe")

# Average time spent in each state
seqmtplot(seq, group = seq.part)

# Some reprensentative sequences in each group
seqrplot(seq, group = seq.part, dist.matrix = seq.om, criterion = "dist")

# Group entropy through time
#the lower the most similar the group at a given time
seqHtplot(seq, group = seq.part, xtlab = 14:50)



#### Typology ####

# Groups description
freq(seq.part)


# Description per generation
cprop(table(seq.part, data$generation))
chisq.test(table(seq.part, data$generation))
