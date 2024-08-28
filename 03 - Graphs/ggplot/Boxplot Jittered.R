# install.packages("ggplot2")
library(ggplot2)

# Data
set.seed(8)
y <- rnorm(200)
df <- data.frame(y)

# Basic box plot
ggplot(df, aes(x = "", y = y)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour = 2, width = 0.2) 






# install.packages("ggplot2")
library(ggplot2)

# Data
set.seed(8)
y <- rnorm(200)
group <- sample(LETTERS[1:3], size = 200,
                replace = TRUE)
df <- data.frame(y, group)

# Box plot by group with jitter
ggplot(df, aes(x = group, y = y, colour = group)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() 
