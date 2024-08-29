# Load xtable
library(xtable) 

# Load example data
data(mtcars) 

# Linear regression
my_mod <- lm(mpg ~ wt + qsec, mtcars) 

# Summary statistics
summary(my_mod) 

# Create LaTeX table
print(xtable(summary(my_mod)), type = "latex") 
