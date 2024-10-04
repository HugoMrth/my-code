#### Splitting a integer vector in a list

# Directly following numbers are in the same elements
X <- c(2, 3, 4, 5, 6, 8, 9, 11, 12, 14, 15, 18, 19, 20, 21)

# diff() to detect whenever there is a gap, adding 2 at the end to detect the last group
# convert to logical -> TRUE means it's the last number of the group
dXb <- c(diff(X), 2) > 1
# splitting X
split(X, f = rep(1:sum(dXb), # as many groups as there are TRUEs in dXb
                 c(which(dXb)[1], diff(which(dXb))))) # diff() again to get the size of each group, adding the starting point
