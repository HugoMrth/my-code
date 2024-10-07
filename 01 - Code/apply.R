#### Conversion de toutes les colonnes integer en numerique
data[, unlist(lapply(data, is.integer))] <- apply(data[, unlist(lapply(data, is.integer))], 2, as.character)





#### Element wise max of two vector 
pmax(x, y)





#### Update a dataframe within sapply# Sample data frame
df <- data.frame(A = c(1, 2, 3, 4, 5),
                 B = c(6, 7, 8, 9, 10))

# List of line numbers to update
line_numbers <- c(2, 4)

# Use sapply to update specific rows
sapply(line_numbers, function(i) {
  # Update a specific row, say multiply column A and B by 10
  # The <<- operator is used to ensure the global data frame df gets updated.
  df[i, "A"] <<- df[i, "A"] * 10
  df[i, "B"] <<- df[i, "B"] * 10
})




