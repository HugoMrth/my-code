HERCULE <- data.frame(
  stump = rep(1:3, each = 3),
  level = 1
)

slash.count <- function(data) {
  new <- unique.array(data)
  new$level <- new$level - 1
  data <- rbind(
    data,
    new
  )
  
  N <- 0
  while (nrow(data) != 0) {
    if (data[1, ]$level != 0) {
      data <- rbind(
        data, 
        data.frame(
          stump = rep(data[1, ]$stump, 2),
          level = rep(data[1, ]$level - 1, 2)
        )
      )
    } 
    data <- data[-1, ]
    N <- N + 1
  }
  return(N)
}

slash.count(HERCULE)