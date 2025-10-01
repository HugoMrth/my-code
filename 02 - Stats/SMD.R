# Function to compute unweighted SMD
smd_unweighted <- function(data, treat, var) {
  treat_data <- data %>% filter(!!sym(treat) == 1)
  control_data <- data %>% filter(!!sym(treat) == 0)
  
  x1 <- treat_data[[var]]
  x0 <- control_data[[var]]
  
  m1 <- mean(x1, na.rm = TRUE)
  m0 <- mean(x0, na.rm = TRUE)
  s1 <- sd(x1, na.rm = TRUE)
  s0 <- sd(x0, na.rm = TRUE)
  
  smd <- (m1 - m0) / sqrt((s1^2 + s0^2) / 2)
  return(smd)
}

# Function to compute weighted SMD
smd_weighted <- function(data, treat, var, weight) {
  treat_data <- data %>% filter(!!sym(treat) == 1)
  control_data <- data %>% filter(!!sym(treat) == 0)
  
  x1 <- treat_data[[var]]
  x0 <- control_data[[var]]
  w1 <- treat_data[[weight]]
  w0 <- control_data[[weight]]
  
  x1 <- x1[!is.na(w1)]
  w1 <- w1[!is.na(w1)]
  x0 <- x0[!is.na(w0)]
  w0 <- w0[!is.na(w0)]
  
  m1 <- weighted.mean(x1, w1, na.rm = TRUE)
  m0 <- weighted.mean(x0, w0, na.rm = TRUE)
  
  # Weighted variance
  v1 <- sum(w1 * (x1 - m1)^2, na.rm = TRUE) / sum(w1, na.rm = TRUE)
  v0 <- sum(w0 * (x0 - m0)^2, na.rm = TRUE) / sum(w0, na.rm = TRUE)
  
  smd <- (m1 - m0) / sqrt((v1 + v0) / 2)
  return(smd)
}
