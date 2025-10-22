# E-value helpers (no extra packages required)
evalue_ratio <- function(R){
  if(is.na(R) || !is.finite(R)) return(NA_real_)
  if(R >= 1){
    return(R + sqrt(R * (R - 1)))
  } else {
    rrec <- 1 / R
    return(rrec + sqrt(rrec * (rrec - 1)))
  }
}

# compute E-value for point estimate and for CI bound nearest the null
# inputs: est (ratio), lo (CI lower), hi (CI upper), null = 1
evalue_from_est_ci <- function(est, lo, hi, null = 1){
  if(est == 0) stop("estimate must be nonzero ratio")
  # choose bound nearer to null (distance on ratio scale)
  # if est >=1, the relevant bound is lo; if est <1, relevant bound is hi
  if(est >= 1){
    bound <- lo
  } else {
    bound <- hi
  }
  list(
    estimate = est,
    evalue_estimate = evalue_ratio(est),
    ci_bound = bound,
    evalue_ci_bound = ifelse(bound == null, NA_real_, evalue_ratio(bound))
  )
}

# Examples
# 1) RR = 2.0 (95% CI 1.5 - 2.7)
evalue_from_est_ci(2.0, 1.5, 2.7)

# 2) HR = 0.6 (95% CI 0.4 - 0.9) (protective)
evalue_from_est_ci(0.6, 0.4, 0.9)
