library(Rcpp)

hdps_screen <- function(outcome, treatment, covars,
                        dimension_names=NULL, dimension_indexes=NULL,
                        keep_n_per_dimension=200, keep_k_total=500,
                        verbose=FALSE, debug=FALSE) {
  
  check_inputs(outcome, treatment, covars)
  
  if (!is.null(dimension_names) && !is.null(dimension_indexes)) {
    stop("At most, one of dimension_names and dimension_indexes should be specified")
  }
  
  if (!is.null(dimension_names)) {
    dimension_indexes <- lapply(dimension_names, grep, x = colnames(covars))
    all_idx <- do.call(c, dimension_indexes)
    if (anyDuplicated(all_idx)) {
      stop("Some column names of covars are matched by more than one pattern in dimension_names")
    }
    if (!all(all_idx %in% 1:ncol(covars))) {
      warning("Some column names of covars are not matched by any of the patterns in dimension_names")
    } 
  }
  
  # Step 2. Identify empirical candidate covariates
  if (is.null(dimension_indexes)) {
    if (verbose) message("No dimensions specified...")
    if (verbose) message("Filtering covariates...")
    filtered_covars <- identify_covariates(covars, keep_n_covars=keep_n_per_dimension, indexes=FALSE)
  } else {
    if (verbose) message("Filtering covariates...")
    filtered_covars <- lapply(seq_along(dimension_indexes), function(i) {
      if (verbose) message("\tFiltering dimension ", 
                           if (!is.null(dimension_names)) dimension_names[i] else i,
                           "...")
      identify_covariates(covars[, dimension_indexes[[i]]], keep_n_covars=keep_n_per_dimension, indexes=FALSE)
    })
    if (verbose) message("Combining dimensions...")
    filtered_covars <- do.call(cbind, filtered_covars)
  }
  
  #Step 3. Assess recurrence
  if (verbose) message("Expanding covariates...")
  ar <- assess_recurrence(filtered_covars, debug=debug)
  expanded_covars <- ar[["mat"]]
  quants <- ar[["quants"]]
  
  if (dim(expanded_covars)[2] != length(quants)) stop("something is wrong...")
  
  #Step 4. Prioritize covariates
  if (verbose) message("Prioritizing covariates...")
  ordered_indexes <- prioritize_covariates(outcome, treatment, expanded_covars, keep_NaNs=TRUE)
  
  res <- list(expanded_covars=expanded_covars,
              quants=quants,
              ordered_indexes=ordered_indexes,
              keep_k_total=keep_k_total
  )
  if (verbose) message("...Done!")
  class(res) <- "hdps_covars"
  
  return(res)
  
}



predict.hdps_covars <- function(object, newdata=NULL, keep_k_total, ...) {
  if (missing(keep_k_total)) keep_k_total <- object$keep_k_total
  
  if (!is.null(newdata)) {
    #could be more efficient here
    # by first filtering the quants to only the keep_k_total needed
    # then by grouping by varname
    mats <- lapply(object$quants, function(quant) {
      x <- newdata[, quant$varname]
      mat <- column_recurrence(x, list(quant))$mat
      colnames(mat) <- paste(quant$varname, colnames(mat), sep="")    
      mat
    })
    expanded_covars <- do.call(cbind, mats)
  } else {
    expanded_covars <- object$expanded_covars
  }
  
  if (ncol(expanded_covars) <= keep_k_total) {
    return(expanded_covars)
  }
  
  ordered_indexes <- object$ordered_indexes
  selected_indexes <- ordered_indexes[1:min(keep_k_total, length(ordered_indexes))]
  selected_covars <- expanded_covars[, sort(selected_indexes)]
  return(selected_covars)
}


assess_recurrence <- function(covars, debug=FALSE) {
  #expands a matrix by replacing it's columns with as.numeric(x > 0), 
  # as.numeric(x > median(x)), as.numeric(x > quantile(x, prob=0.75))
  #only unique columns (per original column) are kept
  
  covars <- as.matrix(covars)
  
  temp <- function(i) {
    column <- covars[,i]
    quants <- get_quantiles(column)
    column_recurrence(column, quants, warndup=debug)
  }
  #mats_quants <- lapply(1:ncol(covars), temp)
  mats_quants <- list()
  
  for (i in 1:ncol(covars))
    mats_quants[[i]] = temp(i)
  
  mats <- lapply(mats_quants, `[[`, "mat")#function(mq) mq[["mat"]])
  quants <- lapply(mats_quants, `[[`, "quants")#function(mq) mq[["quants"]])
  
  cnams <- colnames(covars)  
  if (!is.null(cnams)) {
    for (i in seq_along(mats)) {
      colnames(mats[[i]]) <- paste(cnams[i], colnames(mats[[i]]), sep="")    
      quants[[i]] <- lapply(quants[[i]], function(q) c(varname=cnams[i], q))
    }
  }
  
  mat <- do.call(cbind, mats)
  quants <- do.call(c, quants)
  list(mat=mat, quants=quants)
}


prioritize_covariates <- function(outcome, treatment, covars, return_bias=FALSE, keep_NaNs=FALSE) {
  check_inputs(outcome, treatment, covars, covars_bin=TRUE)
  
  treatment <- factor(treatment)
  
  covar_prev <- by(covars, treatment, colMeans)
  p_c1 <- covar_prev[[levels(treatment)[1]]]
  p_c0 <- covar_prev[[levels(treatment)[2]]]
  
  #a vector of rr_cd if rr_cd > 1, 1/rr_cd otherwise
  rr_cds <- calc_rr_cds(outcome, covars)
  
  #Infs in rr_cds will result in NaN for some covariates
  bias_mult <- (p_c1 * (rr_cds - 1) + 1) / (p_c0 * (rr_cds - 1) + 1)
  
  abs_log_bias_mult <- abs(log(bias_mult))
  
  na.last <- if (keep_NaNs) TRUE else NA
  ordered_idxs <- order(abs_log_bias_mult, decreasing=TRUE, na.last=na.last)
  if (return_bias) attr(ordered_idxs, "bias_m") <- bias_mult[ordered_idxs]
  ordered_idxs
}

identify_covariates <- function(covars, keep_n_covars=200, indexes=FALSE) {
  
  if (!indexes && ncol(covars) <= keep_n_covars) return(covars)
  vars <- colPrevScores(as.matrix(covars))
  var_ords <- order(vars, decreasing=TRUE)[1:min(keep_n_covars, ncol(covars))] 
  
  if (indexes) {
    return(var_ords)
  } else {
    covars[, sort(var_ords)]  
  }
}

get_quantiles <- function(x) {
  xx0 <- x[x>0]
  quants <- quantile(xx0, probs=c(0.5, 0.75), names=FALSE, type=2)
  quants <- list(list(q="_once", count=1),
                 list(q="_sporadic", count=quants[1]),
                 list(q="_frequent", count=quants[2]))
  
  counts <- sapply(quants, `[[`, "count")
  ux <- unique(xx0)
  cutoffs <- sapply(counts, function(count) min(ux[ux >= count]))  
  dups <- duplicated(cutoffs)
  quants[!dups]
}

column_recurrence <- function(x, quants, warndup=FALSE) {
  mat <- matrix(0, length(x), length(quants))    
  colnames(mat) <- sapply(quants, `[[`, "q")
  
  for (i in 1:length(quants)) {
    mat[, i] <- x >= quants[[i]]$count
  }
  
  if (warndup) {
    dups <- duplicated(mat, MARGIN=2)
    if (any(dups)) {
      warning("Duplicate columns in mat. This should not happen when hdps_screen is called, but could when predict is called.")
    }
  }
  
  
  list(mat=mat, quants=quants)
}

check_inputs <- function(outcome, treatment, covars, covars_bin=FALSE) {
  n = nrow(covars)
  
  if(!is.vector(outcome)) stop("outcome should be a vector")
  if(!is.vector(treatment)) stop("treatment should be a vector")
  
  if (!length(outcome) == n || !length(treatment) == n)
    stop("outcome and treatment should be the same length, which should be equal to nrow(covars)")
  
  if (!all(outcome %in% c(0,1)))
    stop("outcome should be binary")
  if (!all(treatment %in% c(0,1)))
    stop("treatment should be binary")
  if (covars_bin && !all(covars %in% c(0,1)))
    stop("covars should be binary")
}


SL.hdps.generator <- function(out_name, dimension_names, predef_covar_names=c(), keep_k_total, ..., 
                              cvglmnet=FALSE, glmnet_args=if (cvglmnet) list() else list(lambda=0)) {
  function(Y, X, newX, family, obsWeights, id) {
    if (missing(newX)) {
      newX <- X
    }
    if(family$family == 'gaussian') {
      stop("SL.hdps only for binomial")
    }
    
    hdps_fit <- hdps_screen(X[, out_name], Y, X, dimension_names, keep_k_total=keep_k_total, ...)
    
    predef_covars <- X[, predef_covar_names]
    if (keep_k_total > 0) {
      hdps_covars <- predict(hdps_fit)
      hdps_keep <- colnames(hdps_covars)[abs(cor(Y, hdps_covars)) <= 0.95]
      hdps_covars <- hdps_covars[, hdps_keep]
      df = as.data.frame(cbind(predef_covars, hdps_covars))
    } else {
      hdps_keep <- NULL
      df = as.data.frame(predef_covars)
    }
    
    smm <- sparse.model.matrix(~.-1, df)
    
    myglmnet <- function(...) if (cvglmnet) 
      cv.glmnet(smm, Y, family="binomial") else 
        glmnet(smm, Y, family="binomial", ...)
    glmnet_fit <- do.call(myglmnet, glmnet_args)
    
    if (identical(X, newX)) {
      smmnew <- smm
    } else {
      
      new_predef_covars <- newX[, predef_covar_names]
      if (keep_k_total > 0) {
        new_hdps_covars <- predict(hdps_fit, newdata=newX)
        new_hdps_covars <- new_hdps_covars[, hdps_keep]
        new_df = as.data.frame(cbind(new_predef_covars, new_hdps_covars))
      } else {
        new_df = as.data.frame(new_predef_covars)
      }
      
      smmnew <- sparse.model.matrix(~.-1, new_df)
    }
    
    pred <- predict(glmnet_fit, smmnew, type="response")
    if (ncol(pred) != 1) stop("Check cvglmnet and glmnet_args arguments to insure that predict returns only one column")
    
    
    # fit returns all objects needed for predict.SL.template
    fit <- list(glmnet_fit = glmnet_fit, hdps_fit = hdps_fit, 
                predef_covar_names=predef_covar_names, hdps_keep=hdps_keep, keep_k_total=keep_k_total)
    # declare class of fit for predict.SL.template
    class(fit) <- 'SL_hdps'
    # return a list with pred and fit
    out <- list(pred = pred, fit = fit)
    return(out)
  }
}


predict.SL_hdps <- function(object, newdata, ...){
  new_predef_covars <- newdata[, object$predef_covar_names]
  new_hdps_covars <- predict(object$hdps_fit, newdata=newdata, keep_k_total=object$keep_k_total)
  new_hdps_covars <- new_hdps_covars[, object$hdps_keep]
  new_df <- cbind(new_predef_covars, new_hdps_covars)
  smmnew <- sparse.model.matrix(~.-1, new_df)
  pred <- predict(object$glmnet_fit, smmnew, type = "response")
  if (ncol(pred) != 1) stop("Check cvglmnet and glmnet_args arguments to insure that predict returns only one column")
  pred
}

screen.names <- function (names) {
  function (Y, X, family, obsWeights, id, ...) {
    colnames(X) %in% names
  }
}


screen.excludenames <- function (names) {
  function (Y, X, family, obsWeights, id, ...) {
    !(colnames(X) %in% names)
  }
}

cppFunction('NumericVector calc_rr_cds(NumericVector outcome, NumericMatrix covars) {
  int nrow = covars.nrow(), ncol = covars.ncol();
  if (outcome.length() != nrow) {
    stop("length of outcome should be the same as the number of rows in covars");
  }
  
  NumericVector out(ncol);
  out.attr("names") = colnames(covars);
  
  for (int j = 0; j < ncol; j++) {
    double outcomes1 = 0;
    double outcomes0 = 0;
    double n1 = 0;
    double n0 = 0;
        
    for (int i = 0; i < nrow; i++) {
      double covar = covars(i,j);
      if (covar == 0.0) {
        n0 += 1;
        outcomes0 += outcome(i);
      } else {
        n1 += 1;
        outcomes1 += outcome(i);
      }
    }
    
    double prev1 = outcomes1/n1;
    double prev0 = outcomes0/n0;
    
    double rr = prev1/prev0;
    out(j) = rr;
  }
  return out;
}')


cppFunction('NumericVector colPrevScores(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(ncol);
  
  for (int j = 0; j < ncol; j++) {
    int num_non_zero = 0;
    
    for (int i = 0; i < nrow; i++) {
      num_non_zero += x(i,j) > 0.0 ? 1 : 0;
    }
    
    double prev = (double) num_non_zero/nrow;
    
    out(j) = std::min(prev, 1.0-prev);
  }
  
  return out;
}')

cppFunction('NumericVector colVars(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(ncol);
  
  for (int j = 0; j < ncol; j++) {
    double mean = 0;
    double M2 = 0;
    int n;
    double delta, xx;
    
    for (int i = 0; i < nrow; i++) {
      n = i+1;
      xx = x(i,j);
      delta = xx - mean;
      mean += delta/n;
      M2 = M2 + delta*(xx-mean);
    }
    
    out(j) = M2/(n-1);
  }
  
  return out;
}')

#### Test code ####

n <- 1000
p <- 10000
out <- rbinom(n, 1, 0.05)
trt <- rbinom(n, 1, 0.5)
covars <- matrix(rbinom(n*p, 3, 0.05), n)
colnames(covars) <- c(paste("drug", 1:(p/2), sep="_"),
                      paste("proc", 1:(p/2), sep="_"))

dimension_names <- c("drug", "proc")

screened_covars_fit <- hdps_screen(out, trt, covars, 
                                   dimension_names = dimension_names,
                                   keep_n_per_dimension = 400,
                                   keep_k_total = 200,
                                   verbose=TRUE)

screened_covars <- predict(screened_covars_fit)
