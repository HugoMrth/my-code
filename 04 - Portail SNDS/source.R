#### Libraries ####

library(haven)
library(sas7bdat)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)
library(TraMineR)
library(cluster)
library(WeightedCluster)
library(survival)
library(survminer)
library(rms)
library(nnet)
library(car)
library(GGally)
library(ggplot2)
library(broom)
library(effects)
library(MASS)
library(stringr)
library(ggfortify)
library(lmtest)


#### Mes fonctions ####

# Fonctions issues de mes packages perso
# Impossible à instalé sur le portail

describe <- function(# Arguments de base
  data,
  vars = NULL,
  factor = NULL,
  label = NULL,
  weights = NULL,
  # Intervalles de confiance
  conf.level = 0.95,
  conf.method.cat = c("waldcc", "sisonglaz", "cplus1", "goodman", "wald", "wilson", "boot"),
  conf.method.num = c("classic", "boot"),
  # Variables quanti
  prop.type = c("col.percent", "row.percent", "tot.percent"),
  prop.test = c("test", "chi", "fisher"),
  chi.correct = TRUE,
  simulate.p.value = FALSE,
  # Variables quali
  num.type = c("mean", "both", "med"),
  mean.test = c("test", "student", "kruskal"),
  # Valeurs manquantes
  na.omit = TRUE,
  na.str.default = c("missing", "mode", "value"),
  na.str.value = NULL,
  na.num.default = c("none", "mean", "med", "value"),
  na.num.value = NULL,
  # Taux d'incidence
  pop.ref = NULL,
  # Décimales
  decimal = 1,
  cut.pvalue = 0.001,
  # P-valeur
  p.decimal = 3,
  p.adjust = FALSE,
  p.adjust.method = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"),
  # Colonnes à inclure
  include.n = TRUE,
  include.tot = TRUE,
  include.conf = TRUE,
  include.minmax = FALSE,
  include.p = TRUE,
  include.test.name = FALSE,
  # Mise en page
  lang = c("fr", "en"),
  merge.cols = TRUE,
  big.mark = NULL) {
  
  options(warn = 1) # Warnings on
  
  # Rétro-compatibilité
  # if ("conf.method" %in% names(list(...)) ) {
  #   conf.method.cat <- InDots(..., arg = "conf.method")
  # }
  
  # Matching des arguments proposant une liste d'options
  # Retourne une erreur si l'argument renseigne n'est pas dans la liste
  # Et le prmier de la liste s'il n'est pas renseigne
  num.type <- match.arg(num.type, choices = c("mean", "both", "med"))
  prop.type <- match.arg(prop.type, choices = c("col.percent", "row.percent", "tot.percent"))
  prop.test <- match.arg(prop.test, choices = c("test", "chi", "fisher"))
  mean.test <- match.arg(mean.test, choices = c("test", "student", "kruskal"))
  conf.method.cat <- match.arg(conf.method.cat, choices = c("waldcc", "sisonglaz", "cplus1", "goodman", "wald", "wilson","boot"))
  conf.method.num <- match.arg(conf.method.num, choices = c("classic","boot"))
  na.num.default <- match.arg(na.num.default, choices = c("none", "mean", "med", "value"))
  na.str.default <- match.arg(na.str.default, choices = c("missing", "mode", "value"))
  p.adjust.method <- match.arg(p.adjust.method, choices = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"))
  lang <- match.arg(lang, choices = c("fr", "en"))
  
  #### Check Params 
  
  # Recuperation des noms de colonnes si les indices sont numeriques
  if (is.numeric(factor))   factor <- colnames(data)[factor]
  if (is.numeric(vars))        vars <- colnames(data)[vars]
  if (is.numeric(weights))     weights <- colnames(data)[weights]
  
  # Stops si parametres non appropries
  if (is.null(data)) stop("argument 'data' is missing, with no default")
  if (!is.data.frame(data)) stop("'data' must be a data.frame or a tibble")
  if (is.null(vars)) {
    warning("argument 'vars' is missing, every variable of data will be describe")
    vars <- colnames(data)
  }
  if (all(vars %ni% colnames(data))) stop("'vars' is not a defined column of data")
  if (is.defined(factor)) {
    if (all(factor %ni% colnames(data))) stop("'factor' is not a defined column of data")
    if (length(factor) > 1) stop("'factor' must be unique")
    if (any(names(table(factor)) %in% c("Total", "minmax", "IC"))) {
      err <- c("Total", "minmax", "IC")[which(c("Total", "minmax", "IC") %in% names(table(factor)))]
      stop(paste0("for functionning purposes, ", err, " cannot be a level of 'factor'."))
    }
  } else {
    message("argument 'factor' is missing, no bivariate analysis will be done")
    if (include.p) message("'include.p' will be ignore if 'factor' is missing")
    if (include.test.name) message("'include.test.name' will be ignore if 'factor' is missing")
    include.tot <- FALSE
    include.p <- FALSE
    include.test.name <- FALSE
  }
  if (is.defined(weights) & num.type != "mean") stop("cannot compile median for weighted data")
  if (any(vars %ni% colnames(data))) { # Si des variable renseignees sont manquantes
    temp <- vars[vars %ni% colnames(data)] # Recuperation des variables manquantes
    warning(paste0("'vars' contains missing variables in data : ", paste0(temp, collapse = " & "), " ; missing variables will be ignored"))
    vars <- vars[vars %in% colnames(data)]
  }
  if (!is.null(label) & length(label) != length(vars)) { # S'il n'y a pas autant de libelles que de variables
    warning("'label' length must match 'vars', label will be ignored")
    label <- NULL
  }
  if (length(label) != length(unique(label))) { # Dedoublonnage des libelles
    for (i in 1:length(label)) {
      cond <- label[i] == label
      if (sum(cond) > 1)  label[cond] <- paste0(label[cond], "_",1:sum(cond)) # Ajout d'un tag de numero pour dedoublonner
    }
    message("duplicated values in 'label' were tagged with column number")
  }
  if (is.defined(pop.ref)) {
    if (is.null(factor)) {
      if (ncol(pop.ref) != 3) stop("'pop.ref' must contains 3 columns : variable name, level name and denominator")
    } else {
      if (ncol(pop.ref) != (2 + length(table(data[, factor]))))
        stop("'pop.ref' must contains ", (2 + length(table(data[, factor]))), " columns : variable name, level name and one denominator column per factor level")
    }
  }
  
  if (na.str.default == "value" & is.null(na.str.value)) stop("na.str.default = 'value' requires na.str.value to be defined")
  if (na.num.default == "value" & is.null(na.num.value)) stop("na.num.default = 'value' requires na.num.value to be defined")
  if (na.str.default != "value" & !is.null(na.str.value)) warning("'na.str.value' only used when na.str.default = 'value' and will be ignored")
  if (na.num.default != "value" & !is.null(na.num.value)) warning("'na.num.value' only used when na.num.default = 'value' and will be ignored")
  if (na.omit & na.str.default != "missing") warning("'na.omit = TRUE' and na.str.default will be ignored")
  if (na.omit & na.num.default != "none") warning("'na.omit = TRUE' and na.num.default will be ignored")
  if (p.adjust.method != "BH" & !p.adjust) warning("'p.adjust.method' only used when p.adjust = TRUE and will be ignored")
  
  # Attribution de valeurs par defaut aux parametres binaires
  if (!is.numeric(conf.level) || conf.level < 0 || conf.level > 1) {
    warning("conf.level must be between 0 and 1, default value is assigned : conf.level <- 0.95")
    conf.level <- 0.95}
  if (chi.correct %ni% c(TRUE, FALSE)) {
    warning("chi.correct must be logical (TRUE, FALSE), default value is assigned : chi.correct <- FALSE")
    chi.correct <- FALSE}
  if (!is.numeric(decimal)) decimal <- as.numeric(decimal)
  if (floor(decimal) != decimal || decimal < 0) {
    warning("decimal must be a positive integer, default value is assigned : decimal <- 1")
    decimal <- 1}
  if (cut.pvalue >= 1 | cut.pvalue < 0) {
    warning("cut.pvalue must be between 0 and 1, default value is assigned : cut.pvalue <- 0.001")
    cut.pvalue <- 0.001}
  if (p.adjust %ni% c(TRUE, FALSE)) {
    warning("p.adjust must be logical (TRUE, FALSE), default value is assigned : p.adjust <- FALSE")
    p.adjust <- FALSE}
  if (na.omit %ni% c(TRUE, FALSE)) {
    warning("include.n must be logical (TRUE, FALSE), default value is assigned : include.n <- TRUE")
    include.n <- TRUE}
  if (include.n %ni% c(TRUE, FALSE)) {
    warning("include.n must be logical (TRUE, FALSE), default value is assigned : include.n <- TRUE")
    include.n <- TRUE}
  if (include.tot %ni% c(TRUE, FALSE)) {
    warning("include.tot must be logical (TRUE, FALSE), default value is assigned : include.tot <- TRUE")
    include.tot <- TRUE}
  if (include.conf %ni% c(TRUE, FALSE)) {
    warning("include.conf must be logical (TRUE, FALSE), default value is assigned : include.conf <- TRUE")
    include.conf <- TRUE}
  if (include.p %ni% c(TRUE, FALSE)) {
    warning("include.p must be logical (TRUE, FALSE), default value is assigned : include.p <- TRUE")
    include.p <- TRUE}
  if (include.minmax %ni% c(TRUE, FALSE)) {
    warning("include.minmax must be logical (TRUE, FALSE), default value is assigned : include.minmax <- FALSE")
    include.minmax <- FALSE}
  if (include.test.name %ni% c(TRUE, FALSE)) {
    warning("include.test.name must be logical (TRUE, FALSE), default value is assigned : include.test.name <- FALSE")
    include.test.name <- FALSE}
  
  # Conversion en entier des params
  decimal <- as.integer(decimal)
  p.decimal <- as.integer(p.decimal)
  
  #### Definition des fonctions IC 
  
  # Fonctions de calculs d'intervalles de confiance pour la moyenne, mediane, et pourcentage
  # Ces fonctions sont utilises dans les fonctions de creation de tableaux descriptifs : tab.res.moy, tab.res.med et tab.res.fac
  
  # Fonction de calcul d'intervalle de confiance pour la mediane
  IC_med <- function(x, conf.level = 0.95, sides = c("two.sided", "left", "right"), na.rm = TRUE, conf.method.num = c("classic", "boot"), R = 999) {
    # Gestion/Matching des arguments
    sides <- match.arg(sides, choices = c("two.sided", "left", "right"), several.ok = FALSE)
    if (sides != "two.sided") conf.level <- 1 - 2 * (1 - conf.level) # Ajustement du alpha si hypothese bilaterale
    
    #Calcul de l'IC et de la mediane
    r <- switch(match.arg(arg = conf.method.num, choices = c("classic", "boot")),
                classic = test_binom_med(x, conf.level = conf.level)$conf.int,  # Si methode de calcul classique
                boot = boot.ci(boot::boot(x, function(x, d) median(x[d], na.rm = na.rm), R = R),
                               conf = conf.level, type = "basic")[[4]][4:5]) # Si calcul par bootstrap
    med <- median(x, na.rm = na.rm)
    
    if (is.na(med)) { # Si mediane = NA
      r2 <- rep(NA, 3) # Mediane, IC inf et IC sup a NA
    } else { # Sinon
      r2 <- c(median = med, r) # Concatenation mediane et IC
      if (length(r2) == 2) {
        r2 <- c(r2, NA) # Si erreur dans le calcul d'IC et retour d'une seule borne
        message("Sample is too small to compile median CI and NA's are produced")
      }
    }
    names(r2) <- c("median", "lwr.ci", "upr.ci")
    
    switch(sides, # Ajustement des bornes dans les hypotheses bilaterales
           left = {r2[3] <- Inf},
           right = {r2[2] <- -Inf})
    return(r2)
  }
  
  IC_med_binom <- function(x, conf.level = 0.95, alternative = c("two.sided", "greater", "less"), na.rm = TRUE) {
    if (na.rm) x <- na.omit(x)
    n <- length(x)
    switch(match.arg(alternative),
           two.sided = {
             k <- qbinom(p = (1 - conf.level)/2, size = n, prob = 0.5, lower.tail = TRUE)
             ci <- sort(x)[c(k, n - k + 1)]
             attr(ci, "conf.level") <- 1 - 2 * pbinom(k - 1, size = n, prob = 0.5)},
           greater = {
             k <- qbinom(p = (1 - conf.level), size = n, prob = 0.5, lower.tail = TRUE)
             ci <- c(sort(x)[k], Inf)
             attr(ci, "conf.level") <- 1 - pbinom(k - 1, size = n, prob = 0.5)},
           less = {
             k <- qbinom(p = conf.level, size = n, prob = 0.5, lower.tail = TRUE)
             ci <- c(-Inf, sort(x)[k])
             attr(ci, "conf.level") <- pbinom(k, size = n, prob = 0.5)
           })
    return(ci)
  }
  
  # Fonction de calcul d'IC pour une moyenne
  IC_moy <- function(x, sd = NULL, trim = 0, conf.level = 0.95, sides = c("two.sided", "left", "right"), na.rm = TRUE , conf.method.num = c("classic", "boot"),...) {
    winvar <- function(x, trim) {
      n <- length(x)
      trn <- floor(trim * n) + 1
      minval <- sort(x, partial = trn)[trn]
      maxval <- sort(x, partial = max((n - trn + 1), 1))[max((n - trn + 1), 1)]
      winvar <- var(Winsorize(x, minval = minval, maxval = maxval))
      DF <- n - 2 * (trn - 1) - 1
      return(c(var = winvar, DF = DF))
    }
    # Matching des arguments
    sides <- match.arg(sides, choices = c("two.sided", "left", "right"), several.ok = FALSE)
    conf.method.num <- match.arg(conf.method.num, choices=c("classic", "boot"))
    if (sides != "two.sided") conf.level <- 1 - 2 * (1 - conf.level)
    if (na.rm) x <- na.omit(x)
    
    switch(conf.method.num,
           classic = {
             if (trim != 0) {
               wvar <- winvar(x, trim)
               se <- sqrt(wvar["var"])/((1 - 2 * trim) * sqrt(length(x)))
               res <- mean(x, trim = trim) + c(0, -1, 1) * qt(1 - (1 - conf.level)/2, wvar["DF"]) * se
               names(res) <- c("mean", "lwr.ci", "upr.ci")
             }
             else {
               if(is.null(sd)) {
                 a <- qt(p = (1 - conf.level)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
               } else {
                 a <- qnorm(p = (1 - conf.level)/2) * sd/sqrt(length(x))
               }
               res <- c(mean = mean(x), lwr.ci = mean(x) + a, upr.ci = mean(x) - a)
             }
           },
           boot = {
             btype <- InDots(..., arg = "type", default = "basic")
             if (trim != 0) {
               boot.fun <- boot::boot(x, function(x, i) { # Fonction retournant moyenne et variance?
                 m <- mean(x[i], na.rm = FALSE, trim = trim)
                 n <- length(i)
                 v <- winvar(x, trim)/((1 - 2 * trim) * sqrt(length(x)))^2
                 c(m, v)
               }, R = InDots(..., arg = "R", default = 999), parallel = InDots(..., arg = "parallel", default = "no"))
             }
             else {
               boot.fun <- boot::boot(x, function(x, i) { # Fonction retournant moyenne et variance?
                 m <- mean(x[i], na.rm = FALSE)
                 n <- length(i)
                 v <- (n - 1) * var(x[i])/n^2
                 c(m, v)
               }, R = InDots(..., arg = "R", default = 999), parallel = InDots(..., arg = "parallel", default = "no"))
             }
             
             ci <- boot.ci(boot.fun, conf = conf.level, type = btype)
             if (btype == "norm") {
               return(c(mean = boot.fun$t0[1], lwr.ci = ci[[4]][2], upr.ci = ci[[4]][3]))
             } else {
               return(c(mean = boot.fun$t0[1], lwr.ci = ci[[4]][4], upr.ci = ci[[4]][5]))
             }
           })
    
    switch(sides, # Ajustement des bornes dans les hypotheses bilaterales
           left = {r2[3] <- Inf},
           right = {r2[2] <- -Inf})
    return(res)
  }
  
  IC_tab <- function(x, conf.level = 0.95, conf.method.cat = conf.method.cat) {
    .moments <- function(c, lambda) {
      a <- lambda + c
      b <- lambda - c
      if (b < 0) b <- 0L
      if (b > 0) den <- ppois(a, lambda) - ppois(b - 1, lambda)
      if (b == 0) den <- ppois(a, lambda)
      mu <- mat.or.vec(4, 1)
      mom <- mat.or.vec(5, 1)
      for (r in 1:4) {
        poisA <- 0L
        poisB <- 0L
        if ((a - r) >= 0) {
          poisA <- ppois(a, lambda) - ppois(a - r, lambda)
        } else {
          poisA <- ppois(a, lambda)
        }
        if ((b - r - 1) >= 0) poisB <- ppois(b - 1, lambda) - ppois(b - r - 1, lambda)
        if ((b - r - 1) < 0 && (b - 1) >= 0) poisB <- ppois(b - 1, lambda)
        if ((b - r - 1) < 0 && (b - 1) < 0) poisB <- 0
        mu[r] <- (lambda^r) * (1 - (poisA - poisB)/den)
      }
      mom[1] <- mu[1]
      mom[2] <- mu[2] + mu[1] - mu[1]^2
      mom[3] <- mu[3] + mu[2] * (3 - 3 * mu[1]) + (mu[1] - 3 * mu[1]^2 + 2 * mu[1]^3)
      mom[4] <- mu[4] + mu[3] * (6 - 4 * mu[1]) + mu[2] * (7L - 12 * mu[1] + 6 * mu[1]^2) + mu[1] - 4 * mu[1]^2 + 6 * mu[1]^3 - 3 * mu[1]^4
      mom[5] <- den
      return(mom)
    }
    .truncpoi <- function(c, x, n, k) {
      m <- matrix(0L, k, 5)
      for (i in 1:k) {
        lambda <- x[i]
        mom <- .moments(c, lambda)
        for (j in 1:5) {
          m[i, j] <- mom[j]
        }
      }
      for (i in 1:k) {
        m[i, 4] <- m[i, 4] - 3 * m[i, 2]^2
      }
      s <- colSums(m)
      z <- (n - s[1])/sqrt(s[2])
      poly <- 1 + (s[3]/(s[2]^(3/2))) * (z^3 - 3 * z)/6 + (s[4]/(s[2]^2)) * (z^4 - 6 * z^2 + 3)/24 + (s[3]/(s[2]^(3/2)))^2 * (z^6 - 15 * z^4 + 45 * z^2 - 15)/72
      probx <- 1
      for (i in 1:k) {
        probx <- probx * m[i, 5]
      }
      return((1/(ppois(n, n) - ppois(n - 1, n))) * probx * (poly * exp(-z^2/2)/(sqrt(2) * gamma(0.5)))/sqrt(s[2]))
    }
    n <- sum(x, na.rm = TRUE)
    k <- length(x)
    p <- x/n
    
    conf.method.cat <- match.arg(arg = conf.method.cat, choices = c("waldcc", "sisonglaz", "cplus1", "goodman", "wald", "wilson","boot"))
    switch(conf.method.cat,
           goodman = {
             q.chi <- qchisq(conf.level, k - 1)
             lci <- (q.chi + 2 * x - sqrt(q.chi * (q.chi + 4 * x * (n - x)/n)))/(2 * (n + q.chi))
             uci <- (q.chi + 2 * x + sqrt(q.chi * (q.chi + 4 * x * (n - x)/n)))/(2 * (n + q.chi))
             res <- cbind(est = p, lwr.ci = pmax(0L, lci), upr.ci = pmin(1, uci))
           },
           wald = {
             q.chi <- qchisq(conf.level, 1)
             lci <- p - sqrt(q.chi * p * (1 - p)/n)
             uci <- p + sqrt(q.chi * p * (1 - p)/n)
             res <- cbind(est = p, lwr.ci = pmax(0L, lci), upr.ci = pmin(1, uci))
           },
           waldcc = {
             q.chi <- qchisq(conf.level, 1)
             lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2 * n)
             uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2 * n)
             res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, uci))
           },
           wilson = {
             q.chi <- qchisq(conf.level, 1)
             lci <- (q.chi + 2 * x - sqrt(q.chi^2 + 4 * x * q.chi * (1 - p)))/(2 * (q.chi + n))
             uci <- (q.chi + 2 * x + sqrt(q.chi^2 + 4 * x * q.chi * (1 - p)))/(2 * (q.chi + n))
             res <- cbind(est = p, lwr.ci = pmax(0L, lci), upr.ci = pmin(1, uci))
           },
           sisonglaz = {
             const <- 0L
             pold <- 0L
             for (cc in 1:n) {
               poi <- .truncpoi(cc, x, n, k)
               if (poi > conf.level && pold < conf.level) {
                 const <- cc
                 break
               }
               pold <- poi
             }
             const <- const - 1
             res <- cbind(est = p, lwr.ci = pmax(0L, p - const/n), upr.ci = pmin(1, p + const/n + 2 * ((conf.level - pold)/(poi - pold))/n))
           },
           cplus1 = {
             const <- 0L
             pold <- 0L
             for (cc in 1:n) {
               poi <- .truncpoi(cc, x, n, k)
               if (poi > conf.level && pold < conf.level) {
                 const <- cc
                 break
               }
               pold <- poi
             }
             const <- const - 1
             res <- cbind(est = p, lwr.ci = pmax(0L, p - const/n - 1/n), upr.ci = pmin(1, p + const/n + 1/n))
           },
           boot = {
             dt <- rep(names(x), times = x)
             
             getCI <- function(x,w) {
               b1 <- boot.ci( x , index = w , conf = conf.level , type = "basic")
               ## extract info for all CI types
               tab <- t(sapply(b1[-(1:3)],function(x) tail(c(x),2)))
               ## combine with metadata: CI method, index
               tab <- cbind(w,rownames(tab),as.data.frame(tab))
               colnames(tab) <- c("index","method","lwr","upr")
               tab
             }
             
             boot.out <- boot::boot( dt , function(.x,d){ (.x[d] %>% table/n)  }  , R = 999)
             if ( sum(x!=0) == 1) {
               res = tibble(est = x/n , lwr.ci= x/n, upr.ci = x/n)
             } else {
               res <- do.call(rbind,lapply(1:sum(x != 0),getCI,x=boot.out)) %>%
                 mutate(est = x/n) %>% rename( lwr.ci = "lwr" , upr.ci = "upr") %>% select(est,lwr.ci,upr.ci)
             }
             rownames(res) = names(x)
             res = as.matrix(res)
           }
    )
    return(res)
  }
  
  test_binom_med <- function (x, y = NULL, conf.level = 0.95, alternative = c("two.sided", "greater", "less"), mu = 0L,  ...) {
    alternative <- match.arg(alternative, choices = c("two.sided", "greater", "less"), several.ok = FALSE)
    
    if (!missing(mu) && ((length(mu) > 1) || !is.finite(mu)))
      stop("'mu' must be a single number")
    if (!((length(conf.level) == 1) && is.finite(conf.level) && (conf.level > 0L) && (conf.level < 1)))
      stop("'conf.level' must be a single number between 0 and 1")
    if (!is.numeric(x))
      stop("'x' must be numeric")
    if (!is.null(y)) {
      if (!is.numeric(y))
        stop("'y' must be numeric")
      if (length(x) != length(y))
        stop("'x' and 'y' must have the same length")
      DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
      OK <- complete.cases(x, y)
      x <- x[OK]
      y <- y[OK]
      METHOD <- "Dependent-samples Sign-Test"
      x <- (x - y)
    } else {
      DNAME <- deparse(substitute(x))
      x <- x[is.finite(x)]
      METHOD <- "One-sample Sign-Test"
    }
    d <- (x - mu)
    n.valid <- sum(d > 0L) + sum(d < 0L)
    if (n.valid > 0L) {
      RVAL <- binom.test(x = sum(d > 0L), n = n.valid, p = 0.5,
                         alternative = alternative, conf.level = conf.level)
    }else {
      RVAL <- binom.test(x = 1, n = 1)
    }
    RVAL$method <- METHOD
    RVAL$data.name <- DNAME
    names(mu) <- if (!is.null(y)) {"median difference"} else {"median"}
    names(RVAL$statistic) <- "S"
    RVAL$estimate <- median(d + mu, na.rm = TRUE)
    names(RVAL$parameter) <- "number of differences"
    mci <- IC_med_binom(d + mu, conf.level = conf.level, alternative = alternative, na.rm = TRUE)
    RVAL$conf.int <- mci
    attr(RVAL$conf.int, "conf.level") = round(attr(mci, "conf.level"), 3)
    names(RVAL$estimate) <- "median of the differences"
    RVAL$null.value <- mu
    class(RVAL) <- "htest"
    return(RVAL)
  }
  
  #### Definition des fonctions de tri croise 
  # Creation de tableau de descriptifs pour une seule variable X
  # Trois fonctions, une pour les facteurs, et deux pour les quantitatives (moyenne et mediane)
  # Ces fonctions sont ensuite appelles dans un "map" pour etre appliquees sur toutes les variables X
  
  # Tableau descriptifs pour la moyenne d'une variable quantitative
  tab.res.moy <- function(x, y, weights, conf.level, decimal, mean.test, include.p, conf.method.num = conf.method.num ){
    if (is.defined(weights)) {
      temp3 <- formatC(round(sum(weights, na.rm = TRUE), decimal), digits = decimal, format = "f")
      x <- x*weights
    } else {
      temp3 <- length(na.omit(x))
    }
    
    # Calcul sur l'ensemble de la colonnes
    temp1 <- formatC(round(IC_moy(x, conf.level = conf.level, conf.method.num = conf.method.num), decimal), digits = decimal, format = "f")
    temp4 <- data.frame(Var = ", mean (sd)",
                        N = temp3,
                        n = temp1[1],
                        p = paste0("(", formatC(round(sd(x, na.rm = TRUE), decimal), digits = decimal, format = "f"), ")"),
                        IC = paste0("[", temp1[2], "-", temp1[3], "]"),
                        minmax = paste0("{", min(x, na.rm = TRUE), "-", max(x, na.rm = TRUE), "}"),
                        stringsAsFactors = FALSE)
    temp4$N <- as.character(temp4$N)
    temp4$n <- as.character(temp4$n)
    
    list.res <- list()
    for (kk in 1:nlevels(y)) { # Boucle sur chaque modalite de facteur
      aux1 <- x[which(y == levels(y)[kk])]
      aux2 <- formatC(round(IC_moy(aux1, conf.level = conf.level), decimal), digits = decimal, format = "f")
      #aux3 <- formatC(round(sd(aux1, na.rm = TRUE), decimal), digits = decimal, format = "f")
      aux4 <- data.frame(n = aux2[1],
                         p = paste0("(", formatC(round(sd(aux1, na.rm = TRUE), decimal), digits = decimal, format = "f"), ")"),
                         IC = paste0("[", aux2[2], "-", aux2[3], "]"),
                         minmax = paste0("{", min(aux1, na.rm = TRUE), "-", max(aux1, na.rm = TRUE), "}"),
                         stringsAsFactors = FALSE)
      colnames(aux4) <- paste0(levels(y)[kk], c("_n", "_p", "_IC", "_minmax"))
      list.res[[kk]] <- aux4
    }
    res <- bind_cols(list.res)
    
    if (include.p) {
      switch(mean.test, # Choix du test sur la moyenne
             test = { # Choix du test par algorithem
               if (bartlett.test(x, y)$p.value >= 0.05 | is.na(bartlett.test(x, y)$p.value)) { # Si bartlett significatif
                 if (length(table(y)) == 2) {
                   p <- t.test(x ~ as.factor(y), var.equal = TRUE)$p.value
                   tn <- "Student"
                 } else {
                   p <- anova_test(data = data.frame(x = x, y = y), formula = x ~ y)$p
                   tn <- "Anova"
                 }
               } else { # Si bartlett non significatif
                 p <- kruskal.test(x, y)$p.value
                 if (length(table(y)) == 2) {
                   tn <- "Wilcoxson"
                 } else {
                   tn <- "Kruskal-Wallis"
                 }
               }
             },
             student = { # test de student
               if (length(table(y)) == 2) {
                 p <- t.test(x ~ as.factor(y), var.equal = TRUE)$p.value
                 tn <- "Student"
               } else {
                 p <- anova_test(data = data.frame(x = x, y = y), formula = x ~ y)$p
                 tn <- "Anova"
               }
             },
             kruskal = { # Kruskal
               p <- kruskal.test(x, y)$p.value
               if (length(table(y)) == 2) {
                 tn <- "Wilcoxson"
               } else {
                 tn <- "Kruskal-Wallis"
               }
             })
    } else { # Si include.p = FALSE, pas de pval ni de test
      p <- "-"
      tn <- "-"
    }
    bind_cols(temp4, res, data.frame(pval = as.character(p)), data.frame(test = tn))
  }
  
  # Tableau descriptifs pour la mediane d'une variable quantitative
  tab.res.med <- function(x, y, conf.level, decimal, include.p,...) {
    # Calcul pour toute la colonnes
    temp1 <- formatC(round(IC_med(x, conf.level = conf.level), decimal), digits = decimal, format = "f")
    temp2 <- formatC(round(quantile(x, c(.25, .75), na.rm = TRUE), decimal), digits = decimal, format = "f")
    temp4 <- data.frame(Var = ", med (iiq)",
                        N = length(na.omit(x)),
                        n = temp1[1],
                        p = paste0("(", temp2[1], "-", temp2[2], ")"),
                        IC = paste0("[", temp1[2], "-", temp1[3], "]"),
                        minmax = paste0("{", min(x, na.rm = TRUE), "-", max(x, na.rm = TRUE), "}"),
                        stringsAsFactors = FALSE)
    temp4$N <- as.character(temp4$N)
    temp4$n <- as.character(temp4$n)
    
    list.res <- list()
    for (kk in 1:nlevels(y)) { # Calcul par modalite du facteur
      aux1 <- x[which(y == levels(y)[kk])] # Sous echantillon
      aux2 <- formatC(round(IC_med(aux1, conf.level = conf.level), decimal), digits = decimal, format = "f")
      aux3 <- formatC(round(quantile(aux1, c(.25, .75), na.rm = TRUE), decimal), digits = decimal, format = "f")
      aux4 <- data.frame(n = aux2[1],
                         p = paste0("(", aux3[1], "-", aux3[2], ")"),
                         IC = paste0("[", aux2[2], "-", aux2[3], "]"),
                         minmax = paste0("{", min(aux1, na.rm = TRUE), "-", max(aux1, na.rm = TRUE), "}"),
                         stringsAsFactors = FALSE)
      colnames(aux4) <- paste0(levels(y)[kk], c("_n", "_p", "_IC", "_minmax"))
      list.res[[kk]] <- aux4
    }
    res <- bind_cols(list.res)
    
    if (include.p) {
      p <- kruskal.test(x, y)$p.value
      if (length(table(y)) == 2) {tn <- "Wilcoxson"} else {tn <- "Kruskal-Wallis"}
    } else {
      p <- "-"
      tn <- "-"
    }
    bind_cols(temp4, res,data.frame(pval = as.character(p)), data.frame(test = tn))
  }
  
  # Tableau descriptif pour une variable qualitative
  tab.res.fac <- function(weights, x, y, conf.level, conf.method.cat, decimal, simulate.p.value, chi.correct, prop.type, prop.test, include.p,...) {
    if (is.defined(weights)) {
      temp1 <- formatC(round(IC_tab(aggregate(w, list(x), sum)$x, conf.level = conf.level, conf.method.cat = conf.method.cat)*100L, decimal), digits = decimal, format = "f")
      rownames(temp1) <- names(table(x))
      temp2 <- formatC(round(aggregate(w, list(x), sum)$x, decimal), digits = 0L, format = "f")
      temp3 <- formatC(round(sum(weights, na.rm = TRUE), decimal), digits = decimal, format = "f")
    } else {
      temp1 <- formatC(round(IC_tab(table(x), conf.level = conf.level, conf.method.cat = conf.method.cat)*100L, decimal), digits = decimal, format = "f")
      temp2 <- formatC(round((table(x)), decimal), digits = 0L, format = "f")
      temp3 <- length(na.omit(x))
    }
    
    temp4 <- data.frame(Var = c("", paste0("  ", rownames(temp1))),
                        N = c(temp3, rep("", length(temp2))),
                        n = c("", temp2),
                        p = c("", paste0("(", temp1[, 1], ")")),
                        IC = c("", paste0("[", paste0(temp1[, 2], "-", temp1[, 3]), "]")),
                        minmax = c(paste0("{", nlevels(x), " val}"), rep("", length(temp2))),
                        stringsAsFactors = FALSE)
    rownames(temp4) <- NULL
    temp4$N <- as.character(temp4$N)
    temp4$n <- as.character(temp4$n)
    
    if (is.defined(weights)) {
      aux1 <- as.data.frame(matrix(aggregate(w, list(x, y), sum)$x, ncol = length(table(y))))
    } else {
      aux1 <- table(x, y)
    }
    
    colnames(aux1) <- names(table(y))
    rownames(aux1) <- names(table(x))
    
    list.pourcent <- list() # Creation de la liste de sortie
    switch(prop.type, # Calcul des pourcentage
           "col.percent" = { # En colonnes
             for (kk in 1:nlevels(y)) { # Boucle sur les colonnes
               aux0 <- formatC(round(IC_tab((aux1[, kk]), conf.level = conf.level, conf.method.cat = conf.method.cat)*100L, decimal), digits = decimal, format = "f")
               aux0 <- data.frame( # Calcul des % et IC sur chaque colonne
                 p = paste0("(", aux0[, 1], ")"),
                 IC = paste0("[", aux0[, 2], "-", aux0[, 3], "]"),
                 minmax = "",
                 stringsAsFactors = FALSE)
               colnames(aux0) <- paste0(levels(y)[kk], "_", colnames(aux0))
               list.pourcent[[kk]] <- aux0 # Remplissage de la liste de sortie
             }
             p.ic <- bind_cols(list.pourcent) # Passage en data.frame
           },
           "row.percent" = { # En lignes
             for (kk in 1:nlevels(x)) { # Boucle sur les lignes
               aux0 <- formatC(round(IC_tab(t(aux1)[, kk], conf.level = conf.level, conf.method.cat = conf.method.cat)*100L, decimal), digits = decimal, format = "f")
               aux0 <- data.frame(p = paste0("(", aux0[, 1], ")"), # Calcul des % et IC sur chaque ligne
                                  IC = paste0("[", aux0[, 2], "-", aux0[, 3], "]"),
                                  minmax = "",
                                  stringsAsFactors = FALSE)
               list.pourcent[[kk]] <- aux0 # Remplissage de la liste de sortie
             }
             for (kk in 1:length(list.pourcent)) { # Concatenation
               to.make <- list.pourcent[[kk]][1, ]
               for (mm in 2:nlevels(y)) { # On ne le fait pas pour la premiere colonnes : le total
                 to.make <- bind_cols(to.make, list.pourcent[[kk]][mm, ])
               }
               list.pourcent[[kk]] <- to.make # Remplissage de la liste de sortie
               p.ic <- bind_rows(list.pourcent) # Passage en data.frame
             }
             colnames(p.ic) <- unlist(lapply(levels(y), function(x) paste0(x, "_", c("p", "IC", "minmax"))))
           },
           "tot.percent" = { # Pour le total
             aux0 <- formatC(round(IC_tab(as.vector(aux1), conf.level = conf.level, conf.method.cat = conf.method.cat)*100L, decimal), digits = decimal, format = "f")
             aux0 <- data.frame( # Calcul des % et IC sur chaque tout le tableau de contingence
               p = paste0("(", aux0[, 1], ")"),
               IC = paste0("[", aux0[, 2], "-", aux0[, 3], "]"),
               minmax = "",
               stringsAsFactors = FALSE)
             aux0 <- matrix(data = c(aux0$p, aux0$IC)[rep(c(1:nlevels(x), (1:nlevels(x)) + (nlevels(x)*nlevels(y))), nlevels(y)) +
                                                        rep((0L:(nlevels(y) - 1))*nlevels(x), each = nlevels(x)*2)],
                            ncol = nlevels(y)*2,
                            byrow = FALSE) # Creation du data.frame de sortie
             colnames(aux0) <- paste0(rep(levels(y), each = 2), "_", c("p", "IC", "minmax"))
             p.ic <- bind_cols(aux0)
           })
    
    aux11 <- as.data.frame.matrix(aux1, stringsAsFactors = FALSE) #Conversion du tableau de contingence en chaine de caracteres
    if (ncol(aux11) > 1) {
      aux11[, unlist(lapply(aux11, is.integer))] <- apply(aux11[, unlist(lapply(aux11, is.integer))], 2, as.character) # Conversion des entier
      aux11[, unlist(lapply(aux11, is.numeric))] <- apply(aux11[, unlist(lapply(aux11, is.numeric))], 2, as.character) # Conversion des strings
    }
    if (ncol(aux11) == 1) {
      aux11[, unlist(lapply(aux11, is.integer))] <- as.character(aux11[, unlist(lapply(aux11, is.integer))])
      aux11[, unlist(lapply(aux11, is.numeric))] <- as.character(aux11[, unlist(lapply(aux11, is.numeric))])
    }
    
    
    colnames(aux11) <- paste0(colnames(aux11), "_n") # Ajout du tag "_n"
    p.ic <- bind_cols(aux11, p.ic) # Concatenation u detail par modalite de facteur
    
    ligne.vide <- data.frame(matrix(rep("", ncol(p.ic)), nrow = 1)) # création de la ligne vide d'entete
    colnames(ligne.vide) <- colnames(p.ic) # Correspondance des noms de colonnes pour la fusion
    ligne.vide[1, str_detect(colnames(ligne.vide), "_minmax")] <- # Pour les colonnes minmax
      paste0("{", apply(table(x, y), 2, function(x) {sum(x != 0L)}), " val}") # Decompte du nb de modalite par facteur
    p.ic <- bind_rows(ligne.vide, p.ic) # Fusion ligne vide et tableau de sortie
    
    if (include.p) { # Tests sur le tableau de contingence
      switch(prop.test,
             "chi" = { # Test de Chi2
               test <- tryCatch(chisq.test(aux1, correct = chi.correct, simulate.p.value = simulate.p.value), warning = function(w) w, error = function(e) e)
               if (any(class(test) %in% c("error", "warning"))) { # En cas d'echec du chi2
                 print(test$message) # Erreur, faire un fisher
                 stop("Le test de Chi-2 ne peut pas être mis en oeuvre. Appelez la fonction avec prop.test = 'fisher'")
               }
               tn <- "Chi2" # Nom du test
             },
             "fisher" = { # Test de fisher
               test <- tryCatch(fisher.test(aux1, simulate.p.value = simulate.p.value), warning = function(w) w, error = function(e) e)
               if (any(class(test) %in% c("error", "warning"))) { # Si erreur, pas de retour de test
                 print(test$message)
                 pvalue <- "-" # Fait pour empecher le crash de la fonction
               }
               tn <- "Fisher" # Nom du test
             },
             "test" = { # Algo de decision de test
               test <- tryCatch(chisq.test(aux1, correct = chi.correct, simulate.p.value = simulate.p.value), warning = function(w) w, error = function(e) e) # On commence par un chi2
               if (any(class(test) %in% c("error", "warning"))) { # Si le chi2 n'aboutit pas
                 test <- tryCatch(fisher.test(aux1, simulate.p.value = simulate.p.value), warning = function(w) w, error = function(e) e) # On fait un fisher
                 if (any(class(test) %in% c("error", "warning"))) { # Si fisher n'aboutit pas
                   print(test$message) # On ne retourne rien
                   pvalue <- "-"
                   tn <- "-"
                 } else {
                   tn <- "Fisher" # Si fisher
                 }
               } else {
                 tn <- "Chi2" # Si chi2
               }
             })
      pvalue <- test$p.value
    } else { # Si include.p = FALSE, pas de test
      pvalue <- "-"
      tn <- "-"
    }
    bind_cols(temp4, p.ic,
              data.frame(pval = c(pvalue, rep("", nrow(temp4) - 1))),
              data.frame(test = c(tn, rep("", nrow(temp4) - 1))))
  }
  
  #### Definition Fonction Map 
  # Detection et gestion des types de la variables X en entree
  # Puis application des fonctions tab.res.moy, tab.res.med ou tab.res.fac selon le type de X et l'argument num.type
  # Cette fonction est ensuite appliquee dans un map a toutes les variables X
  tri_croise_map_fun <- function(vars, factor, weights, decimal, conf.level, num.type, prop.type, prop.test, chi.correct, simulate.p.value,
                                 mean.test, conf.method.cat, conf.method.num, na.omit, na.str.default, na.str.value, na.num.default, na.num.value, include.p) {
    # Gestion de la ponderation
    if (is.defined(weights)) {
      data.temp <- bind_cols(data.frame(y = factor), data.frame(x = vars), data.frame(w = weights)) %>% drop_na()
      factor <- data.temp$factor
      vars <- data.temp$x
      w <- data.temp$w
    }
    
    # Conversion des types
    y <- factor # Pour la variable facteur, conversion en factor
    if (is.vector(factor)) y <- as.factor(y)
    if (is.tbl(factor)) y <- as.factor(pull(y))
    x <- vars # Pour la variable x
    if (is.difftime(x)) x <- as.numeric(x) # On traite les difftime comme des numeriques
    if (is.character(x)) x <- as.factor(x) # On traite les difftime comme des numeriques
    
    # Gestion des valeurs manquantes
    if (!na.omit) { # Si na.omit = FALSE, on prend on compte les valeurs maquantes
      if (is.numeric(x)) { # Pour les variabes numerique
        switch(na.num.default,
               none = { # Si aucun remplacement, on ignore simplement les valeur manquantes
                 cond <- !is.na(x) & !is.na(y)
                 y <- y[cond]
                 x <- x[cond]
               },
               mean = {x[is.na(x)] <- mean(x, na.rm = TRUE)}, # Attribution de la moyenne
               med = {x[is.na(x)] <- median(x, na.rm = TRUE)}, # Attribution de la mediane
               value = {x[is.na(x)] <- na.num.value}) # On remplace les valeur manquantes par la valeur par defaut
      }
      if (is.factor(x)) { # Pour les variables quali
        x <- as.character(x) # Conversion en character pour pouvoir ajouter une modalite de facteur
        x[is.na(x)] <- switch(na.str.default,
                              missing = ifelse(lang == "fr", "Donnée manquante", "Missing value"), # Création modalite valeur manquante
                              mode = names(rev(sort(table(x))))[1], # Attribution de la modalite la plus frequente
                              value = na.str.value) # On remplace les valeur manquantes par la valeur par defaut
        x <- as.factor(x) # Reconversion en facteur une fois la nouvelle modalite ajoutee
      }
    }
    # Création du tableau de sortie selon le type de variable et le statistique choisie
    # Appels des fonctios de creation de tableau
    if (is.numeric(x)) {
      switch(num.type,  # Appelle des fonctions correspondante selon "num.type"
             mean = {return(tab.res.moy(x = x, y = y, weights = weights, conf.level = conf.level, decimal = decimal, mean.test = mean.test, include.p = include.p, conf.method.num = conf.method.num))},
             med = {return(tab.res.med(x = x, y = y, conf.level = conf.level, decimal = decimal, include.p = include.p, conf.method.num = conf.method.num))},
             both = {return(bind_rows( # concatenation des deux precedentes
               tab.res.moy(x = x, y = y, weights = weights, conf.level = conf.level, decimal = decimal, mean.test = mean.test, include.p = include.p, conf.method.num = conf.method.num),
               tab.res.med(x = x, y = y, conf.level = conf.level, decimal = decimal, include.p = include.p, conf.method.num = conf.method.num)
             ))})
    }
    if (is.factor(x)) {
      return(tab.res.fac(weights = weights, x = x, y = y, conf.level = conf.level, conf.method.cat = conf.method.cat, decimal = decimal, simulate.p.value = simulate.p.value,
                         chi.correct = chi.correct, prop.type = prop.type, prop.test = prop.test, include.p = include.p))
    }
  }
  
  # Code Fonction -
  # Application de la fonciton de descrptif a toutes les variables X en utilisant un "map"
  
  if (is.defined(weights)) {
    weights <- as.numeric(data[, weights])
  }
  if (is.defined(factor)) { # Si une variable stratifiante est renseignee
    data <- data[, unique(c(factor, vars))] # Sélection de variables
    
    if (is.vector(data[, unlist(lapply(data, is.integer))])) data[, unlist(lapply(data, is.integer))] <- as.numeric(data[, unlist(lapply(data, is.integer))]) # Conversion des entier
    if (dim(data[, unlist(lapply(data, is.integer))])[2] > 1) data[, unlist(lapply(data, is.integer))] <- apply(data[, unlist(lapply(data, is.integer))], 2, as.numeric) # Conversion des entier
    if (is.vector(data[, unlist(lapply(data, is.character))])) data[, unlist(lapply(data, is.character))] <- as.factor(data[, unlist(lapply(data, is.character))]) # Conversion des strings
    if (dim(data[, unlist(lapply(data, is.character))])[2] > 1) data[, unlist(lapply(data, is.character))] <- apply(data[, unlist(lapply(data, is.character))], 2, as.factor) # Conversion des strings
    
    
    if (length(vars) == 1) { # Si une seule variable explicative, on lance la fonction une seule fois
      res <- list(tri_croise_map_fun(factor = data[, factor], vars = data[, vars], weights = weights, decimal = decimal, prop.type = prop.type, prop.test = prop.test,
                                     mean.test = mean.test, chi.correct = chi.correct, num.type = num.type, conf.level = conf.level, conf.method.num = conf.method.num, conf.method.cat=conf.method.cat,
                                     simulate.p.value = simulate.p.value, na.omit = na.omit, na.str.default = na.str.default, na.num.default = na.num.default,
                                     na.str.value = na.str.value, na.num.value = na.num.value, include.p = include.p))
      names(res) <- vars
    } else { # Sinon, map sur toutes "vars"
      ## Begin debug
      # browser()
      # for(i in 1:5){
      #
      # }
      # tri_croise_map_fun(data[, vars[1]], factor = data[, factor], weights = weights, decimal = decimal, prop.type = prop.type, prop.test = prop.test,
      #            mean.test = mean.test, chi.correct = chi.correct, num.type = num.type, conf.level = conf.level, conf.method.cat = conf.method.cat, conf.method.num = conf.method.num,
      #            simulate.p.value = simulate.p.value, na.omit = na.omit, na.str.default = na.str.default, na.num.default = na.num.default,
      #            na.str.value = na.str.value, na.num.value = na.num.value, include.p = include.p)
      # end debug
      res <- purrr::map(data[, vars], tri_croise_map_fun, factor = data[, factor], weights = weights, decimal = decimal, prop.type = prop.type, prop.test = prop.test,
                        mean.test = mean.test, chi.correct = chi.correct, num.type = num.type, conf.level = conf.level, conf.method.cat = conf.method.cat, conf.method.num = conf.method.num,
                        simulate.p.value = simulate.p.value, na.omit = na.omit, na.str.default = na.str.default, na.num.default = na.num.default,
                        na.str.value = na.str.value, na.num.value = na.num.value, include.p = include.p)
    }
  } else { # Si pas de variable stratifiante, le descriptif est univarie
    data <- data[, vars] # Sélection de variables
    
    if (is.vector(data[, unlist(lapply(data, is.integer))])) data[, unlist(lapply(data, is.integer))] <- as.numeric(data[, unlist(lapply(data, is.integer))]) # Conversion des entier
    if (dim(data[, unlist(lapply(data, is.integer))])[2] > 1) data[, unlist(lapply(data, is.integer))] <- apply(data[, unlist(lapply(data, is.integer))], 2, as.numeric) # Conversion des entier
    if (is.vector(data[, unlist(lapply(data, is.character))])) data[, unlist(lapply(data, is.character))] <- as.factor(data[, unlist(lapply(data, is.character))]) # Conversion des strings
    if (dim(data[, unlist(lapply(data, is.character))])[2] > 1) data[, unlist(lapply(data, is.character))] <- apply(data[, unlist(lapply(data, is.character))], 2, as.factor) # Conversion des strings
    
    data[, "factor"] <- "Tout" # Creation d'une variable constante en substitut de factor
    factor <- "factor" # definition du parametre
    
    res <- purrr::map(data[, vars], tri_croise_map_fun, factor = data[, factor], weights = weights, decimal = decimal, prop.type = prop.type,
                      chi.correct = FALSE, num.type = num.type, conf.level = conf.level, conf.method.cat = conf.method.cat, conf.method.num = conf.method.num,
                      simulate.p.value = FALSE, na.omit = na.omit, na.str.default = na.str.default, na.num.default = na.num.default,
                      na.str.value = na.str.value, na.num.value = na.num.value, include.p = include.p)
  }
  
  # Ajout taux d'incidence 
  # Si factor == "factor" --> signifie que le descriptif est univarié, donc le traitement est fait une seule fois
  # Sinon, le même traitement est mis dans un boucle sur le nmobre de niveaux du facteur
  if (factor == "factor") {
    # Initialisation
    nam <- names(res)
    if (is.defined(pop.ref)) names(pop.ref)[1:2] <- c("factor", "Var")
    # Apply sur tous les element de la liste de descriptifs
    res <- lapply(res, function(x) {
      # Si pop.ref est renseigne, on calcul des taux
      if (x[1, 1] == "" & is.defined(pop.ref)) {
        res_inter <- x[, str_detect(colnames(x), "Var|_n")] # Selection de variable dans le descriptif
        res_inter$Var <- str_replace_all(res_inter$Var, "  ", "") # Ajustement des libelles
        tab_ti <- inner_join( # Jointure avec les effectifs
          res_inter, # Decompte des cas
          pop.ref[pop.ref$factor == nam[parent.frame()$i[]], -1], # Lignes correspondantes au noms de l'element de la liste
          by = "Var"
        )
        rates <- TI(as.numeric(tab_ti[,2]), tab_ti[, ncol(tab_ti)], decimal = decimal) # Calcul des taux avec IC
        x$`_TI` <- c("", formatC(rates$MR, digits = decimal, format = "f")) # Creation colonnes de taux
        x$`_TI_IC` <- c("", paste0("[", formatC(rates$CI$lower, digits = decimal, format = "f"), "-",
                                   formatC(rates$CI$upper, digits = decimal, format = "f"),"]")) # Creation colonne IC
      } else { # Sinon on assigne des valeurs vides
        x$`_TI` <- rep("", nrow(x)) # Creation colonnes de taux
        x$`_TI_IC` <- rep("", nrow(x)) # Creation colonne IC
      }
      colnames(x)[str_detect(colnames(x), "_TI")] <- paste0(rep(names(table(data[, factor])), each = 2), c(" TI", " TI_IC"))
      x
    })
  } else {
    # Initialisation
    nam <- names(res)
    if (is.defined(pop.ref)) names(pop.ref)[1:2] <- c("factor", "Var")
    # Apply sur tous les element de la liste de descriptifs
    res <- lapply(res, function(x) {
      # Si pop.ref est renseigne, on calcul des taux
      if (x[1, 1] == "" & is.defined(pop.ref)) {
        for (k in 1:length(table(data[, factor]))) {# Boucle sur les niveaux du facteur
          res_inter <- x[, str_detect(colnames(x), paste0("Var|^", names(table(data[, factor]))[k], "_n$"))] # Selection de variable dans le descriptif
          res_inter$Var <- str_replace_all(res_inter$Var, "  ", "") # Ajustement des libelles
          tab_ti <- inner_join( # Jointure avec les effectifs
            res_inter, # Decompte des cas
            pop.ref[pop.ref$factor == nam[parent.frame()$i[]], # Lignes correspondantes au noms de l'element de la liste
                    str_detect(colnames(pop.ref), paste0("Var|^", names(table(data[, factor]))[k], "$"))], # Colonnes correspondant au niveau du facteur
            by = "Var"
          )
          rates <- TI(as.numeric(tab_ti[,2]), tab_ti[, ncol(tab_ti)], decimal = decimal) # Calcul des taux avec IC
          x$`_TI` <- c("", formatC(rates$MR, digits = decimal, format = "f")) # Creation colonnes de taux
          x$`_TI_IC` <- c("", paste0("[", formatC(rates$CI$lower, digits = decimal, format = "f"), "-",
                                     formatC(rates$CI$upper, digits = decimal, format = "f"),"]")) # Creation colonne IC
          colnames(x)[str_detect(colnames(x), "_TI")] <- paste0(rep(names(table(data[, factor]))[k], each = 2), c(" TI", " TI_IC"))
        }
      } else { # Sinon on assigne des valeurs vides
        for (i in 1:length(table(data[, factor]))) { # Boucle sur les niveaux du facteur
          x$`_TI` <- rep("", nrow(x)) # Creation colonnes de taux
          x$`_TI_IC` <- rep("", nrow(x)) # Creation colonne IC
          colnames(x)[str_detect(colnames(x), "_TI")] <- paste0(rep(names(table(data[, factor]))[i], each = 2), c(" TI", " TI_IC"))
        }
      }
      x
    })
  }
  
  # Mise en page
  # Nommage des elements de la liste selon (label si dispo, noms de colonnes sinon)
  if (is.null(label)) {
    names(res) <- vars
  } else {
    names(res) <- label
  }
  
  if (is.vector(data[, factor]))  data[, factor] <- as.factor(data[, factor]) # Conversion de la variable factor si besoin
  if (is.tbl(data[, factor]))  data[, factor] <- as.factor(pull(data[, factor])) # Idem
  
  # Ordre des colonnes du tableau complet
  order.var <- c("Var", "N", "n", "p", "IC", "minmax", # Colonnes du descriptifs total
                 paste0(sort(factor(rep(names(table(data[, factor])), 6), # Colonnes par niveau de facteur
                                    levels = names(table(data[, factor])))),
                        c("_n", "_p", "_IC", "_minmax", " TI", " TI_IC")),
                 "pval", "test")
  order.var <- unname(order.var)
  
  # On met les 'labels' dans le tableau et on l'ordonne correctement
  for (i in 1:length(res)) {
    if (all(str_detect(res[[i]]$Var, "mean [(]sd[)]|med [(]iiq[)]"))) {
      res[[i]]$Var <- paste0(names(res[i]), res[[i]]$Var)
    }else{
      res[[i]]$Var[1] <- names(res[i])
    }
    res[[i]] <- res[[i]][order.var[order.var %in% colnames(res[[i]])]]
  }
  
  # Passage de "res" du format liste au format data.frame
  res <- unite(bind_rows(res), # Fusion des descriptifs de chaque variable
               "Total n (%)", c("n", "p"), sep = " ") #Fusion des effectifs et pourcentages
  # browser()
  if (is.factor(data[, factor])) {
    for (jj in levels(as.factor(data[, factor]))) { # Renommage des colonnes de la sortie
      res <-  unite(res, "col.to.change", c(paste0(jj, "_n"), paste0(jj, "_p")), sep = " ")
      colnames(res)[str_detect(colnames(res), "col.to.change")] <- paste0(jj, " n (%)")
    }
  } else {
    for (jj in levels(as.factor(pull(data[, factor])))) { # Renommage des colonnes de la sortie
      res <-  unite(res, "col.to.change", c(paste0(jj, "_n"), paste0(jj, "_p")), sep = " ")
      colnames(res)[str_detect(colnames(res), "col.to.change")] <- paste0(jj, " n (%)")
    }
  }
  
  
  # Conversion en matrice
  if (nrow(res) != 1) {
    res <- as.matrix(res)
  } else {
    # Ou si cas particulier d'une seule ligne --> en data.frame pour ne pas perdre la dimension
    res <- as.data.frame(res)
  }
  
  # Inclusion/Non inclusion des colonnes 
  # Suppression des IC
  if (!include.conf) {
    res <- res[, !(substr(colnames(res), nchar(colnames(res))-1, nchar(colnames(res))) == "IC")]
  } else {
    colnames(res)[colnames(res) == "IC"] <- "Total IC 95%"
  }
  # Suppression de la colonne des minmax
  if (!include.minmax) {
    res <- res[, !(substr(colnames(res), nchar(colnames(res))-5, nchar(colnames(res))) == "minmax")]
  } else { # Ou renommage selon les modalites de la variable facteur
    colnames(res)[substr(colnames(res), nchar(colnames(res))-5, nchar(colnames(res))) == "minmax"] <-
      c("Total minmax", paste(levels(data[, factor]), "minmax"))
  }
  
  
  # Gestion de la langue
  if (lang == "fr") { # Si descriptif en francais
    # Traduction et formatage des 'mean sd' et 'med iiq'
    res[, "Var"] <- str_replace_all(res[, "Var"], ", mean [(]sd[)]", ", moyenne (ET)")
    res[, "Var"] <- str_replace_all(res[, "Var"], ", med [(]iiq[)]", ", médiane (IIQ)")
    res[, -1] <- str_replace_all(res[, -1], "[.]", ",")
    colnames(res) <- str_replace(colnames(res), pattern = " IC", replacement = paste0(" IC ", conf.level*100L, "%"))
    colnames(res) <- str_replace(colnames(res), pattern = "_IC", replacement = paste0(" IC ", conf.level*100L, "%"))
  } else { # Si descriptif en anglais
    # Formatage des 'mean sd' et 'med iiq'
    res[, "Var"] <- str_replace_all(res[, "Var"], ", mean [(]sd[)]", ", mean (SD)")
    res[, "Var"] <- str_replace_all(res[, "Var"], ", med [(]iiq[)]", ", median (Q1Q3)")
    colnames(res) <- str_replace(colnames(res), pattern = " IC", replacement = paste0(" CI ", conf.level*100L, "%"))
    colnames(res) <- str_replace(colnames(res), pattern = "_IC", replacement = paste0(" CI ", conf.level*100L, "%"))
  }
  
  
  # Gestion de la fusion/eclatage des colonnes avec l'argument merge.cols
  if (merge.cols) {
    # Gestion de la fusion des colonnes quand les IC ou les minmaxs sont inclus
    if (include.conf | include.minmax) {
      ncol_suppr <- sum(c(include.conf, include.minmax))# By = 1 si une des deux options, 2 sinon
      ind_c <- which(str_detect(colnames(res), " n \\(%"))  # Indices de colonnes de depart pour la fusion
      for (i in 1:length(ind_c)) { # Boucle sur les nombres de modalites + 1 (total)
        res[, ind_c[i]] <- paste(res[, ind_c[i]], res[, ind_c[i] + 1]) # Fusion des 2 colonnes
        if (ncol_suppr == 2) res[, ind_c[i]] <- paste(res[, ind_c[i]], res[, ind_c[i] + 2]) # Et une deuxième fois si besoin
        
        if (include.conf) {
          if (lang == "fr") colnames(res)[ind_c[i]] <- paste(colnames(res)[ind_c[i]], "[IC95%]") # Ajout dans le nom de colonnes
          if (lang == "en") colnames(res)[ind_c[i]] <- paste(colnames(res)[ind_c[i]], "[CI95%]") # Ajout dans le nom de colonnes
        }
        if (include.minmax) colnames(res)[ind_c[i]] <- paste(colnames(res)[ind_c[i]], "{minmax}") # Idem
      }
      res <- res[, -as.vector(sapply(ind_c, `+`, 1:ncol_suppr))] # Suppression des colonnes concatenees
    }
    
    # Gestion de la fusion des colonnes d'IC quand les taux d'incidence sont calcules
    if (include.conf & is.defined(pop.ref)) {
      ncol_suppr <- which(str_detect(colnames(res), "TI IC|TI CI")) # Indices des colonnes d'IC a suppremier
      ind_c <- which(str_detect(colnames(res), " TI") & !str_detect(colnames(res), "TI IC|TI CI")) # Indice des colonnes de depart pour la fusion
      for (i in 1:length(ind_c)) { # Boucle sur les nombres de modalites + 1 (total)
        res[, ind_c[i]] <- paste(res[, ind_c[i]], res[, ind_c[i] + 1]) # Fusion des 2 colonnes
        if (lang == "fr") colnames(res)[ind_c[i]] <- paste(colnames(res)[ind_c[i]], "[IC95% TI]") # Renommage de la colonne
        if (lang == "en") colnames(res)[ind_c[i]] <- paste(colnames(res)[ind_c[i]], "[CI95% TI]") # Renommage de la colonne
        
      }
      res <- res[, -ncol_suppr] # Suppression des colonnes d'IC apres fusion
    }
  } else {
    # Eclatage des colonnes de n et pourcentage
    boucles <- which(str_detect(colnames(res), " n \\(")) # Nombre de modalites (total inclus)
    res2 <- res # Rendu intermediaire pour l'insertion
    for (i in 1:length(boucles)) { # Boucle sur le nombre de modalités
      names_c <- str_replace(colnames(res2)[boucles[i]], " n \\(%\\)", "") # Nom de la colonne a eclater
      inser <- matrix(unlist(str_split(res2[, boucles[i]], " ")), ncol = 2, byrow = TRUE) # Colonne eclatee a inserer
      colnames(inser) <- paste0(names_c, c(" n", " (%)")) # Rennomage de l'insertion
      res <- cbind(res[, 1:(grep(names_c, colnames(res))[1] - 1)], inser, res[, (grep(names_c, colnames(res))[1] + 1):ncol(res)])
      # res[str_detect(colnames(res), names_c)][1] <- inser # Insertion
    }
    res <- as.data.frame(as.matrix(res)) # Retypage de la sortie
    colnames(res) <- substr(colnames(res), str_index(colnames(res), "\\.", "premiere") + 1, nchar(colnames(res)))
  }
  
  
  if (!include.p) { # Suppression de la colonnes des pvaleurs
    res <- res[, colnames(res) != "pval"] # Suppression de la p-valeur
  } else { # Ou ajustement
    # Recuperation des pvaleur
    new.pval <- as.numeric(str_replace_all(res[res[, "pval"] != "", "pval"], ",", "."))
    if (p.adjust) { # Application fonciton p.adjust si p.adjust = TRUE
      new.pval <- p.adjust(new.pval, method = p.adjust.method)
    }
    # Gestion du format de sortie pour l'affichage
    new.pval <- ifelse(new.pval < cut.pvalue,
                       paste0("<", cut.pvalue), # Application du seuil
                       formatC(round(new.pval, p.decimal), digits = p.decimal, format = "f")) # Arrondi
    # Remplacement dans la sortie
    res[res[, "pval"] != "", "pval"] <- new.pval
  }
  
  
  # Suppression de colonnes selon les argument binaire include...
  if (!include.tot) res <- res <- res[, !(substr(colnames(res), 1, 5) == "Total")] # Suppression du descriptif total
  if (!include.n) res <- res[, colnames(res) != "N"] # Suppresion du N total
  if (!include.test.name) res <- res[, colnames(res) != "test"] # Suppression de la colonne de test
  if (is.null(pop.ref)) res <- res[, !(substr(colnames(res), nchar(colnames(res))-1, nchar(colnames(res))) == "TI")] # Suppression de la p-valeur
  if (is.null(pop.ref)) res <- res[, !(str_detect(colnames(res), "TI IC 95"))]
  if (is.null(pop.ref)) res <- res[, !(str_detect(colnames(res), "TI CI 95"))]
  colnames(res) <- str_replace_all(colnames(res), "95% 95%", "95%")
  
  if (!is.null(big.mark)) {
    res[, 2:ncol(res)] <- apply(res[, 2:ncol(res)], 2, function(x) {
      x <- str_replace_all(x, ",", ".")
      x <- ifelse(nchar(sub("\\ .*", "", x)) > 0,
                  ifelse(str_detect(x, " "),
                         paste(prettyNum(as.numeric(sub("\\ .*", "", x)), big.mark = big.mark), sub("^[^_]* ", "", x)),
                         prettyNum(as.numeric(sub("\\ .*", "", x)), big.mark = big.mark)),
                  x)
      x <- str_replace_all(x, "\\.", ",")
    })
  }
  
  return(as.data.frame(res))
}

str_index <- function(string,
                      pattern,
                      mode = c("first", "all", "last")) {
  
  if(is.null(string)) stop("string missing")
  if(is.null(pattern)) stop("pattern missing")
  
  str_index_intermediaire <- function(string, pattern) {
    unname(lapply(sapply(string, function(x) { gregexpr(pattern, x)}), as.vector))
  }
  
  if (length(mode) > 1) mode <- "all"
  
  ifelse(mode == "all",
         # Si tous les index sont a retouner
         return(str_index_intermediaire(string, pattern)),
         # Selection si seulement le premier ou le dernier
         return(unlist(lapply(str_index_intermediaire(string, pattern), function(x) {
           x[ifelse(mode == "first", 1, length(x))]})))
  )
}

'%ni%' <- function(x, y) {
  !('%in%'(x,y))
}

relevel_factor <- function(fac, new.levels = NULL, ref = NULL) {
  
  
  
  if(is.null(fac)){
    stop("fac missing")
  }
  
  
  # Cas nul
  if (is.null(new.levels)) {
    res <- as.factor(fac)
  }
  
  #Cas liste
  if (is.list(new.levels)) {
    
    # Relveling simple
    res <- factor(fac,
                  levels = names(new.levels),
                  labels = new.levels)
  }
  
  
  #Cas DF
  if (is.data.frame(new.levels) | is.matrix(new.levels)) {
    # Relveling simple
    res <- factor(fac,
                  levels = new.levels[, 1],
                  labels = new.levels[, 2])
  }
  
  # Changement de facteur de reference
  if (is.defined(ref)) {
    res <- relevel(res, ref)
  } else {
    res <- relevel(res, names(which.max(table(as.character(res)))))
  }
  
  return(res)
}


anova_test <- function (data, formula, dv, wid, between, within, covariate, 
                        type = NULL, effect.size = "ges", error = NULL, white.adjust = FALSE, 
                        observed = NULL, detailed = FALSE) 
{
  .args <- rlang::enquos(dv = dv, wid = wid, between = between, 
                         within = within, covariate = covariate) %>% select_quo_variables(data) %>% 
    add_item(type = type, white.adjust = white.adjust, method = "anova_test")
  if (!missing(formula)) 
    .args$formula <- formula
  .anova_test <- function(data, .args, effect.size = "ges", 
                          error = NULL, observed = NULL, detailed = FALSE) {
    .args <- .args %>% add_item(data = data) %>% check_anova_arguments()
    if (.args$type != 1) {
      if (is.null(error)) 
        res.anova <- car_anova(.args)
      else res.anova <- car_anova(.args, error = error)
    }
    else if (.args$type == 1) 
      res.anova <- stats_aov(.args)
    else stop("Something is wrong...")
    results <- res.anova %>% anova_summary(effect.size = effect.size, 
                                           detailed = detailed, observed = observed)
    results
  }
  .append_anova_class <- function(x) {
    class(x) <- c("anova_test", class(x), "rstatix_test")
    x
  }
  if (is_grouped_df(data)) {
    results <- data %>% doo(~.anova_test(data = ., .args = .args, 
                                         effect.size = effect.size, error = error, observed = observed, 
                                         detailed = detailed), result = "anova")
    if ("anova" %in% colnames(results)) {
      results <- results %>% mutate(anova = map(.data$anova, 
                                                .append_anova_class))
    }
    results <- results %>% set_attrs(args = list(data = data))
    class(results) <- c("grouped_anova_test", class(results), 
                        "rstatix_test")
  }
  else {
    results <- .anova_test(data, .args = .args, effect.size = effect.size, 
                           error = error, observed = observed, detailed = detailed) %>% 
      .append_anova_class()
  }
  results
}

is.defined <- function(x) {
  return(!is.null(x))
}


standardizedDifference <- function(
    mean.treatment = NULL, mean.control = NULL,
    sd.treatment = NULL, sd.control = NULL,
    prop.treatment = NULL, prop.control = NULL) {
  
  if ((length(c(mean.treatment, mean.control, sd.treatment, sd.control)) > 0 & 
       length(c(prop.treatment, prop.control)) == 2) |
      (length(c(mean.treatment, mean.control, sd.treatment, sd.control)) == 4 & 
       length(c(prop.treatment, prop.control)) > 0)) {
    stop("You must provide EITHER the means and standard deviation for a quantitative variable,
         or the proportion for a dichotomous variable")
  }
  if (length(c(mean.treatment, mean.control, sd.treatment, sd.control)) < 4 & 
      length(c(prop.treatment, prop.control)) < 2) {
    stop("You must provide the ALL OF the means and standard deviation for a quantitative variable,
         or the proportion for a dichotomous variable")
  }
  
  if (all(!is.null(c(prop.treatment, prop.control)))) {
    standardDifference <- 100 * (prop.treatment - prop.control) / sqrt((prop.treatment * (1 -prop.treatment) + prop.control * (1 -prop.control))/2)
  }
  if (all(!is.null(c(mean.treatment, mean.control, sd.treatment, sd.control)))) {
    standardDifference <- 100 * (mean.treatment - mean.control) / sqrt((sd.treatment^2 + sd.control^2)/2)
  }
  
  diag <- ifelse(abs(standardDifference) < 10, 
                 "NOT indicative of meaningful imbalance in a covariates between treated and control subjects",
                 "Indicative of meaningful imbalance in a covariates between treated and control subjects")
  
  return(list(d = standardDifference,
              diagnostic = diag))
}



createSankeyData <- function(data,
                             states,
                             timesColumns) {
  data <- as.data.frame(data)
  
  n_states <- length(states)
  n_times <- length(timesColumns)
  nodesCols <- c("#AAC0AF", "#B28B84", "#1C4073", "#0f766e", "#653239", "#472C1B", "#5C2751")[1:n_states]
  linksCols <- c("#D0DCD3", "#D0B8B4", "#285CA4", "#17B5A7", "#964A54", "#76492D", "#8F3D7E")[1:n_states]
  
  
  vals <- c()
  for (i in 2:n_times) {
    for (j in 1:n_states) {
      vals <- c(vals, table(data[, timesColumns[i]][data[, timesColumns[i-1]] == states[j]]))
    }
  }
  
  dataSankeyTem <- list(
    Nodes = data.frame(
      label = rep(states, n_times),
      color = rep(nodesCols, n_times)
    ),
    Links = data.frame(
      source = c(rep(1:(n_states*(n_times-1)), each = n_states)) - 1,
      target = as.vector(sapply(split((n_states+1):(n_states*n_times), rep(1:(n_times-1), each = n_states)), function(x) {rep(x, n_states)})) - 1,
      value = vals,
      color = rep(rep(linksCols, each = n_states), n_times-1)
    ))
}


tidyDesc_binary <- function(desc.object) {
  i <- 2
  n <- 0
  buffer_start <- sum(colnames(desc.object) %in% c("Var", "N")) + 1
  buffer_end <- sum(colnames(desc.object) %in% c("pval", "test")) # number of columns that should be kept at the end of the desc
  # Suppressing Oui/Non lines to shorten the table
  while (i < (nrow(desc.object))) {
    # Oui/non double lines detection
    # making sure ther's nothing after
    if (((desc.object[i, 1] == "  Non" & desc.object[i+1, 1] == "  Oui" ) |
         (desc.object[i, 1] == "  Oui" & desc.object[i+1, 1] == "  Non") | 
         (desc.object[i, 1] == "  Yes" & desc.object[i+1, 1] == "  No") |
         (desc.object[i, 1] == "  No" & desc.object[i+1, 1] == "  Yes")) & 
        ifelse(i+1 == nrow(desc.object), TRUE, substr(desc.object[i+2, 1], 1, 2) != "  ")) {
      # replacing empty line with the data
      
      # if yes is the first line, keep it, otherwise, skip to the next one
      rowid <- ifelse(desc.object[i, 1] == "  Oui" | desc.object[i, 1] == "  Yes", i, i+1)
      
      if (buffer_end == 0) {
        # if no buffer_end, keep the whole line
        temp <- desc.object[rowid, buffer_start:ncol(desc.object)]
      } else {
        # otherwise, fetch the buffer_end columns in the title line
        temp <- c(desc.object[rowid, buffer_start:(ncol(desc.object) - buffer_end)],
                  desc.object[i - 1, (ncol(desc.object) - buffer_end + 1):ncol(desc.object)])
      }
      
      desc.object[i-1, buffer_start:ncol(desc.object)] <- temp
      # suppressing the oui/non lines
      desc.object <- desc.object[-c(i, i+1),]
      n <- n + 1
    }
    i <- i + 1
  }
  message(paste(n, "Yes/No variables have been detected and", 2*n, "lines have been suppressed"))
  return(desc.object)
}


tidyDesc_censorLowFreq <- function(desc.object, threshold = 10){
  X <- which(substr(desc.object[, 1], 1, 2) == "  ")
  # X <- c(3, 4, 5, 6, 8, 9, 11, 12, 14, 15, 18, 19, 20, 21)
  dXb <- c(diff(X), 2) > 1
  listQ <- split(X, f = rep(1:sum(dXb), c(which(dXb)[1], diff(which(dXb)))))
  
  res <- desc.object
  sapply(listQ, function(x) {
    if (any(as.numeric(sub("\\ .*", "", unlist(as.vector(res[x, str_detect(colnames(res), " n")])))) < threshold)) {
      res[x, str_detect(colnames(res), " n")] <<- paste("<", threshold)
    }
  })
  res
}

getModelCoefs <- function(
    model, # Une sortie de modèle avec un attribut 'coefficients'
    coefs_digits = 2,
    p_digits = 3,
    p_threshold = 0.001,
    merge_CI = TRUE,
    merge_p = TRUE
) {
  
  if(!merge_CI & merge_p) warning("Confidence intervals will only be unmerged if 'merge_p = FALSE'")
  
  coefficients <- summary(model)$coefficients[, 1]
  se <- summary(model)$coefficients[, 2]
  
  if (merge_p) {
    res <- data.frame(
      Variable = rownames(summary(model)$coefficients)[-1],
      Coefs = paste0(formatC(exp(coefficients)[-1], digits = coefs_digits, format = "f"), " [",     # OR
                     formatC(exp(coefficients - 1.96 * se)[-1], digits = coefs_digits, format = "f"), "-",      # IC inf
                     formatC(exp(coefficients + 1.96 * se)[-1], digits = coefs_digits, format = "f"), "], p ", # IC sup
                     ifelse(summary(model)$coefficients[-1, 4] < p_threshold,
                            paste("<", p_threshold),
                            paste("=", formatC(summary(model)$coefficients[-1, 4], digits = p_digits, format = "f"))))      # p val
    )
  } else {
    if (merge_CI) {
      res <- data.frame(
        Variable = rownames(summary(model)$coefficients)[-1],
        Coefs = paste0(formatC(exp(coefficients)[-1], digits = coefs_digits, format = "f"), " [",     # OR
                       formatC(exp(coefficients - 1.96 * se)[-1], digits = coefs_digits, format = "f"), "-",      # IC inf
                       formatC(exp(coefficients + 1.96 * se)[-1], digits = coefs_digits, format = "f"), "]"),# IC sup
        p = ifelse(summary(model)$coefficients[-1, 4] < p_threshold,
                   paste0("<", p_threshold),
                   formatC(summary(model)$coefficients[-1, 4], digits = p_digits, format = "f")))    # p val
    } else {
      res <- data.frame(
        Variable = rownames(summary(model)$coefficients)[-1],
        Coefs = formatC(exp(coefficients)[-1], digits = coefs_digits, format = "f"),    # OR
        CIlower = formatC(exp(coefficients - 1.96 * se)[-1], digits = coefs_digits, format = "f"),
        CIupper = formatC(exp(coefficients + 1.96 * se)[-1], digits = coefs_digits, format = "f"),
        p = ifelse(summary(model)$coefficients[-1, 4] < p_threshold,
                   paste0("<", p_threshold),
                   formatC(summary(model)$coefficients[-1, 4], digits = p_digits, format = "f"))    # p val
      )
    }
  }
  res
}




#### Fonctiosn sur le portail ####

plot_to_csv <- function(
    plot_fun, # un ggplot
    height_fun, # une taille en cm par défaut sinon dans l'unité choisi
    width_fun, # une largeur en cm par défaut sinon dans l'unité choisi
    plot_name, # un nom pour le plot entre " par exemple "blblblbl"
    image_path = getwd(), # un chemin ou sauvegarder l'image par défaut le path courant
    extension = "png", # une extension d'image
    unit_fun = "cm", # unité pour height_fun et width_fun
    dpi_fun = 300, # dpi du plot 
    keep_img = FALSE, # la version image du plot doit elle etre conservé
    Verbose = TRUE # empecher que la fonction parle
) {
  path_plot_en_cours <- paste0(image_path, "/", plot_name, ".", extension)
  
  ggplot2::ggsave(
    filename = path_plot_en_cours,
    plot = plot_fun,
    units = unit_fun,
    dpi = dpi_fun,
    width = width_fun,
    height = height_fun
  )
  if (file.exists(path_plot_en_cours)) {
    file.copy(
      from = path_plot_en_cours,
      to = paste0("~/Citrix_documents/EXPORT/", plot_name, ".csv"),
      overwrite = TRUE
    )
    if(Verbose){
      print(paste0('Plot save at : ', "~/Citrix_documents/EXPORT/", plot_name, ".csv"))
    }
    if (!keep_img) {
      unlink(
        path_plot_en_cours
      )
      if(Verbose){
        print(paste0('Remove file : ',path_plot_en_cours))
      }
    } 
  }
}

export.csv.SNDS <- function(data, name) {
  file_path <- paste0("Citrix_documents/EXPORT/export_", name, ".csv")
  write.csv2(data, file = file_path, row.names = FALSE)
  message("Export OK")
}


#### Package hdps #### 

# Sourcing github package hdps (https://github.com/lendle/hdps/tree/master)
# Pour utilisation sur le SNDS

# Basé sur Schneeweiss 2009 (https://pubmed.ncbi.nlm.nih.gov/19487948/)


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
