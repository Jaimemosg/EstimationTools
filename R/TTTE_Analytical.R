#' @title Empirical Total Time on Test (TTT), analytic version.
#' @family EmpiricalTTT
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' This function allows to compute the TTT curve from a formula
#' containing a factor type variable (classification variable).
#'
#' @param formula an object of class \code{\link{formula}} with the response on
#'                the left of an operator \code{~}. The right side can be a factor
#'                variable as term or an \code{1} if a classification by factor
#'                levels is not desired.
#' @param response an optional numeric vector with data of the response variable.
#'                Using this argument is equivalent to define a formula with the
#'                right side such as \code{~ 1}. See the fourth example below.
#' @param scaled logical. If \code{TRUE} (default value), scaled TTT is computed.
#' @param data an optional data frame containing the variables (response and the
#'             factor, if it is desired). If data is not specified, the variables
#'             are taken from the environment from which \code{TTT_analytical}
#'             is called.
#' @param method a character specifying the method of computation. There are two
#'               options available: \code{'Barlow'} and \code{'censored'}. Further
#'               information can be found in the \strong{Details} section.
#' @param partition_method a list specifying cluster formation when the covariate in
#'                         \code{formula} is numeric. Equispaced distribution based
#'                         on quantiles is the only one currently available.
#' @param ... further arguments passing to \code{\link[survival]{survfit}}.
#'
#' @details When \code{method} argument is set as \code{'Barlow'}, this function
#' uses the original expression of empirical TTT presented by
#' \insertCite{Barlow1979;textual}{EstimationTools}  and used by
#' \insertCite{Aarset1987;textual}{EstimationTools}:
#'
#' \deqn{\phi_n\left( \frac{r}{n}\right) = \frac{\left( \sum_{i=1}^{r} T_{(i)} \right) +
#' (n-r)T_{(r)}}{\sum_{i=1}^{n} T_i}}
#'
#' where \eqn{T_{(r)}}{T_(r)} is the \eqn{r^{th}}{rth} order statistic, with
#' \eqn{r=1,2,\dots, n}, and \eqn{n} is the sample size. On the other hand, the option
#' \cite{'censored'} is an implementation based on integrals presented in
#' \insertCite{Westberg1994;textual}{EstimationTools}, and using
#' \code{\link[survival]{survfit}} to compute the Kaplan-Meier estimator:
#'
#' \deqn{\phi_n\left( \frac{r}{n}\right) = \sum_{j=1}^{r} \left[ \prod_{i=1}^{j}
#' \left( 1 - \frac{d_i}{n_i}\right) \right] \left(T_{(j)} - T_{(j-1)} \right)}
#'
#' @return A list with class object \code{Empirical.TTT} containing a list with the
#'        following information:
#' \item{i/n`}{A matrix containing the empirical quantiles. This matrix has the
#'             number of columns equals to the number of levels of the factor
#'             considered (number of strata).}
#' \item{phi_n}{A matrix containing the values of empirical TTT. his matrix has the
#'             number of columns equals to the number of levels of the factor
#'             considered (number of strata).}
#' \item{strata}{A numeric named vector storing the number of observations per strata,
#'               and the name of each strata (names of the levels of the factor).}
#'
#' @importFrom stats model.frame model.extract terms end quantile
#' @importFrom survival survfit.formula is.Surv strata
#' @importFrom BBmisc is.error
#' @export
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' # First example: Scaled empirical TTT from 'mgus1' data from 'survival' package.
#'
#' TTT_1 <- TTTE_Analytical(Surv(stop, event == 'pcm') ~1, method = 'cens',
#'                          data = mgus1, subset=(start == 0))
#' head(TTT_1$`i/n`)
#' head(TTT_1$phi_n)
#' print(TTT_1$strata)
#'
#'
#' #--------------------------------------------------------------------------------
#' # Second example: Scaled empirical TTT using a factor variable with 'aml' data
#' # from 'survival' package.
#'
#' TTT_2 <- TTTE_Analytical(Surv(time, status) ~ x, method = "cens", data = aml)
#' head(TTT_2$`i/n`)
#' head(TTT_2$phi_n)
#' print(TTT_2$strata)
#'
#' #--------------------------------------------------------------------------------
#' # Third example: Non-scaled empirical TTT without a factor (arbitrarily simulated
#' # data).
#'
#' y <- rweibull(n=20, shape=1, scale=pi)
#' TTT_3 <- TTTE_Analytical(y ~ 1, scaled = FALSE)
#' head(TTT_3$`i/n`)
#' head(TTT_3$phi_n)
#' print(TTT_3$strata)
#'
#'
#' #--------------------------------------------------------------------------------
#' # Fourth example: non-scaled empirical TTT without a factor (arbitrarily simulated
#' # data) using the 'response' argument (this is equivalent to Third example).
#'
#' y <- rweibull(n=20, shape=1, scale=pi)
#' TTT_4 <- TTTE_Analytical(response = y, scaled = FALSE)
#' head(TTT_3$`i/n`)
#' head(TTT_3$phi_n)
#' print(TTT_3$strata)
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @references
#' \insertRef{Barlow1979}{EstimationTools}
#'
#' \insertRef{Aarset1987}{EstimationTools}
#'
#' \insertRef{Klefsjo1991}{EstimationTools}
#'
#' \insertRef{Westberg1994}{EstimationTools}
#'
#' @importFrom Rdpack reprompt
#'
#' @seealso \code{\link{plot.EmpiricalTTT}}
#'
#==============================================================================
# Empirical TTT computation ---------------------------------------------------
#==============================================================================
TTTE_Analytical <- function(formula, response = NULL, scaled = TRUE, data,
                            method = c('Barlow', 'censored'),
                            partition_method = list(method='K-fold', folds=3),
                            ...){
  method <- match.arg(method, c('Barlow', 'censored'))
  mycall <- match.call()
  formula_definition <- try(!is.null(formula), silent = TRUE)

  if ( is.null(response) & !BBmisc::is.error(formula_definition) ){

    # data frame building inspired by 'suvrfit' function from 'survival' package
    id_arg <- match(c("formula", "data"), names(mycall),
                    nomatch=0)
    if (id_arg[1L] == 0) stop("a formula argument is required")
    temp <- mycall[c(1L, id_arg)]
    temp[[1L]] <- quote(stats::model.frame)
    modfrm <- eval.parent(temp)
    Terms <- stats::terms(formula)

    ord <- attr(Terms, "order")
    if (length(ord) & any(ord != 1))
      stop("Interaction terms are not valid for this function")

    x_id <- attr(stats::terms(modfrm), 'term.labels')
    if ( length(x_id) > 1 )
      stop('Empirical TTT statistic only can be classified by one covariate at
            time')

    y <- stats::model.extract(modfrm, 'response')

    if (length(x_id) == 0){
      x <- factor(rep(1, nrow(modfrm)))
    } else {
      if ( is.factor(class(modfrm[x_id][[1]])) )
        x <- survival::strata(modfrm[x_id])
      if ( is.character(class(modfrm[x_id][[1]])) ){
        modfrm[x_id][[1]] <- as.factor(modfrm[x_id][[1]])
        x <- survival::strata(modfrm[x_id])
      }
      if ( is.numeric(class(modfrm[x_id][[1]])) ){
        x <- num2fac(partition_method = partition_method, y = y)
      }
    }

  } else {

    if ( !is.numeric(response) )
      stop("'response' argument must be numeric")
    x <- factor(rep(1, length(response)))
    y <- response
    formula <- y ~ x
    modfrm <- stats::model.frame(formula = formula,
                                 data = data.frame(y, x))

  }

  if ( is.Surv(y) & method == "Barlow")
    stop("Non-censored data must not be handled with a 'Surv' object in the
         'formula' argument, and the method must be set as 'censored'")

  Alldots <- substitute(...())
  inputs <- switch(method,
                   censored = Cens_action(y, formula, model_frame = modfrm,
                                          data = data, Alldots),
                   Barlow = Baction(y, x))

  TTT <- TTT_formula_selector(inputs, scaled, method)
  class(TTT) <- "EmpiricalTTT"
  return(TTT)
}
#==============================================================================
# TTT preparation for numerical covariate -------------------------------------
#==============================================================================
num2fac <- function(partition_method, y){
  if (partition_method[[1]] %in% c('equispaced','density-based')){
    prob <- 1/partition_method[[2]] * (1:(partition_method[[2]]))
    prob <- c(0, prob)
    qi <- as.numeric(quantile(y, probs=prob))
    cuts <- sapply(1:length(qi),
                   function(x) which.min(abs(y - qi[x])))
    indexes <- lapply(1:(length(cuts) - 1),
                      function(i) which(y>=y[cuts[i]] & y<y[cuts[i+1]]))
    ranges <- sapply(1:(length(cuts) - 2), simplify = 'list',
                     function(i) paste0('[', round(y[cuts[i]],2), ' - ',
                                        round(y[cuts[i+1]],2), ')'))
    ranges[[length(cuts)-1]] <- paste0('[', round(y[cuts[length(cuts)-1]],2), ' - ',
                                       round(y[cuts[length(cuts)]],2), ']')
    x <- rep(NA, length(y))
    for (i in 1:(length(cuts) - 1) ) x[indexes[[i]]] <- ranges[i]
    x[cuts[length(cuts)]] <- ranges[length(ranges)]
    x <- as.factor(x)
    return(x)
  } else {
    stop("Choose one partition method from the following partition methods:
         'equispaced' or 'density-based'")
  }
}
#==============================================================================
# Data preparation for TTT computation ----------------------------------------
#==============================================================================
Cens_action <- function(y, fo, model_frame, data, Alldots){
  # if ( !is.Surv(y) ){
  #   fo <- formula2Surv(model_frame)
  #   if ( missing(data) ) data <- model_frame
  # } else {
  #   if ( missing(data) ){
  #     vars <- names(model_frame)
  #     ySurv <- vars[1L]
  #     yname <- gsub("Surv\\((.*?),.*", "\\1", ySurv)
  #     statusname <- gsub(paste0("Surv\\(", yname, ",(.*?)\\)"), "\\1", ySurv)
  #     factorname <- vars[length(vars)]
  #     data <- data.frame(y[,1], y[,2], model_frame[,2])
  #     colnames(data) <- c(yname, statusname, factorname)
  #   }
  # }
  cens_outs <- fo_and_data(y, fo, model_frame, data, fo2Surv = TRUE)
  args_matches <- match(names(formals(survfit.formula)), names(Alldots),
                        nomatch = 0)
  survfit_extras <- Alldots[args_matches]
  survfit_dots <- Alldots[-args_matches]
  survfit_dots <- if ( length(survfit_dots) == 0 ){ NULL }
  inputs <- do.call("survfit.formula", args = c(list(formula = cens_outs$fo,
                                                     data = cens_outs$data),
                                                survfit_extras, survfit_dots))
  return(inputs)
}
Baction <- function(y, x){
  if ( is.Surv(y) ){
    inputs <- data.frame(y[,1], x)
  } else { inputs <- data.frame(y, x) }
  return(inputs)
}
#==============================================================================
# Routine for TTT computation of censored data  -------------------------------
#==============================================================================
TTT_censored <- function(inputs, scaled){
  y <- inputs$time
  Surv <- inputs$surv
  Strata <- inputs$strata
  nlevs <- if ( is.null(Strata) ){ length(y) } else { inputs$strata }
  x_int <- c(0, as.integer(cumsum(nlevs)))
  ngroups <- length(nlevs)

  if ( is.null(names(nlevs)) ){
    names(nlevs) <- "SingleGroup"
  }

  levs <- names(nlevs)
  levs_int <- as.integer(factor(levs))
  phi_n <- r_full <- matrix(nrow = (max(nlevs) + 1), ncol = ngroups,
                            byrow = FALSE)

  for (i in 1:ngroups){
    fac <- (x_int[i] + 1):x_int[i+1]
    y_data <- y[fac]

    n <- as.numeric(nlevs[i])
    r <- c(0, (1:n)/n)
    y_order <- order(y_data)
    sorty <- y_data[y_order]
    sorty <- c(0, sorty)

    Trn <- cumsum(diff(sorty)*Surv[fac])

    temp_phi <- if (scaled){ Trn/Trn[n] } else { Trn }
    temp_phi <- c(0, temp_phi)
    length(temp_phi) <- length(r) <- max(nlevs) + 1
    phi_n[,i] <- temp_phi
    r_full[,i] <- r
  }

  return(list(`i/n` = r_full, phi_n = phi_n, strata = nlevs))
}
#==============================================================================
# Routine for TTT computation of Non-censored data  ---------------------------
#==============================================================================
TTT_Barlow <- function(inputs, scaled){
  y <- inputs[,1]
  x <- inputs[,2]
  levs <- levels(x)
  levs_int <- as.integer(factor(levs))
  x_int <- as.integer(x)
  ngroups <- length(levs)

  nlevs <- sapply(levs_int, function(x) length(which(x_int == x)))
  phi_n <- r_full <- matrix(nrow = (max(nlevs) + 1), ncol = ngroups, byrow = FALSE)

  for (i in 1:ngroups){
    fac <- which(x_int == levs_int[i])
    y_data <- y[fac]

    n <- nlevs[i]
    r <- c(0, (1:n)/n)
    y_order <- order(y_data)
    sorty <- y_data[y_order]
    sorty <- c(0, sorty)

    Trn <- cumsum((n - (1:n) + 1)*diff(sorty))

    temp_phi <- if (scaled){ Trn/Trn[n] } else { Trn }
    temp_phi <- c(0, temp_phi)
    length(temp_phi) <- length(r) <- max(nlevs) + 1
    phi_n[,i] <- temp_phi
    r_full[,i] <- r
  }

  if ( is.null(names(nlevs)) ){
    names(nlevs) <- "SingleGroup"
  } else { names(nlevs) <- levs }

  return(list(`i/n` = r_full, phi_n = phi_n, strata = nlevs))
}
#==============================================================================
# Selector (Non-censored vs. censored) ----------------------------------------
#==============================================================================
TTT_formula_selector <- function(inputs, scaled, method){
  method <- paste0('TTT_', method)
  TTT <- do.call(method, list(inputs, scaled = scaled))
  return(TTT)
}

