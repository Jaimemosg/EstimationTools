#' Hazard Shape estimation from TTT plot
#'
#' @author Jaime Mosquera Gutiérrez \email{jmosquerag@unal.edu.co}
#' @family HazardShape
#'
#' @description
#' This function can be used so as to estimate hazard shape corresponding
#' to a given data set.
#'
#' @param formula an object of class \code{\link{formula}} with the response on
#'                the left of an operator \code{~}. The right side must be
#'                \code{1}.
#' @param data an optional data frame containing the response variables. If
#'             data is not specified, the variables are taken from the
#'             environment from which \code{\link{TTT_hazard_shape}} is called.
#' @param local_reg a list of control parameters for LOESS. See
#'                  \code{\link{loess.options}}.
#' @param interpolation a list of control parameters for interpolation function. See
#'                  \code{\link{interp.options}}.
#' @param ... further arguments passed to
#'            \code{\link{TTTE_Analytical}}.
#'
#' @details
#' This function performs a non-parametric estimation of the empirical total
#' time on test (TTT) plot. Then, this estimated curve can be used so as to
#' get suggestions about initial values and the search region for parameters
#' based on hazard shape associated to the  shape of empirical TTT plot.
#'
#' @importFrom stats terms predict na.omit formula
#' @importFrom survival is.Surv
#' @importFrom BBmisc is.error
#' @examples
#' #--------------------------------------------------------------------------------
#' # Example 1: Increasing hazard and its corresponding TTT plot with simulated data
#' hweibull <- function(x, shape, scale){
#'   dweibull(x, shape, scale)/pweibull(x, shape, scale, lower.tail = FALSE)
#'   }
#' curve(hweibull(x, shape = 2.5, scale = pi), from = 0, to = 42,
#'                col = "red", ylab = "Hazard function", las = 1, lwd = 2)
#'
#' y <- rweibull(n = 50, shape = 2.5, scale = pi)
#' my_initial_guess <- TTT_hazard_shape(formula = y ~ 1)
#' Hazard_Shape(my_initial_guess)
#' plot(my_initial_guess, par_plot=list(mar=c(3.7,3.7,1,1.5),
#'                                      mgp=c(2.5,1,0)))
#'
#'
#' #--------------------------------------------------------------------------------
#' @export
TTT_hazard_shape <- function(formula, data=NULL,
                             local_reg = loess.options(),
                             interpolation = interp.options(), ...){
  if ( length(attr(terms(formula), "term.labels")) > 0 )
    stop('initValuesOW_TTT function only uses response variable.')
  mycall <- match.call()
  id_arg <- match(c('formula', 'data'), names(mycall),
                  nomatch=0)
  temp <- mycall[c(1L, id_arg)]
  temp[[1L]] <- quote(stats::model.frame)
  modfrm <- eval.parent(temp)
  y <- stats::model.extract(modfrm, 'response')

  method <- if ( is.Surv(y) ){'censored'} else {'Barlow'}

  dots <- substitute(...())
  args_matches <- match(names(formals(TTTE_Analytical)),
                        names(dots), nomatch = 0)
  TTTE_params <- dots[args_matches]
  TTTE_dots <- dots[-args_matches]
  TTTE_dots <- if ( length(TTTE_dots) == 0 ){ NULL }

  g1 <- do.call("TTTE_Analytical",
                args = c(list(formula = formula, response = NULL,
                              method = method,
                              scale = TRUE), TTTE_params, TTTE_dots))

  the_warning <- NULL

  g2 <- cbind(g1$`i/n`, g1$phi_n)
  g3 <- try(do.call("loess",
                    c(list(formula=g2[,2] ~ g2[,1]),
                      local_reg)), silent=TRUE)
  g4 <- do.call(interpolation$interp.fun,
                list(x = g2[,1], y=predict(g3),
                     interpolation$passing_args))

  if (is.error(g3) | is.nan(g3$s)){
    hazard_type <- NA
    warning(paste0("Problem with LOESS estimation. The sample",
                   "size may be too small"))
  } else {
    lout <- (length(y) - 1)*5
    dTTT_dp <- g4(seq(0,1,length.out = interpolation$length.out), deriv=1)
    d2TTT_dp2 <- g4(seq(0,1,length.out = interpolation$length.out), deriv=2)

    target <- diff(sign(d2TTT_dp2))
    inflex <- which( target != 0 )
    diff_val <- try(target[inflex], silent = TRUE)

    if ( length(inflex) < 2 ){
      if ( length(inflex) > 0 ){
        if (diff_val == 2){
          # all(nu > 1)"
          hazard_type <- "Unimodal"
        }
        if (diff_val == -2){
          hazard_type <- "Bathtub"
        }
      } else {
        sign_search <- any(sign(d2TTT_dp2) < 0) # if (is.na(sum(sign_search))){
        if (sign_search){# negative second derivative
          hazard_type <- "Increasing"
        } else { # positive second derivative
          hazard_type <- "Decreasing"
        }
      }
    } else {
      the_warning <- paste0("Non-parametric estimate for Empirical TTT",
                            " is irregular.\nPlease, ",
                            "use the 'plot()' method to see the TTT ",
                            "shape and set the search region manually in ",
                            "'gamlss()' if there is no conincidence between ",
                            "'summary()' and 'plot()'. Visit ",
                            "'OW distribution' vignette to get further ",
                            "information.")
      warning(the_warning)
      criterion <- sapply(g2[,1], criteria, x_val=0, y_val=1, g3=g3)
      control1 <- all(criterion)
      control2 <- all(criterion[2:(criterion[length(g2[,1])] - 1)])
      if ( control1 ){
        # Decreasing hazard
        hazard_type <- "Decreasing"
      } else if ( !control2 ){
        # Increasing hazard
        hazard_type <- "Increasing"
      } else {
        sigma <- NA;  nu <- NA;
        sigma.valid <- NA; nu.valid <- NA
        hazard_type <- NA
      }
    }
  }

  output <- list(formula=formula, response=y,
                 local_reg=g3, interpolation=g4, TTTplot=g2,
                 hazard_type=hazard_type, warning=the_warning)
  class(output) <- "HazardShape"
  return(output)
}
#==============================================================================
# Convexity criterion ---------------------------------------------------------
#
# f(lambda*x + (1 - lambda)*y) <= lambda*f(x) + (1 - lambda)*f(y)
#==============================================================================
#' @keywords internal
#'
criteria <- function(lambda, x_val, y_val, g3){
  f_xy <- predict(g3, newdata = c(x_val, y_val))
  right <- matrix(c(lambda, 1-lambda), ncol=2) %*% matrix(f_xy, nrow=2)
  right <- as.numeric(right)
  argument <- lambda*x_val + (1 - lambda)*y_val
  left <- predict(g3, newdata = argument)
  return(left <= right)
}
