#==============================================================================
# Default link functions ------------------------------------------------------
#==============================================================================
#' @title Logit link function (for estimation with \code{maxlogL})
#' @family link functions
#'
#' @description
#' \code{log_link} object provides a way to implement logit link function that
#' \code{\link{maxlogL}} needs to perform estimation. See documentation for
#' \code{\link{maxlogL}} for further information on parameter estimation and implementation
#' of link objects.
#'
#' @return A list with logit link function, its inverse and its name.
#' @export
#'
#' @examples
#' # Estimation of proportion in binomial distribution with 'logit' function
#' # 10 trials, probability of success equals to 30%)
#' N <- rbinom(n = 100, size = 10, prob = 0.3)
#' phat <- maxlogL(x = N, dist = 'dbinom', fixed = list(size=10),
#'                 link = list(over = "prob", fun = "logit_link"))
#' summary(phat)
#'
#' # Link function name
#' fun <- logit_link()$name
#' print(fun)
#'
#' # Link function
#' g <- logit_link()$g
#' curve(g(x), from = 0, to = 1)
#'
#' # Inverse link function
#' ginv <- logit_link()$g_inv
#' curve(ginv(x), from = -10, to = 10)
#'
#' @details \code{log_link} is part of a family of generic functions with no input arguments that
#' defines and returns a list with details of the link function:
#' \enumerate{
#'    \item \code{name}: a character string with the name of the link function.
#'    \item \code{g}: implementation of the link function as a generic function in \code{R}.
#'    \item \code{g_inv}: implementation of the inverse link function as a generic function
#'          in \code{R}.
#' }
#'
#' There is a way to add new mapping functions. The user must specify the details aforesaid.
#'
#' @seealso \code{\link{maxlogL}}
#'
logit_link <- function(){
  name <- "Logit"
  g <- function(x) log(x/(1-x))
  g_inv <- function(x) exp(x)/(exp(x)+1)
  out <- list(name = name, g = g, g_inv = g_inv)
  return(out)
}
#=============================================================================#
#' @title Logarithmic link function (for estimation with \code{maxlogL})
#' @family link functions
#'
#' @description
#' \code{log_link} object provides a way to implement logarithmic link function that
#' \code{\link{maxlogL}} needs to perform estimation. See documentation for
#' \code{\link{maxlogL}} for further information on parameter estimation and implementation
#' of link objects.
#'
#' @return A list with logit link function, its inverse and its name.
#' @export
#'
#' @examples
#'# One parameters of normal distribution mapped with logarithmic function
#' x <- rnorm(n = 10000, mean = 50, sd = 4)
#' theta_2 <- maxlogL( x = x, link = list(over = "sd",
#'                                        fun = "log_link") )
#' summary(theta_2)
#'
#' # Link function name
#' fun <- log_link()$name
#' print(fun)
#'
#' # Link function
#' g <- log_link()$g
#' curve(g(x), from = 0, to = 1)
#'
#' # Inverse link function
#' ginv <- log_link()$g_inv
#' curve(ginv(x), from = -5, to = 5)
#'
#' @details \code{log_link} is part of a family of generic functions with no input arguments that
#' defines and returns a list with details of the link function:
#' \enumerate{
#'    \item \code{name}: a character string with the name of the link function.
#'    \item \code{g}: implementation of the link function as a generic function in \code{R}.
#'    \item \code{g_inv}: implementation of the inverse link function as a generic function
#'          in \code{R}.
#' }
#'
#' There is a way to add new mapping functions. The user must specify the details aforesaid.
#'
#' @seealso \code{\link{maxlogL}}
#'
log_link <- function(){
  name <- "Log"
  g <- function(x) log(x)
  g_inv <- function(x) exp(x)
  out <- list(name = name, g = g, g_inv = g_inv)
  return(out)
}
#=============================================================================#
#' @title Negative inverse link function (for estimation with \code{maxlogL})
#' @family link functions
#'
#' @description
#' \code{NegInv_link} object provides a way to implement negative inverse link function that
#' \code{\link{maxlogL}} needs to perform estimation. See documentation for
#' \code{\link{maxlogL}} for further information on parameter estimation and implementation
#' of link objects.
#'
#' @return A list with negative inverse link function, its inverse and its name.
#' @export
#'
#' @examples
#' # Estimation of rate parameter in exponential distribution
#' T <- rexp(n = 1000, rate = 3)
#' lambda <- maxlogL(x = T, dist = "dexp", start = 5,
#'                   link = list(over = "rate", fun = "NegInv_link"))
#' summary(lambda)
#'
#' # Link function name
#' fun <- NegInv_link()$name
#' print(fun)
#'
#' # Link function
#' g <- NegInv_link()$g
#' curve(g(x), from = 0.1, to = 1)
#'
#' # Inverse link function
#' ginv <- NegInv_link()$g_inv
#' curve(ginv(x), from = 0.1, to = 1)
#'
#' @details \code{logit_link} is part of a family of generic functions with no input arguments that
#' defines and returns a list with details of the link function:
#' \enumerate{
#'    \item \code{name}: a character string with the name of the link function.
#'    \item \code{g}: implementation of the link function as a generic function in \code{R}.
#'    \item \code{g_inv}: implementation of the inverse link function as a generic function
#'          in \code{R}.
#' }
#'
#' There is a way to add new mapping functions. The user must specify the details aforesaid.
#'
#' @seealso \code{\link{maxlogL}}
#'
NegInv_link <- function(){
  name <- "NegInv"
  g <- function(x) -1/x
  g_inv <- function(x) -1/x
  out <- list(name = name, g = g, g_inv = g_inv)
  return(out)
}
#=============================================================================#
InvAdd_link <- function(){
  name <- "InvAdd"
  g <- function(x) -x
  g_inv <- function(x) -x
  out <- list(name = name, g = g, g_inv = g_inv)
  return(out)
}
