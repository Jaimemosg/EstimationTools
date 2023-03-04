#' Acute Lymphoblastic Leukemia, table 4.1
#'
#' Survival times, in weeks, of 17 patients with acute leukemia. For these
#' patients, their white blood cell counts (WBC) were recorded at the time of
#' diagnosis. The variables are:
#'
#' \itemize{
#'   \item times: time-to-event (in weeks).
#'   \item status: censorship indicators.
#'   \item wbc: WBC, white blood cells counts.
#'   \item lwbc: base-10 logarithms of the WBC.
#' }
#'
#' @format A data frame with 17 observations.
#' @examples
#' data(ALL_colosimo_table_4_1)
#' hist(ALL_colosimo_table_4_1$time, main="", xlab="Time (Weeks)")
#'
#' @keywords dataset
#' @references
#' \insertRef{Colosimo2006a}{EstimationTools}
"ALL_colosimo_table_4_1"

#' Acute Lymphoblastic Leukemia, table 4.3
#'
#' Survival times, in weeks, of 17 patients with acute leukemia. For these
#' patients, their white blood cell counts (WBC) were recorded at the time of
#' diagnosis. The variables are:
#'
#' \itemize{
#'   \item times: time-to-event (in weeks).
#'   \item status: censorship indicators.
#'   \item wbc: WBC, white blood cells counts.
#'   \item lwbc: base-10 logarithms of the WBC.
#'   \item Ag_plus: whether the subject expresses the Calla antigen
#'         (\code{Ag_plus} = 1) or not (\code{Ag_plus} = 0).
#' }
#'
#' @format A data frame with 33 observations.
#' @examples
#' data(ALL_colosimo_table_4_3)
#' hist(ALL_colosimo_table_4_3$time, main="", xlab="Time (Weeks)")
#'
#' @keywords dataset
#' @references
#' \insertRef{Colosimo2006a}{EstimationTools}
"ALL_colosimo_table_4_3"

# Source code to build the data frames -----
# temp1 <- c(65, 156, 100,134, 16, 108, 121, 4, 39, 143, 56, 26, 22, 1, 1, 5, 65)
# cens1 <- rep(1, 17)
# lwbc1 <- c(
#   3.36, 2.88, 3.63, 3.41, 3.78, 4.02, 4, 4.23,
#   3.73,3.85,3.97,4.51, 4.54, 5, 5, 4.72, 5
# )
# wbc1 <- c(2300, 750, 4300, 2600, 6000, 10500, 10000, 17000,5400, 7000, 9400,
#          32000, 35000, 100000, 100000, 52000, 100000)
# Ag_plus1 <- rep(1, 17)
#
# ALL_colosimo_table_4_1 <- data.frame(
#   times = temp1, status = cens1, wbc = wbc1, lwbc = lwbc1
# )
# save(ALL_colosimo_table_4_1, file = "./data/ALL_colosimo_table_4_1.rda")
#
# temp2 <- c(56, 65, 17, 7, 16, 22, 3, 4, 2, 3, 8, 4, 3, 30, 4, 43)
# cens2 <- rep(1, 16)
# lwbc2 <- c(
#   3.64, 3.48, 3.60, 3.18, 3.95, 3.72, 4.00, 4.28,
#   4.43, 4.45, 4.49, 4.41, 4.32, 4.90, 5.00, 5.00
# )
# wbc2 <- c(4400, 3000,4000, 1500, 9000, 5300, 10000, 19000, 27000, 28000,
#           31000, 26000,2100, 79000, 100000, 100000)
# Ag_plus2 <- rep(0, 16)
#
# ALL_colosimo_table_4_3 <- data.frame(
#   times = c(temp1, temp2),
#   status = c(cens1, cens2),
#   wbc = c(wbc1, wbc2),
#   lwbc = c(lwbc1, lwbc2),
#   Ag_plus = as.factor(c(Ag_plus1, Ag_plus2))
# )
#
# save(ALL_colosimo_table_4_3, file = "./data/ALL_colosimo_table_4_3.rda")
