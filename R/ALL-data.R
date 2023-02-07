#' Acute Lymphoblastic Leukemia
#'
#' Survival times, in weeks, of 17 patients with acute leukemia. For these
#' patients, their white blood cell counts (WBC) were recorded at the time of
#' diagnosis.
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
#' data(ALL_colosimo)
#' par(mfrow = c(1,2))
#' hist(ALL_colosimo$Time, main="", xlab="Time (Days)")
#' plot(ALL_colosimo$wbc, xlab = "Status", lty = 3, type="h")
#' points(ALL_colosimo$status, pch = 16)
#'
#' @references
#' \insertRef{Colosimo2006a}{EstimationTools}
"ALL_colosimo"

# temp <- c(65, 165, 100,134, 16, 108, 121, 4, 39, 143, 56, 26, 22, 1, 1, 5, 65)
# cens <- rep(1,17)
# lwbc <- c(3.36, 2.88, 3.63, 3.41, 3.78, 4.02, 4, 4.23, 3.73,3.85,3.97,4.51, 4.54,
#           5, 5, 4.72,5)
# wbc <- c(2300, 750, 4300, 2600, 6000, 10500, 10000, 17000,5400, 7000, 9400,
#          32000, 35000, 100000, 100000, 52000, 100000)
#
# ALL_colosimo <- data.frame(times = temp, status = cens, wbc = wbc, lwbc = lwbc)
# save(ALL_colosimo, file = "./data/ALL_colosimo.rda")
