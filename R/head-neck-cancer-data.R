#' Head and neck cancer
#'
#' Time-to-event data a randomized clinical trial to compare two therapies for
#' head and neck cancer.51 patients were treated with radiation only and 45
#' patients treated with radiation plus chemotherapy. The variables are:
#'
#' \itemize{
#'   \item Time: time (in days) to recurrence of the cancer.
#'   \item Therapy: treatment applied to the patients.
#'   \item Status: censorship indicators.
#' }
#'
#' @format A data frame with 96 observations.
#' @examples
#' data(head_neck_cancer)
#' par(mfrow = c(1,2))
#' hist(head_neck_cancer$Time, main="", xlab="Time (Days)")
#' plot(head_neck_cancer$Time, xlab = "Patient (subjects)", lty = 3, type="h")
#'
#' @references
#' \insertRef{Khan2018}{EstimationTools}
"head_neck_cancer"
