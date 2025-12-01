#' Birthday Function
#'
#' @param x # The number of people
#'
#' @returns # Returns the probability of 2 people in the sample having the same birthday
#' @export
#'
#' @examples birthday(20:24)
birthday <- function(x) {
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}


