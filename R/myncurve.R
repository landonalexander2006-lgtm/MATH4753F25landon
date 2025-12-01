#' Draws the N(mu, sigma^2) density, shades the region (-Inf, a],
#' and returns a named list with mu, sigma, and the probability P(X <= a).
#'
#' @param mu    Mean of the normal distribution (numeric)
#' @param sigma Standard deviation (> 0)
#' @param a     Cuttoff for shading; area is (-inf, a] (numeric)
#'
#' @returns list with components: mu, sigma, prob
#
#' @export
#'
#' @examples myncurve(10, 5, 6)
#' @importFrom stats dnorm pnorm
#' @importFrom graphics polygon text
#' @importFrom grDevices adjustcolor rainbow
myncurve <- function(mu, sigma, a) {
  stopifnot(is.numeric(mu), is.numeric(sigma), is.numeric(a), length(mu)==1, length(sigma)==1, length(a)==1)
  if (sigma <= 0) stop("sigma must be > 0")

  # plotting window
  xlim <- c(mu - 4 * sigma, mu + 4 * sigma)
  xs   <- seq(xlim[1], xlim[2], length.out = 1200)
  ys   <- dnorm(xs, mean = mu, sd = sigma)

  plot(xs, ys, type = "l", xlab = "x", ylab = "density",
       main = sprintf("N(%.3g, %.3g^2) \u2014 shaded P(X \u2264 %.3g)", mu, sigma, a))

  # shade (-Inf, a] intersected with plotting window
  left  <- xlim[1]
  right <- min(a, xlim[2])
  if (left < right) {
    xshade <- seq(left, right, length.out = 600)
    yshade <- dnorm(xshade, mean = mu, sd = sigma)
    polygon(c(left, xshade, right), c(0, yshade, 0),
            border = NA, col = adjustcolor("gray50", 0.4))
  }

  prob <- pnorm(a, mean = mu, sd = sigma)
  # annotate the plot with the numeric value (4 d.p.)
  text(x = mu, y = max(ys) * 0.9, labels = paste0("P(X \u2264 a) = ", sprintf("%.4f", prob)))

  list(mu = mu, sigma = sigma, prob = prob)
}
