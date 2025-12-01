#' Bootstrap confidence interval for a statistic
#'
#' Compute a percentile bootstrap confidence interval for a statistic and plot
#' a histogram of the bootstrap replicates.
#'
#' @param iter Integer. Number of bootstrap iterations. Default is 10000.
#' @param x Numeric vector. The sample data to bootstrap.
#' @param fun Function or character. Statistic to compute (e.g. \code{mean}, \code{median}). Default is \code{mean}.
#' @param alpha Numeric in (0,1). Significance level for the CI. Default is 0.05 (95\% CI).
#' @param cx Numeric. Scaling factor for text in the plot. Default is 1.5.
#' @param seed Optional integer. If provided, sets a seed for reproducible resampling.
#' @param ... Additional graphical parameters passed to \code{hist()} (e.g., \code{xlab}, \code{col}, \code{main}).
#'
#' @return A (invisible) list with components:
#' \describe{
#'   \item{ci}{Numeric vector of length 2: lower and upper percentile bootstrap CI.}
#'   \item{fun}{The function used for the statistic.}
#'   \item{x}{The original sample vector.}
#'   \item{xstat}{Numeric vector of bootstrap statistics.}
#'   \item{pte}{Point estimate of the statistic from the original sample.}
#'   \item{iter}{Number of bootstrap iterations used.}
#'   \item{alpha}{Alpha level used.}
#' }
#'
#' @examples
#' myboot2(iter = 1000, x = rnorm(30), fun = mean, alpha = 0.05,
#'         xlab = "Bootstrap mean", col = "purple", cx = 1.2)
#' @importFrom stats quantile
#' @importFrom graphics segments
#'
#' @export
myboot2 <- function(iter = 10000, x, fun = mean, alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)

  # Resample with replacement
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Apply the statistic
  xstat <- apply(rs.mat, 2, fun)

  # Confidence interval
  ci <- quantile(xstat, c(alpha/2, 1 - alpha/2))

  # Histogram of bootstrap statistics
  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap sample statistics\n",
                            "alpha =", alpha, " iter =", iter, sep = ""),
               ...)

  # Point estimate
  mat <- matrix(x, nrow = n, ncol = 1)
  pte <- apply(mat, 2, fun)

  abline(v = pte, lwd = 3, col = "black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste0("(", round(ci[1], 2)), col = "red", cex = cx)
  text(ci[2], 0, paste0(round(ci[2], 2), ")"), col = "red", cex = cx)
  text(pte, max(para$density)/2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x, xstat = xstat, pte = pte))
}
