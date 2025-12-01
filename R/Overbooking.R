#' Discrete and normal approximations to the
#' overbooking problem
#'
#' @param N number of seats
#' @param gamma desired maximum overbooking
#' probability
#' @param p probability of people showing up
#' @param max_mult maximum for search upper
#'  bound
#' @param plot Should plots be shown
#'
#' @returns a list with elements nd, nc, N, p,
#' gamma, and two  plots showing the discrete
#' and continuous cases
#' @importFrom graphics abline
#' @importFrom stats pbinom uniroot
#'
#' @export
#'
#' @examples ntickets(400, 0.02, 0.95, 5, TRUE)
ntickets <- function(N, gamma, p, max_mult = 5, plot = TRUE) {
  if (!is.numeric(N) || N != floor(N) || N < 1) stop("N must be a positive integer")
  if (!is.numeric(p) || p <= 0 || p >= 1) stop("p must be in (0,1)")
  if (!is.numeric(gamma) || gamma < 0 || gamma >= 1) stop("gamma must be in [0,1)")


  discrete_overbook_prob <- function(n) 1 - pbinom(N, size = n, prob = p)
  ns <- seq(N, max_mult * N)
  probs <- vapply(ns, discrete_overbook_prob, numeric(1))
  idx <- which(probs <= gamma)
  if (length(idx) == 0) stop("Discrete solution not found. Increase max_mult.")
  nd <- ns[idx[1]]


  f_normal <- function(n) {
    mu <- n * p
    sd <- sqrt(n * p * (1 - p))
    if (!is.finite(sd) || sd <= 0) return((ifelse(N < mu, 1, 0)) - gamma)
    val <- 1 - pnorm((N + 0.5 - mu) / sd) - gamma
    if (!is.finite(val)) val <- sign(val) * .Machine$double.xmax
    val
  }
  lower <- N
  upper <- max(2*N, N+10)
  cap <- max_mult * N
  while (f_normal(lower) * f_normal(upper) > 0 && upper < cap) upper <- min(upper + N, cap)
  if (f_normal(lower) * f_normal(upper) > 0) {
    nc <- ceiling(upper)
  } else {
    root <- uniroot(f_normal, lower = lower, upper = upper, tol = 1e-6)
    nc <- ceiling(root$root)
  }

  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)


  if (plot) {

    ns_d <- seq(max(N, nd - 30), nd + 30)
    obj_d <- vapply(ns_d, function(nn) 1 - pbinom(N, size = nn, prob = p) - gamma, numeric(1))
    plot(ns_d, obj_d, type = "l", lwd = 2, col = "blue",
         xlab = "n", ylab = "Objective",
         main = "Objective vs n")
    abline(h = 0, col = "red", lty = 2)
    abline(v = nd, col = "darkblue", lwd = 3)

    ns_c <- seq(max(N, nc - 30), nc + 30, length.out = 400)
    obj_c <- vapply(ns_c, function(nn) {
      mu <- nn * p
      sd <- sqrt(nn * p * (1 - p))
      if (!is.finite(sd) || sd <= 0) return(1 - gamma)
      1 - pnorm((N + 0.5 - mu) / sd) - gamma
    }, numeric(1))
    plot(ns_c, obj_c, type = "l", lwd = 2, col = "green",
         xlab = "n", ylab = "Objective",
         main = "Objective vs n")
    abline(h = 0, col = "red", lty = 2)
    abline(v = nc, col = "darkgreen", lwd = 3)
  }

  print(result)
  invisible(result)
}
