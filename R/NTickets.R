#' Title
#'
#' @param N The number of available seats
#' @param gamma The acceptable probability of exceeding the number of available seats
#' @param p Probability of a ticket holder showing up
#'
#' @returns named list containing nd, nc, N, p and gamma
#' @export
#' @importFrom graphics abline
#' @importFrom stats pbinom uniroot
#'
#' @examples
#' \dontrun{ntickets(N=400,gamma = 0.02, p = 0.95)}
ntickets <- function(N, gamma, p) {
  #function for discrete case
  discrete_objective <- function(n) {
    return(1 - gamma - pbinom(N, n, p))
  }

  #Finding n for discrete case
  nd <- which.min(abs(sapply(N:(N*1.2), discrete_objective))) + (N - 1)

  #Function for continuous case
  continuous_objective <- function(n) {
    return(1 - gamma - pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p))))
  }

  #Finding n for continuous case
  nc <- uniroot(continuous_objective, c(N, N*1.2))$root

  ns <- seq(N - 10, N + 20, by = 1)
  discrete_vals <- sapply(ns, discrete_objective)
  continuous_vals <- sapply(ns, continuous_objective)

  par(mfrow = c(2, 1))
  plot(ns, discrete_vals, type = "o", pch = 16, col = "blue", lty = 2, main = paste("Objective Vs n to find optimal tickets sold\n(", nd, ") gamma=", gamma, "N=", N, "discrete"), xlab = "n", ylab = "Objective")
  abline(v = nd, col = "red", lwd = 2)
  abline(h = 0, col = "red", lwd = 2)
  plot(ns, continuous_vals, type = "l", col = "black", lty = 1, main = paste("Objective Vs n to find optimal tickets sold\n(", nc, ") gamma=", gamma, "N=", N, "continuous"), xlab = "n", ylab = "Objective")
  abline(v = nc, col = "blue", lwd = 2)
  abline(h = 0, col = "black", lwd = 2)
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
