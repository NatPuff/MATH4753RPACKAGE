#' Title
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a upper bound shading
#'
#' @returns list
#' @export
#'
#'@importFrom grDevices rgb
#'@importFrom graphics curve polygon
#'@importFrom stats dnorm pnorm
#' @examples
myncurve <- function(mu, sigma, a) {
  # range for the x-axis
  x_min <- mu - 3 * sigma
  x_max <- mu + 3 * sigma

  normal_density <- function(x) dnorm(x, mean = mu, sd = sigma)

  curve(normal_density, from = x_min, to = x_max,
        col = "blue", lwd = 2, ylab = "Density", xlab = "X",
        main = paste("Normal Curve (mu =", mu, ", sigma =", sigma, ")"))


  x_shade <- seq(x_min, a, length.out = 100)
  y_shade <- dnorm(x_shade, mean = mu, sd = sigma)


  polygon(c(x_min, x_shade, a), c(0, y_shade, 0), col = rgb(1, 0, 0, 0.4), border = NA)

  # P(X <= a)
  prob <- pnorm(a, mean = mu, sd = sigma)

  # printing prob
  cat("P(X <=", a, ") =", prob, "\n")

  # return the list
  return(list(mu = mu, sigma = sigma, a = a, probability = prob))
}
