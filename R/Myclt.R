#' @title mycltfunc
#'
#' @param n integer
#' @param iter iterations
#'
#' @returns - histogram
#' @export
#'@importFrom stats runif
#' @examples
myclt <- function(n, iter) {
  y <- runif(n * iter, 0, 5) # A
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  sm <- apply(data, 2, sum)  # C
  hist(sm)
  return(sm)
}
