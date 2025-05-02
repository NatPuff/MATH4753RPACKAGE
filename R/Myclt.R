#' @title mycltfunc
#'
#' @param n Sample size
#' @param iter iterations
#'
#' @returns - histogram
#' @export
#'@importFrom stats runif
#' @examples
#' \dontrun{myclt(n = 30, iter = 1000)}
myclt <- function(n, iter) {
  y <- runif(n * iter, 0, 5) # A
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  sm <- apply(data, 2, sum)  # C
  hist(sm)
  return(sm)
}
