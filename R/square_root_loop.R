#' Function to compute the square root of a number
#' @param a the number whose square root is computed
#' @param init an initial guess
#' @param eps *optional* the precision. Default value: 0.01
#' @param iter *optional* the number of iteration. Default value: 100
#' @description This function computes the square root of a number using a loop.
#' @examples
#' sqrt_newton(81,2)
#' @import dplyr
#' @export
sqrt_newton <- function(a, init, eps = 0.01){
  stopifnot(a >= 0)

  while(abs(init**2 - a) > eps){
    init <- 1/2 *(init + a/init)

  }
  return(init)
}
