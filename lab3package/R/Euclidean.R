#' Recursive Euclidean algorithm function
#' 
#' @description 
#' Algorithm to find the greatest common divisor of two numbers
#' 
#' 
#' @param a A positive scalar number
#' @param b A positive scalar number
#' 
#' @return The maximum common divisor of the two numbers
#' 
#' @references 
#' 
#' Euclidean algorithm - https://en.wikipedia.org/wiki/Euclidean_algorithm
#' 
#' @examples
#' euclidean(93164, 5826)
#' euclidean(1000, 250)
#' 
euclidean <- function(a, b){
  stopifnot(length(a) == 1, length(b) == 1, is.numeric(c(a,b)), a%%1 == 0 & b%%1 == 0)
  a <- abs(a)
  b <- abs(b)
  if (b == 0){
    return (a) 
  } 
  else return (euclidean(b, a %% b)) 
}