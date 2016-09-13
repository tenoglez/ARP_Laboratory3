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
  if (b == 0){
    return (a) 
  } 
  else return (euclidean(b, a %% b)) 
}