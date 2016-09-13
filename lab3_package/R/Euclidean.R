#Recursive Euclidean function
euclidean <- function(a, b){
  if (b == 0){
    return (a) 
  } 
  else return (euclidean(b, a %% b)) 
}