#`%*%` <- function(a,b){
 # .C("convolve1", as.double(a), as.integer(length(a)), as.double(b), as.integer(length(b)),
  #   ab = as.double(length(a)+length(b)-1))$ab

#}
ans = integer(1)
fnc <- function(x)
  return(
    try(.C("fncA", PACKAGE = "stealrugarch",n = as.integer(x), ans = ans),silent = TRUE)
    )



