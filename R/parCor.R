#' Partial correlation
#' Calculate the partial correlation of X and Y partiallying out W
#' 
#'  @param x The x variable
#'  @param y The y variable
#'  @param w The w variable (the variable to be partialled out)
parCor <- function(x, y, w, ci = TRUE){
  cor <- (cor(x, y) - cor(x, w)*cor(y, w)) / sqrt((1 - cor(x, w)^2)*(1 - cor(w, y)^2))
  if(ci) {
    z <- qnorm(1 - .05/2)
    se <- sqrt(1/((length(x) - 1 - 3)))
    zr <- log((1 + cor)/(1 - cor))/2
    ll0 <- zr - z*se
    ul0 <- zr + z*se
    ll <- (exp(2*ll0) - 1)/(exp(2*ll0) + 1)
    ul <- (exp(2*ul0) - 1)/(exp(2*ul0) + 1)
    ci <- c(ll, ul)
    data.frame(cor, ll = ci[1], ul = ci[2])
  } else cor
}
