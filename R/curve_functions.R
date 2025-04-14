
# need functions for every curve
#' Gain-Loss function
#'
#' @param top model top
#' @param ga gnls Gain AC50
#' @param gw Hill coefficient in the gain direction
#' @param la AC50 in the loss direction for the gain-loss model
#' @param lw Hill coefficient in the loss direction
#' @param lconc Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
gnls_curve <- function(top, ga, gw, la, lw, lconc) {
  gain <- 1 / (1 + 10^((ga - lconc) * gw))
  loss <- 1 / (1 + 10^((lconc - la) * lw))
  return(top * gain * loss)
}

gnls_curve_unlogged <- function(top,ga,gw,la,lw, lconc){
  #gnls function with regular units
  #tp = ps[1], ga = ps[2], p = ps[3], la = ps[4], q = ps[5]
  x <- 10^lconc
  gn <- 1/(1 + (ga/x)^gw)
  ls <- 1/(1 + (x/la)^lw)
  return(top*gn*ls )
}

#' Hill Function
#'
#' @param hill_tp Top parametre for hill model
#' @param hill_ga Gain ac50
#' @param hill_gw Hill coefficient in the gain direction
#' @param lconc Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
hill_curve <- function(hill_tp, hill_ga, hill_gw, lconc) {
  hill_tp / (1 + 10^((hill_ga - lconc) * hill_gw))
}

hill_curve_unlogged = function(hill_tp,hill_ga,hill_gw,lconc){
  #hill function with regular units
  #tp = ps[1], ga = ps[2], p = ps[3]
  x <- 10^lconc
  return(hill_tp/(1 +  (hill_ga/x)^hill_gw) )
}

#' Title
#'
#' @param ps Unused in cnst function
#' @param x Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
cnst <- function(ps, x) {
  # ignores ps
  return(rep(0, length(x)))
}

# function definition for exp2
#' exp2
#'
#' @param a Function Parameter
#' @param b Function Parameter
#' @param x Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
exp2 <- function(a, b, x) {
  x <- 10^x
  return(a * (exp(x / b) - 1))
}

#' exp3
#'
#' @param a Function Parameter
#' @param b Function Parameter
#' @param p Function Parameter
#' @param x Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
exp3 <- function(a, b, p, x) {
  x <- 10^x
  # a = ps[1], b = ps[2], p = ps[3]
  return(a * (exp((x / b)^p) - 1))
}

#' Exp 4
#'
#' @param tp model top
#' @param ga  Function Parameter
#' @param x Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
exp4 <- function(tp, ga, x) {
  x <- 10^x
  # tp = ps[1], ga = ps[2]
  return(tp * (1 - 2^(-x / ga)))
}


#' Exp5
#'
#' @param tp Function Parameter
#' @param ga Function Parameter
#' @param p Function Parameter
#' @param x Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
exp5 <- function(tp, ga, p, x) {
  x <- 10^x
  # tp = ps[1], ga = ps[2], p = ps[3]
  return(tp * (1 - 2^(-(x / ga)^p)))
}

#' Poly1
#'
#' @param a  Function Parameter
#' @param x Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
poly1 <- function(a, x) {
  x <- 10^x
  # a = ps[1]
  return(a * x)
}

#' Poly2
#'
#' @param a Function Parameter
#' @param b Function Parameter
#' @param x Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
poly2 <- function(a, b, x) {
  x <- 10^x
  # a = ps[1], b = ps[2]
  x0 <- x / b
  return(a * (x0 + x0 * x0))
}

#' Pow
#'
#' @param a Function Parameter
#' @param p Function Parameter
#' @param x Log base 10 concentration
#'
#' @return response value at given concentration based on function parameters
#' @export
#'
pow <- function(a, p, x) {
  x <- 10^x
  # a = ps[1], p = ps[2]
  return(a * x^p)
}
