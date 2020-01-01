#' @title Comparison of two scaled methods
#' @name compare
#' @description Compare the bias of estimated sigma and average model size of two methods
#' @import picasso
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm
#' @useDynLib SC19027
#' @examples
#' \dontrun{
#' n<-200;p<-2000
#' data<-datageneration(n,p,0)
#' X<-data$X
#' Y<-data$Y
#' lam0<-sqrt(2*log(p)/n)
#' mcp<-scalescad_mcp(X,Y,lam0,method="mcp")
#' scad<-scalescad_mcp(X,Y,lam0,method="scad")
#' r1<-resultC(mcp$hsigma,mcp$coefficients)
#' r2<-resultC(scad$hsigma,scad$coefficients)
#' }
NULL
