#' @title The Scaled MCP or SCAD
#' @name scalescad_mcp
#' @description The scaled penalized methods with the minimax concave penalty(MCP) or smoothly clipped absolute deviation penalty(SCAD).
#' @import picasso
#' @param X X is an n by p design matrix where n is the sample size and p is the data dimension.
#' @param y Y is the n dimensional response vector. 
#' @param lam0 the initial value of penalty level.
#' @param method the penalized methods,can be "scad" or "mcp".
#' @return  hsigma the estimated noise level
#' @return coefficients the estimated coefficients
#' @examples
#' \dontrun{
#' n<-200;p<-2000
#' data<-datageneration(n,p,0)
#' X<-data$X
#' Y<-data$Y
#' lam0<-sqrt(2*log(p)/n)
#' mcp<-scalescad_mcp(X,Y,lam0,method="mcp")
#' sigma<-mcp$hsigma
#' }
#' @export
scalescad_mcp<-function(X,y,lam0,method){
  X <- as.matrix(X)
  y <- as.numeric(y)
  nX=dim(X)[1]; pX=dim(X)[2]
  
  #calculation of gamma
  m1<-t(X)%*%X
  m<-m1-diag(diag(m1))
  gamma=2/(1-max(abs(m))/nX) 
  
  #iterative algorithm
  obj=picasso(X,y,method=method,gamma=gamma,intercept = FALSE)
  sigmaint=0.1; sigmanew=5; flag=0
  while(abs(sigmaint-sigmanew)>0.0001 & flag <= 100){
    flag=flag+1
    sigmaint=sigmanew; lam=lam0*sigmaint
    fit<-picasso(X,y,lambda = lam,method = method,gamma=gamma,intercept = FALSE)
    hy=as.numeric(X %*% fit$beta[,1])
    sigmanew=sqrt(mean((y-hy)^2))
  }
  hsigma=sigmanew; hlam=lam
  hbeta=picasso(X,y,lambda = lam,method = method,gamma=gamma,intercept = FALSE)$beta[,1]
  #results
  return(list(hsigma=hsigma,coefficients=hbeta))
}
