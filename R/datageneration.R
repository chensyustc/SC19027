#' @title A Data Generation Function for Linear Regression
#' @name datageneration
#' @description This function generates a dataset of linear model.The dataset includes design matrix X,response Y and coefficient beta.X has independent and identically distributed Gaussian rows with marginal distribution N(0,1),and correlated first 50 columns with r0.The first three coefficients are nonzero,1/sqrt(3). The model has a normal noise.
#' @importFrom stats rnorm
#' @param n the number of samples
#' @param p the number of variables
#' @param r0 the correlation of first 50 columns
#' @return X design matrix
#' @return Y response
#' @return true_beta coefficients of the model
#' @examples
#' \dontrun{
#' data<-datageneration(200,2000,0)
#' X<-data$X
#' Y<-data$Y
#' beta<-data$true_beta
#' }
#' @export
datageneration<-function(n=200,p=2000,r0=0.5){
  S <- matrix(0,p,p)+1*diag(p)
  for(i in 1:50){
    for(j in 1:50){
      if(i!=j) S[i,j]<-r0
    }
  }
  R <- chol(S) #t(R)%*%R=S
  X <- scale(matrix(rnorm(n*p),n,p)%*%R)*sqrt(n-1)/sqrt(n)
  attributes(X) <- NULL
  X <- matrix(X,n,p)
  true_beta<-c(rep(1/sqrt(3),3),rep(0,p-3))
  Y <- X%*%true_beta+rnorm(n)
  return(list(X=X, Y=c(Y), true_beta = true_beta))
}
