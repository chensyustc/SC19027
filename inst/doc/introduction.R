## ----eval=FALSE---------------------------------------------------------------
#  datageneration<-function(n=200,p=2000,r0=0.5){
#    S <- matrix(0,p,p)+1*diag(p)
#    for(i in 1:50){
#      for(j in 1:50){
#        if(i!=j) S[i,j]<-r0
#      }
#    }
#    R <- chol(S) #t(R)%*%R=S
#    X <- scale(matrix(rnorm(n*p),n,p)%*%R)*sqrt(n-1)/sqrt(n)
#    attributes(X) <- NULL
#    X <- matrix(X,n,p)
#    true_beta<-c(rep(1/sqrt(3),3),rep(0,p-3))
#    Y <- X%*%true_beta+rnorm(n)
#    return(list(X=X, Y=c(Y), true_beta = true_beta))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  scalescad_mcp<-function(X,y,lam0,method){
#    X <- as.matrix(X)
#    y <- as.numeric(y)
#    nX=dim(X)[1]; pX=dim(X)[2]
#  
#    #calculation of gamma
#    m1<-t(X)%*%X
#    m<-m1-diag(diag(m1))
#    gamma=2/(1-max(abs(m))/nX)
#  
#    #iterative algorithm
#    obj=picasso(X,y,method=method,gamma=gamma,intercept = FALSE)
#    sigmaint=0.1; sigmanew=5; flag=0
#    while(abs(sigmaint-sigmanew)>0.0001 & flag <= 100){
#      flag=flag+1
#      sigmaint=sigmanew; lam=lam0*sigmaint
#      fit<-picasso(X,y,lambda = lam,method = method,gamma=gamma,intercept = FALSE)
#      hy=as.numeric(X %*% fit$beta[,1])
#      sigmanew=sqrt(mean((y-hy)^2))
#    }
#    hsigma=sigmanew; hlam=lam
#    hbeta=picasso(X,y,lambda = lam,method = method,gamma=gamma,intercept = FALSE)$beta[,1]
#    #results
#    return(list(hsigma=hsigma,coefficients=hbeta))
#  }

## ----eval=TRUE----------------------------------------------------------------
library(SC19027)
n<-200;p<-2000
set.seed(123)
data<-datageneration(n,p,0)
X<-data$X
Y<-data$Y
lam0<-sqrt(2*log(p)/n)
mcp<-scalescad_mcp(X,Y,lam0,method="mcp")
sigma<-mcp$hsigma
beta<-mcp$coefficients
print(sigma)
print(beta[1:10])

## ----eval=FALSE---------------------------------------------------------------
#  NumericVector resultC(double hsigma, NumericVector hbeta) {
#        NumericVector out(2);
#        out[0]=hsigma/1-1;
#        int n=hbeta.size();
#        int k=0;
#        for(int i = 0; i < n; ++i) {
#              if(hbeta[i]>0)   k=k+1;
#         }
#        out[1]=k;
#       return out;
#  }

## ----eval=TRUE----------------------------------------------------------------
n<-200;p<-2000
set.seed(124)
data<-datageneration(n,p,0)
X<-data$X
Y<-data$Y
lam0<-sqrt(2*log(p)/n)
mcp<-scalescad_mcp(X,Y,lam0,method="mcp")
scad<-scalescad_mcp(X,Y,lam0,method="scad")
r1<-resultC(mcp$hsigma,mcp$coefficients)
r2<-resultC(scad$hsigma,scad$coefficients)
a<-as.matrix(rbind(r1,r2),ncol=2)
colnames(a)<-c("bias","AMS")
rownames(a)<-c("scaled MCP","scaled SCAD")
a

