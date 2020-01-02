## -----------------------------------------------------------------------------
a = rnorm(50,mean=5)
m = mean(a)
s = sd(a)
c(m-qnorm(0.975)*s/sqrt(50),m+qnorm(0.975)*s/sqrt(50)) 

## -----------------------------------------------------------------------------
knitr::kable(head(airquality,n=10))

## -----------------------------------------------------------------------------
x <- 1:10
y <- c(1,3,8,4,6,7,5,4,8,9)
plot(x,y,asp=1,type="p")                                  
lmout <- lm(y ~ x)                                                  
abline(lmout)  

## -----------------------------------------------------------------------------
#function of Rayleigh(σ) distribution.
f<-function(x,s) x/(s^2)*exp(-x^2/(2*s^2))
#function of random samples from a Rayleigh(σ) distribution.
samp4<-function(n,s){
  x<-rnorm(n,s,s)
  y<-runif(n,0,2*dnorm(x,s,s))
  z<-x[y<f(x,s)]
  z
}

## -----------------------------------------------------------------------------
set.seed(11)
samp<- samp4(10000,0.5)[1:5000]
hist(samp, freq=FALSE, breaks=seq(0,3,0.05),border="darkblue",main="histogram of sample for σ=0.5")
curve(f(x,0.5),from=0, to=3, add=T, col="red", lwd=2)

## -----------------------------------------------------------------------------
set.seed(10)
samp<- samp4(10000,1)[1:5000]
hist(samp, freq=FALSE, breaks=seq(0,5,0.05),border="darkblue",main="histogram of sample for σ=1")
curve(f(x,1),from=0, to=5, add=T, col="red", lwd=2)

## -----------------------------------------------------------------------------
set.seed(14)
samp<- samp4(10000,1.5)[1:5000]
hist(samp, freq=FALSE, breaks=seq(0,7,0.1),border="darkblue",main="histogram of sample for σ=1.5")
curve(f(x,1.5),from=0, to=7, add=T, col="red", lwd=2)

## -----------------------------------------------------------------------------
set.seed(22)
samp<- samp4(10000,2)[1:5000]
hist(samp, freq=FALSE, breaks=seq(0,10,0.1),border="darkblue",main="histogram of sample for σ=2")
curve(f(x,2),from=0, to=10, add=T, col="red", lwd=2)

## -----------------------------------------------------------------------------
samp11 <- function(n,p){  
  s <- rbinom(10000, 1, 1-p)                    
  S <- rnorm(n, 3*s)                 
  S  }

## -----------------------------------------------------------------------------
#the real density
f11 <- function(x,p) p*dnorm(x,0)+(1-p)*dnorm(x,3) 
#random sample for p=0.75
set.seed(2235)
F11<-samp11(1000,0.75)
SAMP<-F11[F11>-3&F11<6]
#Graph the histogram of the sample with density superimposed
hist(SAMP,freq=FALSE,breaks=seq(-3,6,0.2),ylim=c(0,0.4), axes=FALSE, border="darkblue",main="histogram of sample for p1=0.75")
axis(1,at=(-3):6)
axis(2,at=seq(0,0.4,0.1))
curve(f11(x,p=0.75),from=-3,to=6,add=T,col="red",lwd=2)

## -----------------------------------------------------------------------------
#random sample for p=0.15
set.seed(12)
F11<-samp11(1000,0.15)
SAMP<-F11[F11>-3&F11<6]      
#Graph the histogram of the sample with density superimposed
hist(SAMP,freq=FALSE,breaks=seq(-3,6,0.2),ylim=c(0,0.4), axes=FALSE, border="darkblue",main="histogram of sample for p1=0.15")
axis(1,at=(-3):6)
axis(2,at=seq(0,0.4,0.1))
curve(f11(x,p=0.15),from=-3,to=6,add=T,col="red",lwd=2)

## -----------------------------------------------------------------------------
#random sample for p=0.25
set.seed(2334)
F11<-samp11(1000,0.25)
SAMP<-F11[F11>-3&F11<6]      
#Graph the histogram of the sample with density superimposed
hist(SAMP,freq=FALSE,breaks=seq(-3,6,0.2),ylim=c(0,0.4), axes=FALSE, border="darkblue",main="histogram of sample for p1=0.25")
axis(1,at=(-3):6)
axis(2,at=seq(0,0.4,0.1))
curve(f11(x,p=0.25),from=-3,to=6,add=T,col="red",lwd=2)

## -----------------------------------------------------------------------------
#random sample for p=0.5
set.seed(2346)
F11<-samp11(1000,0.5)
SAMP<-F11[F11>-3&F11<6]      
#Graph the histogram of the sample with density superimposed
hist(SAMP,freq=FALSE,breaks=seq(-3,6,0.2),ylim=c(0,0.4), axes=FALSE, border="darkblue",main="histogram of sample for p1=0.5")
axis(1,at=(-3):6)
axis(2,at=seq(0,0.4,0.1))
curve(f11(x,p=0.5),from=-3,to=6,add=T,col="red",lwd=2)

## -----------------------------------------------------------------------------
#random sample for p=0.65
set.seed(23)
F11<-samp11(1000,0.65)
SAMP<-F11[F11>-3&F11<6]      
#Graph the histogram of the sample with density superimposed
hist(SAMP,freq=FALSE,breaks=seq(-3,6,0.2),ylim=c(0,0.4), axes=FALSE, border="darkblue",main="histogram of sample for p1=0.65")
axis(1,at=(-3):6)
axis(2,at=seq(0,0.4,0.1))
curve(f11(x,p=0.65),from=-3,to=6,add=T,col="red",lwd=2)

## -----------------------------------------------------------------------------
#random sample for p=0.85
set.seed(234)
F11<-samp11(1000,0.85)
SAMP<-F11[F11>-3&F11<6]      
#Graph the histogram of the sample with density superimposed
hist(SAMP,freq=FALSE,breaks=seq(-3,6,0.2),ylim=c(0,0.4), axes=FALSE, border="darkblue",main="histogram of sample for p1=0.85")
axis(1,at=(-3):6)
axis(2,at=seq(0,0.4,0.1))
curve(f11(x,p=0.85),from=-3,to=6,add=T,col="red",lwd=2)

## ----eval=FALSE---------------------------------------------------------------
#  wishart<-function(sig,d,n){
#    T<-matrix(0,nrow=d,ncol=d)
#    for(i in 1:d){
#      for(j in 1:d){
#        if(i>j) T[i,j]<-rnorm(1,0,1)
#        else if(i==j) T[i,j]<-sqrt(rchisq(1,df=n-i+1))
#      }
#    }
#    A<-T%*%t(T)  #Wd(Id,n) distribution
#    L<-t(chol(sig))  #lower triangular
#    W<-L%*%A%*%t(L)  #Bartlett’s decomposition
#    W
#  }

## -----------------------------------------------------------------------------
set.seed(124)
m<-10000
x<-runif(m,min=0,max=pi/3)
theta.hat<-mean(sin(x))*(pi/3)
print(c(theta.hat,0.5))

## -----------------------------------------------------------------------------
MC<-function(R=10000,antithetic=TRUE){
  u<-runif(R/2)
  if(!antithetic) v<-runif(R/2) 
  else v<-1-u
  u<-c(u,v)
  g<-mean(exp(-u)/(1+u^2))
  g
}
set.seed(1234)
theta.hat<-MC(anti=TRUE)
#true value of the integral
f<-function(x) exp(-x)/(1+x^2)
theta<-integrate(f,0,1)
print(c(theta.hat,theta$value))
#percentage of variance reduction
m<-1000
MC1<-MC2<-numeric(m)
for(i in 1:m){
  MC1[i]<-MC(anti=FALSE)
  MC2[i]<-MC(anti=TRUE)
}
print(c(sd(MC1),sd(MC2),(sd(MC1)-sd(MC2))/sd(MC1)))

## -----------------------------------------------------------------------------
m<-10000
k<-5
r<-m/k
N<-1000
T2<-numeric(k)
est<-matrix(0,N,2)
a<-numeric(k+1)
for(j in 1:(k+1)) a[j]<--log(1-(j-1)/k*(1-exp(-1)))
set.seed(1235)
for(i in 1:N){
  u1<-runif(m)
  x<--log(1-u1*(1-exp(-1)))
  fg<-(1-exp(-1))/(1+x^2)
  est[i,1]<-mean(fg)
  for(j in 1:k){
    u2<-runif(r)
    x<--log(exp(-a[j])-u2*(1-exp(-1))*1/5)
    fg<-1/5*(1-exp(-1))/(1+x^2)
    T2[j]<-mean(fg)*5
  }
  est[i,2]<-mean(T2)
}
estimate<-apply(est,2,mean)
sd<-apply(est,2,sd)
print(rbind(estimate,sd))

## -----------------------------------------------------------------------------
n <- 20
alpha <- 0.05
CI1 <- replicate(1000, expr = {
x <- rchisq(n, df = 2)
mean(x)-qt(1-alpha/2, df = n-1)*sd(x)/sqrt(n)
} )
CI2 <- replicate(1000, expr = {
x <- rchisq(n, df = 2)
mean(x)+qt(1-alpha/2, df = n-1)*sd(x)/sqrt(n)
} )
#The estimation of the coverage probability
mean(CI1<2&CI2>2)

## -----------------------------------------------------------------------------
n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
x <- rchisq(n, df = 2)
(n-1) * var(x) / qchisq(alpha, df=n-1)
} )
mean(UCL>4)

## -----------------------------------------------------------------------------
print(c(mean(CI1<2&CI2>2),mean(UCL>4)))

## -----------------------------------------------------------------------------
m<-1000
n<-1000
q<-c(0.025,0.05,0.95,0.975)
skew <- matrix(0,ncol=4,nrow=m)
for(j in 1:m){
  a<-numeric(n)
  for(i in 1:n){
    x<-rnorm(n)
    a[i]<-mean((x-mean(x))^3)/(var(x))^(3/2)
  }
  skew[j,1]<-quantile(a,q[1])
  skew[j,2]<-quantile(a,q[2])
  skew[j,3]<-quantile(a,q[3])
  skew[j,4]<-quantile(a,q[4])
}
#estimated quantiles
sq_b1<-apply(skew,2,mean)
#quantiles of the large sample approximation
b<-numeric(4)
for(i in 1:4){
  b[i]<-qnorm(q[i],0,sqrt(6/m))
}
#the standard error of the estimates
se<-numeric(4)
for(i in 1:4){
  se[i]<-sqrt((1/m)*q[i]*(1-q[i])/dnorm(b[i],0,sqrt(6*(m-2)/((m+1)*(m+3)))))
}
print(se)

## -----------------------------------------------------------------------------
a<-rbind(sq_b1,b)
rownames(a)<-c("estimated quantiles","quantiles of approximation")
colnames(a)<-q
print(a)

## -----------------------------------------------------------------------------
#function to compute the sample skewness statistic
sk <- function(x) {
  #computes the sample skewness coeff
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  return( m3 / m2^1.5 )
}

n <- 50
m <- 1000
alpha<-1:20
N <- length(alpha)
pow <- numeric(N)
#critical value for the skewness test
cv <- qnorm(0.975,0,sqrt(6*(n-2)/((n+1)*(n+3))))
#Power of the skewness test of normality
for (j in 1:N) { 
   a <- alpha[j]
   sktests <- numeric(m)
   for (i in 1:m) { 
       x <- rbeta(n,a,a)
       sktests[i] <- as.integer(abs(sk(x)) >= cv)
   }
   pow[j] <- mean(sktests)
}
#plot power vs alpha
plot(alpha, pow, type = "b",xlab = bquote(alpha), ylim = c(0,0.1))
abline(h = 0.05, lty = 3)
se <- sqrt(pow * (1-pow) / m) #add standard errors
lines(alpha, pow+se, lty = 3)
lines(alpha, pow-se, lty = 3)

## -----------------------------------------------------------------------------
n <- 50
m <- 1000
v<-1:20
N <- length(v)
pow <- numeric(N)
#critical value for the skewness test
cv <- qnorm(0.975,0,sqrt(6*(n-2)/((n+1)*(n+3))))
#Power of the skewness test of normality
for (j in 1:N) { #for each v
   a <- v[j]
   sktests <- numeric(m)
   for (i in 1:m) { #for each replicate
       x <- rt(n,a)
       sktests[i] <- as.integer(abs(sk(x)) >= cv)
   }
   pow[j] <- mean(sktests)
}
#plot power vs v
plot(v, pow, type = "b",xlab = bquote(v), ylim = c(0,1))
abline(h = 0.05, lty = 3)
se <- sqrt(pow * (1-pow) / m) #add standard errors
lines(alpha, pow+se, lty = 3)
lines(alpha, pow-se, lty = 3)

## -----------------------------------------------------------------------------
n <- c(10,20,40,50,80,100)
N<-length(n)
t1<-numeric(N)
se.hat<-numeric(N)
alpha <- 0.05
mu0 <- 1
m <- 10000 #number of replicates
for (i in 1:N) {
  l<-n[i]
  p<-numeric(m)
  for (j in 1:m) {
        x <- rchisq(l, 1)
        ttest <- t.test(x,  mu = mu0)
        p[j] <- ttest$p.value
   }
  t1[i] <- mean(p < alpha)
  se.hat[i] <- sqrt(t1[i] * (1 - t1[i]) / m)
}
X<-rbind(t1,se.hat)
colnames(X)<-c(10,20,40,50,80,100)
print(X)

## -----------------------------------------------------------------------------
n <- c(10,20,40,50,80,100)
N<-length(n)
t1<-numeric(N)
se.hat<-numeric(N)
alpha <- 0.05
mu0 <- 1
m <- 10000 #number of replicates
for (i in 1:N) {
  l<-n[i]
  p<-numeric(m)
  for (j in 1:m) {
        x <- runif(l,0,2)
        ttest <- t.test(x,  mu = mu0)
        p[j] <- ttest$p.value
   }
  t1[i] <- mean(p < alpha)
  se.hat[i] <- sqrt(t1[i] * (1 - t1[i]) / m)
}
X<-rbind(t1,se.hat)
colnames(X)<-c(10,20,40,50,80,100)
print(X)

## -----------------------------------------------------------------------------
n <- c(10,20,40,50,80,100)
N<-length(n)
t1<-numeric(N)
se.hat<-numeric(N)
alpha <- 0.05
mu0 <- 1
m <- 10000 #number of replicates
for (i in 1:N) {
  l<-n[i]
  p<-numeric(m)
  for (j in 1:m) {
        x <- rexp(l,1)
        ttest <- t.test(x, mu = mu0)
        p[j] <- ttest$p.value
   }
  t1[i] <- mean(p < alpha)
  se.hat[i] <- sqrt(t1[i] * (1 - t1[i]) / m)
}
X<-rbind(t1,se.hat)
colnames(X)<-c(10,20,40,50,80,100)
print(X)

## -----------------------------------------------------------------------------
X<-c(6510,3490,3240,6760)
dim(X)<-c(2,2)
mcnemar.test(X,correct=FALSE)

## -----------------------------------------------------------------------------
library(bootstrap)
head(scor)

## -----------------------------------------------------------------------------
pairs(scor)
cor(scor)

## -----------------------------------------------------------------------------
#correlation functions
r12<-function(x,i){
  cor(x[i,1],x[i,2])
}
r34<-function(x,i){
  cor(x[i,3],x[i,4])
}
r35<-function(x,i){
  cor(x[i,3],x[i,5])
}
r45<-function(x,i){
  cor(x[i,4],x[i,5])
}
#bootstrap estimates
library(boot)
set.seed(1234)
obj12<-boot(data=scor,statistic=r12,R=2000)
obj34<-boot(data=scor,statistic=r34,R=2000)
obj35<-boot(data=scor,statistic=r35,R=2000)
obj45<-boot(data=scor,statistic=r45,R=2000)
r<-c(obj12$t0,obj34$t0,obj35$t0,obj45$t0)
b<-c(mean(obj12$t)-obj12$t0,mean(obj34$t)-obj34$t0,mean(obj35$t)-obj35$t0,mean(obj45$t)-obj45$t0)
se<-c(sd(obj12$t),sd(obj34$t),sd(obj35$t),sd(obj45$t))
X<-cbind(r,b,se)
colnames(X)<-c("correlation","bias","se")
rownames(X)<-c("ρ12","ρ34","ρ35","ρ45")
round(X,5)

## -----------------------------------------------------------------------------
n<-100
m<-10000
set.seed(123)
#sample data matrix of normal distributions and χ2(5) distributions
skewness<-function(X,i){
  x<-X[i]
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  return( m3 / m2^1.5 )
}
ci.norm1<-ci.basic1<-ci.perc1<-matrix(0,m,2)
ci.norm2<-ci.basic2<-ci.perc2<-matrix(0,m,2)
for(i in 1:m){
  X1<-rnorm(n)
  boot1<-boot(data=X1,statistic = skewness,R=1000)
  ci1<-boot.ci(boot1,type=c("norm","basic","perc"))
  ci.norm1[i,]<-ci1$norm[2:3]
  ci.basic1[i,]<-ci1$basic[4:5]
  ci.perc1[i,]<-ci1$percent[4:5]
}
for(i in 1:m){
  X2<-rchisq(n,5)
  boot2<-boot(data=X2,statistic = skewness,R=1000)
  ci2<-boot.ci(boot2,type=c("norm","basic","perc"))
  ci.norm2[i,]<-ci2$norm[2:3]
  ci.basic2[i,]<-ci2$basic[4:5]
  ci.perc2[i,]<-ci2$percent[4:5]
}

## -----------------------------------------------------------------------------
mu1<-0
mu2<-sqrt(8/5)
c1<-c(mean(ci.norm1[,1]<=mu1&ci.norm1[,2]>=mu1),mean(ci.basic1[,1]<=mu1&ci.basic1[,2]>=mu1),mean(ci.perc1[,1]<=mu1&ci.perc1[,2]>=mu1))
c2<-c(mean(ci.norm2[,1]<=mu2&ci.norm2[,2]>=mu2),mean(ci.basic2[,1]<=mu2&ci.basic2[,2]>=mu2),mean(ci.perc2[,1]<=mu2&ci.perc2[,2]>=mu2))
X<-rbind(c1,c2)
colnames(X)<-c("norm","basic","perc")
rownames(X)<-c("normal","χ2(5)")
print(X)

## -----------------------------------------------------------------------------
l1<-c(mean(ci.norm1[,1]>mu1),mean(ci.basic1[,1]>mu1),mean(ci.perc1[,1]>mu1))
l2<-c(mean(ci.norm2[,1]>mu2),mean(ci.basic2[,1]>mu2),mean(ci.perc2[,1]>mu2))
r1<-c(mean(ci.norm1[,2]<mu1),mean(ci.basic1[,2]<mu1),mean(ci.perc1[,2]<mu1))
r2<-c(mean(ci.norm2[,2]<mu2),mean(ci.basic2[,2]<mu2),mean(ci.perc2[,2]<mu2))
X1<-rbind(l1,r1,l2,r2)
colnames(X1)<-c("norm","basic","perc")
rownames(X1)<-c("left miss(normal)","right miss(normal)","left miss(χ2(5))","right miss(χ2(5))")
print(X1)

## -----------------------------------------------------------------------------
library(bootstrap)
#proportion function
x<-as.matrix(scor)
theta<-function(x,n){
  sigma<-(n-1)/n*cov(x)
  lamda<-eigen(sigma)$values
  theta<-lamda[1]/sum(lamda)
  theta
}
n<-nrow(x)
theta.hat<-theta(x,n)
theta.jack<-numeric(n)
for(i in 1:n) theta.jack[i]<-theta(x[-i,],n-1)
bias.jack<-(n-1)*(mean(theta.jack)-theta.hat)
se.jack<-sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))
round(c(original=theta.hat,bias.jack=bias.jack,se.jack=se.jack),3)

## -----------------------------------------------------------------------------
library(DAAG); attach(ironslag)
a <- seq(10, 40, .1) #sequence for plotting fits

L1 <- lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear", pch=16)
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)

L2 <- lm(magnetic ~ chemical + I(chemical^2))
plot(chemical, magnetic, main="Quadratic", pch=16)
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)

L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
lines(a, yhat3, lwd=2)

L4 <- lm(magnetic ~ chemical + I(chemical^2)+ I(chemical^3))
plot(chemical, magnetic, main="Cubic", pch=16)
yhat4 <- L4$coef[1] + L4$coef[2] * a + L4$coef[3] * a^2 + L4$coef[4] * a^3
lines(a, yhat4, lwd=2)

## -----------------------------------------------------------------------------
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)

for (k in 1:n) {
   y <- magnetic[-k]
   x <- chemical[-k]
   
   L1 <- lm(y ~ x)
   yhat1 <- L1$coef[1] + L1$coef[2] * chemical[k]
   e1[k] <- magnetic[k] - yhat1
   
   L2 <- lm(y ~ x + I(x^2))
   yhat2 <- L2$coef[1] + L2$coef[2] * chemical[k] +L2$coef[3] * chemical[k]^2
   e2[k] <- magnetic[k] - yhat2
   
   L3 <- lm(log(y) ~ x)
   logyhat3 <- L3$coef[1] + L3$coef[2] * chemical[k]
   yhat3 <- exp(logyhat3)
   e3[k] <- magnetic[k] - yhat3
   
   L4 <- lm(y ~ x + I(x^2)+ I(x^3))
   yhat4 <- L4$coef[1] + L4$coef[2] * chemical[k] +L4$coef[3] * chemical[k]^2 +L4$coef[4] * chemical[k]^3
   e4[k] <- magnetic[k] - yhat4
}
X<-matrix(c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2)),ncol=4)
rownames(X)<-"prediction error"
colnames(X)<-c("Linear","Quadratic","Exponential","Cubic")
round(X,3)

## -----------------------------------------------------------------------------
L2<-lm(magnetic ~ chemical + I(chemical^2))
L2

## -----------------------------------------------------------------------------
L1 <- lm(magnetic ~ chemical)
L2 <- lm(magnetic ~ chemical + I(chemical^2))
L3 <- lm(log(magnetic) ~ chemical)
L4 <- lm(magnetic ~ chemical + I(chemical^2)+ I(chemical^3))
r1<-summary(L1)
r2<-summary(L2)
r3<-summary(L3)
r4<-summary(L4)
X<-matrix(c(r1$adj.r.squared, r2$adj.r.squared, r3$adj.r.squared, r4$adj.r.squared),ncol=4)
rownames(X)<-"adjusted R square"
colnames(X)<-c("Linear","Quadratic","Exponential","Cubic")
round(X,3)

## -----------------------------------------------------------------------------
L2

## -----------------------------------------------------------------------------
count5test<-function(x,y){
   x<-x-mean(x)
   y<-y-mean(y)
   outx<-sum(x>max(y))+sum(x<min(y))
   outy<-sum(y>max(x))+sum(y<min(y))
   return(as.integer(max(c(outx,outy))>5))
}
permutation_test<-function(R,n){
   x<-rnorm(n1,mu1,sigma1)
   y<-rnorm(n2,mu2,sigma2)
   z<-c(x,y)
   reps<-numeric(R)
   for(i in 1:R){
      k<-sample(1:n,size=n1,replace = FALSE)
      x1<-z[k]
      reps[i]<-count5test(x1,x)
   }
   mean(reps)
}

## -----------------------------------------------------------------------------
#when sample sizes are equal
n1<-n2<-20
mu1<-mu2<-0
sigma1<-sigma2<-1
R<-999
m<-1000
set.seed(123)
alphahat1<-mean(replicate(m,permutation_test(R,n1+n2)))
round(alphahat1,4)

## -----------------------------------------------------------------------------
#when sample sizes are unequal
n1<-20
n2<-30
set.seed(1234)
alphahat2<-mean(replicate(m,permutation_test(R,n1+n2)))
n1<-20
n2<-35
set.seed(1233)
alphahat3<-mean(replicate(m,permutation_test(R,n1+n2)))
n1<-20
n2<-40
set.seed(1232)
alphahat4<-mean(replicate(m,permutation_test(R,n1+n2)))
n1<-20
n2<-50
set.seed(1231)
alphahat5<-mean(replicate(m,permutation_test(R,n1+n2)))
a<-matrix(c(alphahat2,alphahat3,alphahat4,alphahat5),ncol=4)
colnames(a)<-c("n1=20,n2=30","n1=20,n2=35","n1=20,n2=40","n1=20,n2=50")
rownames(a)<-"type I error"
round(a,4)

## ----eval=FALSE---------------------------------------------------------------
#  library(MASS)
#  library(boot)
#  library(Ball)
#  #function of distance correlation test
#  dCov <- function(x, y) {
#     x <- as.matrix(x)
#     y <- as.matrix(y)
#     n <- nrow(x)
#     m <- nrow(y)
#     if (n != m || n < 2) stop("Sample sizes must agree")
#     if (! (all(is.finite(c(x, y))))) stop("Data contains missing or infinite values")
#     Akl <- function(x) {
#       d <- as.matrix(dist(x))
#       m <- rowMeans(d)
#       M <- mean(d)
#       a <- sweep(d, 1, m)
#       b <- sweep(a, 2, m)
#       return(b + M)
#     }
#     A <- Akl(x)
#     B <- Akl(y)
#     dCov <- sqrt(mean(A * B))
#     dCov
#  }
#  ndCov2 <- function(z, ix, dims) {
#    p <- dims[1]
#    q <- dims[2]
#    d <- p + q
#    x <- z[ , 1:p] #leave x as is
#    y <- z[ix, -(1:p)] #permute rows of y
#    return(nrow(z) * dCov(x, y)^2)
#  }
#  
#  #p-values of two models
#  p_model1<-function(n){
#     p.cor<-numeric(length(n))
#     p.ball<-numeric(length(n))
#     for(i in 1:length(n)){
#       N<-n[i]
#       x1<-mvrnorm(N,rep(0,2),matrix(c(1,0,0,1),nrow=2))
#       e1<-mvrnorm(N,rep(0,2),matrix(c(1,0,0,1),nrow=2))
#       y1<-x1/4+e1
#       z1<-as.matrix(cbind(x1,y1))
#       boot.obj1<-boot(data=z1,statistic = ndCov2,R=999,sim = "permutation",dims=c(2,2))
#       tb1<-c(boot.obj1$t0,boot.obj1$t)
#       p.cor[i]<-mean(tb1>=tb1[1])
#       p.ball[i]<-bcov.test(x1,y1,R=999,seed=sample(1:100000,size = 1))$p.value
#     }
#     return(c(p.cor,p.ball))
#  }
#  
#  p_model2<-function(n){
#   p.cor<-numeric(length(n))
#   p.ball<-numeric(length(n))
#   for(i in 1:length(n)){
#     N<-n[i]
#     x2<-mvrnorm(N,rep(0,2),matrix(c(1,0,0,1),nrow=2))
#     e2<-mvrnorm(N,rep(0,2),matrix(c(1,0,0,1),nrow=2))
#     y2<-cbind(x2[,1]*e2[,1]/4,x2[,2]*e2[,2]/4)
#     z2<-as.matrix(cbind(x2,y2))
#     boot.obj2<-boot(data=z2,statistic = ndCov2,R=999,sim = "permutation",dims=c(2,2))
#     tb2<-c(boot.obj2$t0,boot.obj2$t)
#     p.cor[i]<-mean(tb2>=tb2[1])
#     p.ball[i]<-bcov.test(x2,y2,R=999,seed=sample(1:100000,size = 1))$p.value
#   }
#    return(c(p.cor,p.ball))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  n<-c(seq(25,50,5),seq(100,200,20))
#  m<-100
#  set.seed(12)
#  
#  #powers of two models
#  pow1<-apply(replicate(m,p_model1(n))<=0.05,1,mean)
#  pow2<-apply(replicate(m,p_model2(n))<=0.05,1,mean)
#  
#  #power comparison by plots
#  plot(n,pow1[1:length(n)],type = "o",ylim=c(0,1),ylab="power",main="power comparison of two methods for model 1")
#  lines(n,pow1[(length(n)+1):(2*length(n))],type="o",col="blue")
#  legend("bottomright",c("distance correlation test","ball covariance test"),col=c("black","blue"),cex=0.7,lty=1,pch=1)
#  plot(n,pow2[1:length(n)],type = "o",ylim=c(0,1),ylab="power",main="power comparison of two methods for model 2")
#  lines(n,pow2[(length(n)+1):(2*length(n))],type="o",col="blue")
#  legend("bottomright",c("distance correlation test","ball covariance test"),col=c("black","blue"),cex=0.7,lty=1,pch=1)

## ----echo=FALSE---------------------------------------------------------------
n<-c(seq(25,50,5),seq(100,200,20))
m<-100
pow1<-c(0.27,0.24,0.26,0.36,0.35,0.41,0.64,0.84,0.91,0.93,0.95,0.99,0.20,0.14,0.13,0.27,0.20,0.22,0.38,0.48,0.65,0.66,0.70,0.85)
pow2<-c(0.37,0.44,0.47,0.36,0.49,0.55,0.89,0.92,0.95,1.00,0.99,1.00,0.83,0.91,0.95,0.97,0.96,0.99,1.00,1.00,1.00,1.00,1.00,1.00)
plot(n,pow1[1:length(n)],type = "o",ylim=c(0,1),ylab="power",main="power comparison of two methods for model 1")
lines(n,pow1[(length(n)+1):(2*length(n))],type="o",col="blue")
legend("bottomright",c("distance correlation test","ball covariance test"),col=c("black","blue"),cex=0.7,lty=1,pch=1)
plot(n,pow2[1:length(n)],type = "o",ylim=c(0,1),ylab="power",main="power comparison of two methods for model 2")
lines(n,pow2[(length(n)+1):(2*length(n))],type="o",col="blue")
legend("bottomright",c("distance correlation test","ball covariance test"),col=c("black","blue"),cex=0.7,lty=1,pch=1)

## -----------------------------------------------------------------------------
rw.Metropolis<-function(sigma,x0,N){
   x<-numeric(N)
   x[1]<-x0
   u<-runif(N)
   k<-0
   for(i in 2:N){
      y<-rnorm(1,x[i-1],sigma)
      if(u[i]<=(exp(abs(x[i-1]))/exp(abs(y)))) x[i]<-y
      else{
         x[i]<-x[i-1]
         k<-k+1
      }
   }
   return(list(x=x,k=k))
}

## -----------------------------------------------------------------------------
N<-2000
sigma<-c(0.05,0.5,2,16)
x0<-20
set.seed(12)
rw1<-rw.Metropolis(sigma[1],x0,N)
rw2<-rw.Metropolis(sigma[2],x0,N)
rw3<-rw.Metropolis(sigma[3],x0,N)
rw4<-rw.Metropolis(sigma[4],x0,N)

#par(mfrow=c(2,2)) #display 6 graphs together
#the 0.05 and 0.95 quantile of laplace distribution is log(0.1) and -log(0.1)
refline <- c(log(0.1),-log(0.1))
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
for (j in 1:4) {
 plot(rw[,j], type="l",xlab=bquote(sigma == .(round(sigma[j],3))),ylab="X",ylim=range(rw[,j]))
 abline(h=refline)
}


## -----------------------------------------------------------------------------
a<-matrix(1-c(rw1$k,rw2$k,rw3$k,rw4$k)/N,ncol=4)
colnames(a)<-c("σ = 0.05","σ = 0.5","σ = 2","σ = 16")
rownames(a)<-"acceptance rate"
print(a)

## -----------------------------------------------------------------------------
#par(mfrow=c(1,2)) 
#histogram
x<-rw3$x
x<-x[x>-10&x<10]
hist(x,breaks=seq(-10,10,0.5),freq = FALSE,ylim=c(0,0.6),main="")
laplace<-function(x) 1/2*exp(-abs(x))
curve(laplace,from=-10,to=10,add=TRUE,col="red")
#Q-Q plot
a<-ppoints(100)
y<-rw3$x[500:2000]
Q<-quantile(y,a)
QL<-c(log(2*a[a<=0.5]),-log(2*(1-a[a>0.5]))) #quantiles of Laplace
qqplot(QL,Q,xlab="Laplace quantiles",ylab="Sample quantiles")
lines(c(-4,4),c(-4,4),type="l")

## -----------------------------------------------------------------------------
x<-5
c(isTRUE(log(exp(x))==exp(log(x))),isTRUE(exp(log(x))==x))
x<-10
c(isTRUE(log(exp(x))==exp(log(x))),isTRUE(exp(log(x))==x))

## -----------------------------------------------------------------------------
x<-5
c(isTRUE(all.equal(log(exp(x)),exp(log(x)))),isTRUE(all.equal(exp(log(x)),x)))
x<-10
c(isTRUE(all.equal(log(exp(x)),exp(log(x)))),isTRUE(all.equal(exp(log(x)),x)))


## -----------------------------------------------------------------------------
#exercise 11.5
set.seed(123)
ck<-function(k,a) sqrt((a^2)*k/(k+1-a^2))
f1<-function(k) exp(lgamma(k/2)-lgamma((k-1)/2))/sqrt(k-1)
f<-function(u)  (1+u^2/(k-1))^(-k/2)
f2<-function(c,k) integrate(f,lower=0,upper=c)$value
root<-function(k){
   it<-0
   r<-seq(1e-5,sqrt(k+1)/2,length=3)
   y<-numeric(3)
   y[1]<-f1(k)*f2(ck(k,r[1]),k)-f1(k+1)*f2(ck(k+1,r[1]),k+1)
   y[2]<-f1(k)*f2(ck(k,r[2]),k)-f1(k+1)*f2(ck(k+1,r[2]),k+1)
   y[3]<-f1(k)*f2(ck(k,r[3]),k)-f1(k+1)*f2(ck(k+1,r[3]),k+1)
   while(it<1000){
      it<-it+1
      if(y[1]*y[2]<0){
         r[3]<-r[2]
         y[3]<-y[2]
      } else {
         r[1]<-r[2]
         y[1]<-y[2]
      }
      r[2]<-(r[1]+r[3])/2
      y[2]<-f1(k)*f2(ck(k,r[2]),k)-f1(k+1)*f2(ck(k+1,r[2]),k+1)
   }
   return(r[2])
}
K<-c(4:25)
a1<-numeric(length(K))
for (i in 1:length(K)) {
   k<-K[i]
   a1[i]<-root(k)
}
print(round(a1,3))

## -----------------------------------------------------------------------------
#exercise 11.4
set.seed(1234)
s<-function(k,a) 1-pt(sqrt((a^2)*k/(k+1-a^2)),df=k)
root2<-function(k){
   it<-0
   r<-seq(1e-5,sqrt(k+1)/2,length=3)
   y<-numeric(3)
   y[1]<-s(k-1,r[1])-s(k,r[1])
   y[2]<-s(k-1,r[2])-s(k,r[2])
   y[3]<-s(k-1,r[3])-s(k,r[3])
   while(it<1000){
      it<-it+1
      if(y[1]*y[2]<0){
         r[3]<-r[2]
         y[3]<-y[2]
      } else {
         r[1]<-r[2]
         y[1]<-y[2]
      }
      r[2]<-(r[1]+r[3])/2
      y[2]<-s(k-1,r[2])-s(k,r[2])
   }
   return(r[2])
}
K<-c(4:25)
a2<-numeric(length(K))
for (i in 1:length(K)) {
   a2[i]<-root2(K[i])
}
print(round(a2,3))


## -----------------------------------------------------------------------------
plot(1:22,a1,main="solution comparison",xlab="k",ylab="roots",xaxt="n",type = "o",ylim=c(1,2))
lines(a2,type="o",col="red")
axis(1,at=1:22,labels = K)


## ----echo=FALSE---------------------------------------------------------------
genotype<-c("AA","BB","OO","AO","BO","AB","Sum")
frequency<-c("p^2","q^2","r^2","2pr","2qr","2pq","1")
Count<-c("nAA","nBB","nOO","nAO","nBO","nAB","n")
a<-rbind(genotype,frequency,Count)
print(a,quote = FALSE,colnames=FALSE)

## -----------------------------------------------------------------------------
#observed data loglikelihood
L<-function(nA,nB,nAB,nOO,p,q,r){
   nAB*log(2*p*q)+nA*log(p*2+2*p*r)+nB*log(q^2+2*q*r)+nOO*log(r^2)
}

em<-function(P,n.obs){
  n<-sum(n.obs)
  nA<-n.obs[1]
  nB<-n.obs[2]
  nOO<-n.obs[3]
  nAB<-n.obs[4]
  
  l<-rep(0,14)
  p<-q<-r<-rep(0,15)
  p[1]<-P[1]
  q[1]<-P[2]
  r[1]<-1-P[1]-P[2]
  for(i in 2:15){

    p.old<-p[i-1]
    q.old<-q[i-1]
    r.old<-r[i-1]
  
    nAA<-nA*p.old^2/(p.old^2+2*p.old*r.old)
    nBB<-nB*q.old^2/(q.old^2+2*q.old*r.old)
    nAO<-nA-nAA
    nBO<-nB-nBB
  
    p[i]<-(2*nAA+nAB+nAO)/(2*n)
    q[i]<-(2*nBB+nAB+nBO)/(2*n)
    r[i]<-(2*nOO+nAO+nBO)/(2*n)
    l[i-1]<-L(nA,nB,nAB,nOO,p[i],q[i],r[i])
  }
return(list(p=p,q=q,r=r,l=l))
}
n.obs=c(28,24,41,70)
P<-c(0.1,0.1)
a<-em(P,n.obs)
print(rbind(a$p,a$q,a$r))
plot(1:14,a$l,main="log-maximum likelihood values",type="o",xlab="step",ylab="l values")


## -----------------------------------------------------------------------------
formulas <- list(mpg ~ disp,mpg ~ I(1 / disp),mpg ~ disp + wt,mpg ~ I(1 / disp) + wt)

#for loop
lm1<-vector("list",length(formulas))
for(i in seq_along(formulas)){
  lm1[[i]]<-lm(formulas[[i]],data = mtcars)
}
lm1

#lapply()
lm2<-lapply(seq_along(formulas),function(i) lm(formulas[[i]],data = mtcars))
lm2

## -----------------------------------------------------------------------------
bootstraps<-lapply(1:10, function(i){
   rows <- sample(1:nrow(mtcars), rep = TRUE)
   mtcars[rows, ]
})

#for loop
l1<-vector("list",length(bootstraps))
for(i in seq_along(bootstraps)){
  l1[[i]]<-lm(mpg~disp,data = bootstraps[[i]])
}
l1

#lapply() 
#do it without an anonymous function
l2<-lapply(bootstraps,lm,formula=mpg~disp)
l2

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared

#model in exercise 3
unlist(lapply(lm1, rsq))
unlist(lapply(lm2, rsq))
#model in exercise 4
unlist(lapply(l1, rsq))
unlist(lapply(l2, rsq))

## -----------------------------------------------------------------------------
trials <- replicate(100,t.test(rpois(10,10),rpois(7,10)),simplify = FALSE)
#extract the p-value from every trial
sapply(trials,function(x) x$p.value)

#Extra challenge: get rid of the anonymous function by using [[ directly.
sapply(trials,'[[',3)

## ----eval=FALSE---------------------------------------------------------------
#  #Implement mcsapply()
#  library(parallel)
#  num_cores = detectCores()
#  cluster = makePSOCKcluster(num_cores)
#  mcsapply = function(cluster,X,FUN,...){
#   res = parLapply(cluster,X,FUN,...)
#   simplify2array(res)
#  }
#  stopCluster(cluster)

## -----------------------------------------------------------------------------
## R function for Exercise 9.4
rw.Metropolis<-function(sigma,x0,N){
   x<-numeric(N)
   x[1]<-x0
   u<-runif(N)
   for(i in 2:N){
      y<-rnorm(1,x[i-1],sigma)
      if(u[i]<=(exp(abs(x[i-1]))/exp(abs(y)))) x[i]<-y
      else{
         x[i]<-x[i-1]
      }
   }
   return(x)
}

## Rcpp function for Exercise 9.4
library(Rcpp)
cppFunction('NumericVector rwC(double sigma, int x0, int N) {
      NumericVector out(N);
      out[0] = x0;
      
      for(int i=1;i<N;i++){
        double u = (double)rand() / RAND_MAX;
        
        double V1, V2, S;
        int phase = 0;
        double X;
     
        if ( phase == 0 ) {
         do {
            double U1 = (double)rand() / RAND_MAX;
            double U2 = (double)rand() / RAND_MAX;
             
            V1 = 2 * U1 - 1;
            V2 = 2 * U2 - 1;
            S = V1 * V1 + V2 * V2;
         } while(S >= 1 || S == 0);
         
         X = V1 * sqrt(-2 * log(S) / S);
         } else
         X = V2 * sqrt(-2 * log(S) / S);
         
        phase = 1 - phase;
      
        double y = sigma*X+out[i-1];
        
        if(u <= (exp(abs(out[i-1]))/exp(abs(y))))
           out[i] = y;
        else
           out[i] = out[i-1];
      }
      return out;
    }')

## initial values
N<-2000
sigma<-2
x0<-20

set.seed(123)
y1<-rwC(sigma,x0,N)
y2<-rw.Metropolis(sigma,x0,N)

## Compare the generated random numbers by the two functions using qqplot.
a<-ppoints(100)
Q1<-quantile(y1[500:2000],a)
Q2<-quantile(y2[500:2000],a)
qqplot(Q1,Q2,xlab="Rcpp function quantiles",ylab="R function quantiles")

## Compare the computation time of the two functions with microbenchmark.
library(microbenchmark)
ts <- microbenchmark(rwR=rw.Metropolis(sigma,x0,N),rwRcpp=rwC(sigma,x0,N))
summary(ts)[,c(1,3,5,6)]


