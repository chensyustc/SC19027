% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datageneration.R
\name{datageneration}
\alias{datageneration}
\title{A Data Generation Function for Linear Regression}
\usage{
datageneration(n = 200, p = 2000, r0 = 0.5)
}
\arguments{
\item{n}{the number of samples}

\item{p}{the number of variables}

\item{r0}{the correlation of first 50 columns}
}
\value{
X design matrix

Y response

true_beta coefficients of the model
}
\description{
This function generates a dataset of linear model.The dataset includes design matrix X,response Y and coefficient beta.X has independent and identically distributed Gaussian rows with marginal distribution N(0,1),and correlated first 50 columns with r0.The first three coefficients are nonzero,1/sqrt(3). The model has a normal noise.
}
\examples{
\dontrun{
data<-datageneration(200,2000,0)
X<-data$X
Y<-data$Y
beta<-data$true_beta
}
}
