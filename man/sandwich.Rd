\name{sandwich}
\alias{sandwich}
\alias{lm_sando}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sandwich Estimator Standard Errors for \code{lm}}
\description{
  Output sandwich estimator standard errors for an ols regression model.
}
\usage{
sandwich( x, cluster=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The \code{lm} model object or the output from sandwich (for reprinting).}
  \item{cluster}{A numeric vector or factor to represent group clustering for the data.}
}
\details{
    This uses sandwich estimator to compute the standard errros and reruns the t-tests. See McNeish et al. (2017) for more details.
}
\references{}
\author{Brian Keller}
\seealso{lm}
\examples{
mydata <- na.omit(dropout[,c('school','math7','math8','math10')])
mdl.lm <- lm(math10 ~ math7 + math8, mydata)
sandwich(mdl.lm, mydata$school)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
