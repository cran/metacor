\encoding{latin1}
\name{metacor.OP}
\alias{metacor.OP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Olkin-Pratt (OP) meta-analytical approach with correlation coefficients as effect sizes}
\description{
Implements the Olkin-Pratt (DSL) fixed-effect meta-analytical approach with correlation coefficients as effect sizes, as described by Schulze (2004).
}
\usage{
metacor.OP(r, n, labels, alpha = 0.05, plot = TRUE, xlim = c(-1, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{vector of correlations}
  \item{n}{vector of sample sizes}
  \item{labels}{vector of the study names}
  \item{alpha}{alpha-level for the main test and for the confidence intervals}
  \item{plot}{logical; should a forest plot be returned?}
  \item{xlim}{range of the x-axis of the forest plot}
}
\value{
  \item{G}{vector of the G-values}
  \item{G.var}{vector of the variances of each G}
  \item{G.lower}{the lower limits of the confidence intervals for G}
  \item{G.upper}{the upper limits of the confidence intervals for G}
  \item{G.mean}{the mean effect size G}
  \item{G.se}{the standard error of G.mean}
  \item{G.mean.lower}{the lower limit of the confidence interval for G.mean}
  \item{G.mean.upper}{the upper limit of the confidence interval for G.mean}
  \item{p}{the p-value for the null hypothesis H0 -> G.mean = 0}
}
\references{Schulze, R. (2004) Meta-analysis: a comparison of approaches. Hogrefe & Huber, Gottingen, Germany.}
\author{Etienne Laliberté \email{etiennelaliberte@gmail.com} \url{http://www.elaliberte.info/}}
\seealso{\code{\link{metacor.DSL}}}
\examples{
data(lui)
lui <- lui[order(lui$r.FDis),]
test <- metacor.OP(lui$r.FDis, lui$n, lui$label)
test
}
\keyword{Statistical Models
}
