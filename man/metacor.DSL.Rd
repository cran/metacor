\name{metacor.DSL}
\alias{metacor.DSL}
\title{DerSimonian-Laird (DSL) meta-analytical approach with correlation coefficients as effect sizes}
\description{
Implements the DerSimonian-Laird (DSL) random-effect meta-analytical approach with correlation coefficients as effect sizes, as described by Schulze (2004).
}
\usage{
metacor.DSL(r, n, labels, alpha = 0.05, plot = TRUE, xlim = c(-1, 1), transform = TRUE)
}
\arguments{
  \item{r}{vector of correlations}
  \item{n}{vector of sample sizes}
  \item{labels}{vector of the study names}
  \item{alpha}{alpha-level for the main test and for the confidence intervals}
  \item{plot}{logical; should a forest plot be returned?}
  \item{xlim}{range of the x-axis of the forest plot}
  \item{transform}{logical; should the z-values be back-transformed to r-space?}
}
\value{
  \item{z}{vector of the z-values}
  \item{z.var}{vector of the variances of each z}
  \item{z.lower}{the lower limits of the confidence intervals for each z}
  \item{z.upper}{the upper limits of the confidence intervals for each z}
  \item{z.mean}{the mean effect size z}
  \item{r.mean}{the mean effect size r, back-transformed from z-space}
  \item{z.se}{the standard error of z.mean}
  \item{z.mean.lower}{the lower limit of the confidence interval for z.mean}
  \item{r.mean.lower}{the lower limit of the confidence interval for r.mean, back-transformed from z-space}
  \item{z.mean.upper}{the upper limit of the confidence interval for z.mean}
  \item{r.mean.upper}{the upper limit of the confidence interval for r.mean, back-transformed from z-space}
  \item{p}{the p-value for the null hypothesis H0 -> z.mean = 0}
}
\references{Schulze, R. (2004) Meta-analysis: a comparison of approaches. Hogrefe & Huber, Gottingen, Germany.}
\author{Etienne Laliberte \email{etiennelaliberte@gmail.com} \url{http://www.elaliberte.info/}}
\seealso{\code{\link{metacor.OP}}}
\examples{
data(lui)
lui <- lui[order(lui$r.FDis),]
test <- metacor.DSL(lui$r.FDis, lui$n, lui$label)
test
}
\keyword{Statistical Models
}

