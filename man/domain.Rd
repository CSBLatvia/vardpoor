\name{domain}
\alias{domain}
\title{Split data variables by domains}
\usage{
  domain(Y, D, texts)
}
\arguments{
  \item{Y}{data.frame, matrix, data.table with column names; its number of rows is n,
   the sample size.}
  \item{D}{data.frame, matrix, data.table with column names}
  \item{texts}{a character string, name of column of \code{X}.}
}
\value{A numeric data.table containing data Y divided by domain variable D}
\description{
  Data Y divided by domain variable D.
}
\references{
  Sixten Lundstrom and Carl-Erik Sarndal. Estimation in the presence of
  Nonresponse and Frame Imperfections. Statistics Sweden, 2001, p. 43-44.
}
\examples{

### Example 1

Y <- as.matrix(1:10)
colnames(Y) <- "Y1"
Y
D <- as.matrix(rep(1, 10))
colnames(D) <- "D1"
D
domain(Y, D, "Y")


### Example 2

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
Y
D <- matrix(rep(1:2, each = 5), 10, 1)
colnames(D) <- "D"
D
domain(Y, D, "Y")


### Example 3

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
Y
D <- matrix(rep(1:4, each = 5), 10, 2)
colnames(D) <- paste("D", 1:2, sep="")
D
domain(Y, D, "Y")


### Example 4

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
Y
D <- matrix(c(rep(1:2, each = 5), rep(3, 10)), 10, 2)
colnames(D) <- paste("D", 1:2, sep="")
D
domain(Y, D, "Y")
}

\seealso{
  \code{\link{vardom}}, \code{\link{vardomh}}, \code{\link{lin.ratio}},  \code{\link{residual_est}},
  \code{\link{var_srs}},  \code{\link{variance_est}}, \code{\link{variance_othstr}}
}
\keyword{survey}
