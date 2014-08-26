\name{domain}

\alias{domain}

\title{Extra variables for domain estimation}

\description{
  The function computes extra variables for domain estimation. Each unique \code{D} row defines a domain. Extra variables are computed for each \code{Y} variable.
}

\usage{
  domain(Y, D)
}

\arguments{
  \item{Y}{Matrix of study variables. Any object convertable to \code{data.table} with numeric values, \code{NA} values are not allowed.}
  \item{D}{Matrix of domain variables. Any object convertable to \code{data.table}. The number of rows of \code{D} must match the number of rows of \code{Y}. Duplicated names are not allowed.}
}

\value{Numeric \code{data.table} containing extra variables for domain estimation.}

\references{
Carl-Erik Sarndal, Bengt Swensson, Jan Wretman. Model Assisted Survey Sampling. Springer-Verlag, 1992, p.70.
}

\seealso{\code{\link{vardom}}, \code{\link{vardomh}}}

\examples{

### Example 0

domain(1, "A")


### Example 1

Y <- as.matrix(1:10)
colnames(Y) <- "Y1"
D <- as.matrix(rep(1, 10))
colnames(D) <- "D1"
domain(Y, D)


### Example 2

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
D <- matrix(rep(1:2, each = 5), 10, 1)
colnames(D) <- "D"
domain(Y, D)


### Example 3

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
D <- matrix(rep(1:4, each = 5), 10, 2)
colnames(D) <- paste("D", 1:2, sep="")
domain(Y, D)


### Example 4

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
D <- matrix(c(rep(1:2, each = 5), rep(3, 10)), 10, 2)
colnames(D) <- paste("D", 1:2, sep="")
domain(Y, D)
}

\keyword{survey}
