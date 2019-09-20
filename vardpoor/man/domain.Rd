\name{domain}

\alias{domain}

\title{Extra variables for domain estimation}

\description{
  The function computes extra variables for domain estimation. Each unique \code{D} row defines a domain. Extra variables are computed for each \code{Y} variable.
}

\usage{
  domain(Y, D, dataset = NULL, checking = TRUE)
}

\arguments{
  \item{Y}{Matrix of study variables. Any object convertable to \code{data.table} with numeric values, \code{NA} values are not allowed. Object convertible to \code{data.table} or variable names as character, column numbers.}
  \item{D}{Matrix of domain variables. Any object convertable to \code{data.table}. The number of rows of \code{D} must match the number of rows of \code{Y}. Duplicated names are not allowed. Object convertible to \code{data.table} or variable names as character, column numbers.}
  \item{dataset}{Optional survey data object convertible to \code{data.table}.}
  \item{checking}{Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.}
}

\value{Numeric \code{data.table} containing extra variables for domain estimation.}

\references{
Carl-Erik Sarndal, Bengt Swensson, Jan Wretman. Model Assisted Survey Sampling. Springer-Verlag, 1992, p.70.
}

\seealso{\code{\link{vardom}}, \code{\link{vardomh}}}

\examples{

### Example 0

domain(Y = 1, D = "A")


### Example 1

Y1 <- as.matrix(1 : 10)
colnames(Y1) <- "Y1"
D1 <- as.matrix(rep(1, 10))
colnames(D1) <- "D1"
domain(Y = Y1, D = D1)


### Example 2

Y <- matrix(1 : 20, 10, 2)
colnames(Y) <- paste0("Y", 1 : 2)
D <- matrix(rep(1 : 2, each = 5), 10, 1)
colnames(D) <- "D"
domain(Y, D)


### Example 3

Y <- matrix(1 : 20, 10, 2)
colnames(Y) <- paste0("Y", 1 : 2)
D <- matrix(rep(1 : 4, each = 5), 10, 2)
colnames(D) <- paste0("D", 1 : 2)
domain(Y, D)


### Example 4

Y <- matrix(1 : 20, 10, 2)
colnames(Y) <- paste0("Y", 1 : 2)
D <- matrix(c(rep(1 : 2, each = 5), rep(3, 10)), 10, 2)
colnames(D) <- paste0("D", 1 : 2)
domain(Y, D)
}

\keyword{survey}
