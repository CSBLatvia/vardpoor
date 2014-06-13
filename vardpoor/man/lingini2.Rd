\name{lingini2}

\alias{lingini2}

\title{Linearization of the GINI coefficient II}

\description{
  Estimate the Gini coefficient, which is a measure for
  inequality, and its linerization.
}

\usage{
lingini2(inc, id = NULL, weight = NULL,
         sort = NULL, Dom = NULL,
         period=NULL, dataset = NULL,
         na.rm = FALSE, var_name="lin_gini2")
}

\arguments{

  \item{inc}{Study variable (for example equivalized disposable income). One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{id}{Optional variable for unit ID codes. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{weight}{Optional weight variable. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{sort}{Optional variable to be used as tie-breaker for sorting. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{Dom}{Optional variables used to define population domains. If supplied, linearization of the at-risk-of-poverty rate is done for each domain. An object convertable to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{period}{Optional variable for survey period. If supplied, linearization of the at-risk-of-poverty rate is done for each time period. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{dataset}{Optional survey data object convertable to \code{data.frame}.}

  \item{na.rm}{A logical value indicating whether missing values in study variable should be removed.}

  \item{var_name}{A character specifying the name of the linearized variable.}
}

\value{
 A list with two objects are returned by the function:

  \item{value}{A \code{data.frame} containing the estimated Gini coefficients (in percentage) by Y. Tille and Eurostat papers. The estimates are done by domains if \code{Dom} is not \code{NULL}.}
  
  \item{lin}{A \code{data.frame} containing the linearized variables of the Gini coefficients (in percentage) by Y. Tille paper. The linearized variables are computed by domains if \code{Dom} is not \code{NULL}.}

}

\references{
  Working group on Statistics on Income and Living
  Conditions (2004) Common cross-sectional EU indicators
  based on EU-SILC; the gender pay gap.  \emph{EU-SILC
  131-rev/04}, Eurostat.\cr
  
  MATTI LANGEL - YVES TILLE, Corrado Gini, a pioneer in balanced sampling
  and inequality theory. \emph{METRON - International Journal of Statistics},
  2011, vol. LXIX, n. 1, pp. 45-65, 
  URL \url{ftp://metron.sta.uniroma1.it/RePEc/articoli/2011-1-3.pdf}. \cr

  Jean-Claude Deville (1999). Variance estimation for complex statistics
  and estimators: linearization and residual techniques. Survey
  Methodology, 25, 193-203,
  URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}

\examples{
data(eusilc)
dati <- data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"
dat1 <- lingini2("eqIncome","IDd",weight = "rb050",Dom=c("db040"),dataset=dati)
dat2 <- lingini2("eqIncome","IDd",weight = "rb050",dataset=dati)

dat1$value
dat2$value
}


\seealso{
\code{\link{lingini}}, \code{\link{varpoord}}, \code{\link{linqsr}}}
\keyword{Linearization}
