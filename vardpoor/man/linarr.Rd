\name{linarr}

\alias{linarr}

\title{Linearization of the aggregate replacement ratio}

\description{
Estimates the aggregate replacement ratio (defined as the gross median individual pension income of the population aged 65-74 relative to the gross median individual earnings from work of the population aged 50-59, excluding other social benefits) and computes linearized variable for variance estimation.
}


\usage{
linarr(Y, Y_den, id = NULL, age,
  pl085, month_at_work,
  weight = NULL,
  sort = NULL,
  Dom = NULL,
  period = NULL,
  dataset = NULL,
  order_quant = 50,
  var_name = "lin_arr")
}


\arguments{
\item{Y}{Numerator variable (for gross pension income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{Y_den}{Denominator variable (for example gross individual earnings). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{id}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{weight}{Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{age}{Age variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{pl085}{Retirement variable (Number of months spent in retirement or early retirement). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{month_at_work}{Variable for total number of month at work (sum of the number of months spent at full-time work as employee, number of months spent at part-time work as employee, number of months spent at full-time work as self-employed (including family worker), number of months spent at part-time work as self-employed (including family worker)).  One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{sort}{Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{Dom}{Optional variables used to define population domains. If supplied, linearization of at-risk-of-poverty threshold is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers as numeric vector or logical vector (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{period}{Optional variable for survey period. If supplied, linearization of at-risk-of-poverty threshold is done for each survey period. Object convertible to \code{data.table} or variable names as character, column numbers as numeric vector or logical vector (length of the vector has to be the same as the column count of \code{dataset}).
}

\item{dataset}{Optional survey data object convertible to \code{data.table}.
}
 
\item{order_quant}{A numeric value in range \eqn{\left[ 0,100 \right]}{[0,100]} for \eqn{\alpha} in the formula for at-risk-of-poverty threshold computation:
\deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
For example, to compute at-risk-of-poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.
}

\item{var_name}{A character specifying the name of the linearized variable.
}

}


\details{The implementation strictly follows the Eurostat definition.}


\value{
A list with four objects are returned:
    
\item{value}{A \code{data.table} containing the estimated the aggregate replacement ratio.
}
  
\item{lin}{A \code{data.table} containing the linearized variables of the aggregate replacement ratio.
}

}


\references{
Working group on Statistics on Income and Living Conditions (2015) Task 5 - Improvement and optimization of calculation of net change. \emph{LC- 139/15/EN}, Eurostat. \cr
  
Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}


\seealso{
\code{\link{varpoord}} , \code{\link{vardcrospoor}}, \code{\link{vardchangespoor}}}

\examples{
data(eusilc)

dati <- data.table(IDd = 1:nrow(eusilc), eusilc)
dati$pl085 <- 12*trunc(runif(nrow(dati),0,2))
dati$month_at_work <- 12*trunc(runif(nrow(dati),0,2))

# Full population
d <- linarr(Y="eqIncome", Y_den="eqIncome", id="IDd",  
                 age="age", pl085="pl085", month_at_work="month_at_work",
                 weight="rb050",  Dom=NULL, dataset=dati, order_quant=50)
d$value

\dontrun{
# By domains
dd <- linarr(Y="eqIncome", Y_den="eqIncome", id="IDd",  
                 age="age", pl085="pl085", month_at_work="month_at_work",
                 weight="rb050",  Dom="db040", dataset=dati, order_quant=50)
dd
} 
}


\keyword{Linearization}
