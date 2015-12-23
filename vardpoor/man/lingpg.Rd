\name{lingpg}

\alias{lingpg}

\title{Linearization of the gender pay (wage) gap.}


\description{
Estimation of gender pay (wage) gap and computation of linearized variables for variance estimation.
}


\usage{
lingpg(Y, gender = NULL,
  id = NULL, 
  weight = NULL,
  sort = NULL,
  Dom = NULL,
  period = NULL,
  dataset = NULL,
  var_name = "lin_gpg")
}


\arguments{

\item{Y}{Study variable (for example the gross hourly earning). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
}

\item{gender}{Numerical variable for gender, where 1 is for males, but 2 is for females. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
}


\item{id}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
}

\item{weight}{Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
}

\item{sort}{Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
}

\item{Dom}{Optional variables used to define population domains. If supplied, estimation and linearization of gender pay (wage) gap is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
}

\item{period}{Optional variable for survey period. If supplied, estimation and linearization of gender pay (wage) gap is done for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
}

\item{dataset}{Optional survey data object convertible to \code{data.table}.
}

\item{var_name}{A character specifying the name of the linearized variable.
}

}


\value{A list with two objects are returned:
  
\item{value}{\code{data.table} containing the estimated gender pay (wage) gap (in percentage).
}
  
\item{lin}{\code{data.table} containing the linearized variables of the gender pay (wage) gap (in percentage) for variance estimation.
}

}


\references{
Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap. \emph{EU-SILC 131-rev/04}, Eurostat. \cr
  
Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr

Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}


\seealso{
\code{\link{linqsr}}, \code{\link{lingini}}, \code{\link{varpoord}} , \code{\link{vardcrospoor}}, \code{\link{vardchangespoor}}}


\examples{
data(ses)

dati <- data.table(ID = 1:nrow(ses), ses)
setnames(dati, "sex", "sexf")
dati[sexf=="male", sex:=1]
dati[sexf=="female", sex:=2]


# Full population
gpgs1 <- lingpg(Y="earningsHour", gender="sex",
                id="ID", weight="weights", dataset=dati)
gpgs1$value

\dontrun{
# Domains by education
gpgs2 <- lingpg(Y="earningsHour", gender="sex",
                id="ID", weight="weights",
                Dom="education", dataset=dati)
gpgs2$value

# Sort variable
gpgs3 <- lingpg(Y="earningsHour", gender="sex",
                id="ID", weight="weights", sort = "ID",
                Dom="education", dataset=dati)
gpgs3$value

# Two survey periods
dati[, year := 2010]
dati2 <- copy(dati)
dati2[, year := 2011]
dati <- rbind(dati, dati2)
gpgs4 <- lingpg(Y="earningsHour", gender="sex",
                id="ID", weight="weights", sort = "ID",
                Dom="education", period = "year",
                dataset=dati)
gpgs4$value
names(gpgs4$lin)
}
}

\keyword{Linearization}
