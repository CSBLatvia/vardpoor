\name{variance_est}
\alias{variance_est}
\title{Variance estimation for sample surveys by the ultimate cluster method}
\description{Computes the variance estimation by the ultimate cluster method.}
\usage{variance_est(Y, H, PSU, w_final, N_h=NULL, fh_zero=FALSE,
                    PSU_level=TRUE, period=NULL, dataset=NULL)}
\arguments{
  \item{Y}{either a numeric data.frame, matrix, data.table with
  column names giving the variables of interest, or (if \code{dataset} 
  is not \code{NULL}) a character string, an integer or a
  logical vector (length is the same as 'dataset' column count)
  specifying the corresponding column of \code{dataset}.}

  \item{H}{either 1 column data.frame, matrix, data.table with
  column name giving elements  indicating the unit stratum,
  or (if \code{dataset} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as 'dataset'
  column count) specifying the corresponding column of \code{dataset}.}

  \item{PSU}{either 1 column data.frame, matrix, data.table giving
  primary sampling unit, or (if \code{dataset} is not \code{NULL})
  a character string, an integer or a logical vector (length is the
  same as 'dataset' column count) specifying the corresponding
  column of \code{dataset}.}

  \item{w_final}{either a numeric vector, 1 column data.frame, matrix,
  data.table giving the final weights, or (if \code{dataset} is not
  \code{NULL}) a character string,  an integer or a logical vector
  (length is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}.}

  \item{N_h}{either a matrix giving the first column - stratum, but
  the second column - the total of the population in each stratum.}

   \item{fh_zero}{by default FALSE; fh is calculated as division of n_h and N_h 
  in each strata, if true, fh value is zero in each strata.}

  \item{PSU_level}{by default TRUE; if PSU_level is true, in each strata fh
  is calculated as division of count of PSU in sample (n_h) and count of PSU in frame(N_h). 
  if PSU_level is false, in each strata fh is calculated as division of count of units in
  sample (n_h) and count of units in frame(N_h), which calculated as sum of weights. }

  \item{period}{optional; either a data.frame, matrix, data.table
  with column names giving different periods, or (if 
  \code{dataset} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as 
  'dataset' column count) specifying the corresponding
  columns of \code{dataset}. If supplied, the values for
  each period are computed independently.
  }

  \item{dataset}{an optional name of the individual dataset
  \code{data.frame}.}

}
\value{a \code{data.table} containing the  values of the
  variance estimation by totals.
}
\details{
If we assume that \eqn{n_h \geq 2} for all \eqn{h}, that is, two or more PSUs
 are selected from each stratum, then the variance of \eqn{\hat{\theta}} 
can be estimated from the variation among the estimated PSU totals of the variable \eqn{Z}:

\deqn{\hat{V} \left(\hat{\theta} \right)=\sum\limits_{h=1}^{H} \left(1-f_h \right) \frac{n_h}{n_{h}-1} \sum\limits_{i=1}^{n_h} \left( z_{hi\bullet}-\bar{z}_{h\bullet\bullet}\right)^2, }

where

\eqn{\bullet}  
\eqn{z_{hi\bullet}=\sum\limits_{j=1}^{m_{hi}} \omega_{hij} z_{hij}}

\eqn{\bullet}  
\eqn{\bar{z}_{h\bullet\bullet}=\frac{\left( \sum\limits_{i=1}^{n_h} z_{hi\bullet} \right)}{n_h}}

\eqn{\bullet}  
\eqn{f_h} is the sampling fraction of PSUs within stratum

\eqn{\bullet}  
\eqn{h} is the stratum number, with a total of H strata

\eqn{\bullet}  
\eqn{i} is the primary sampling unit (PSU) number within stratum \eqn{h}, with a total of \eqn{n_h} PSUs

\eqn{\bullet}  
\eqn{j} is the household number within cluster \eqn{i} of stratum \eqn{h}, with a total of \eqn{m_{hi}} household

\eqn{\bullet}  
\eqn{w_{hij}} is the sampling weight for household \eqn{j} in PSU \eqn{i} of stratum \eqn{h}

\eqn{\bullet}  
\eqn{z_{hij}} denotes the observed value of the analysis variable \eqn{z} for household \eqn{j} in PSU \eqn{i} of stratum \eqn{h}

}

\references{
Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard
error estimation and other related sampling issues in EU-SILC,
URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013}    \cr

Guillaume Osier and Emilio Di Meglio. The linearisation approach implemented by Eurostat for
the first wave of EU-SILC: what could be done from the second onwards? 2012
}

\examples{
Ys <- matrix(rchisq(10, 3), 10, 1)
w <- rep(2, 10)
PSU <- 1:nrow(Ys)
H <- w
variance_est(Y=Ys, H=H, PSU=PSU, w_final=w)

} 
\seealso{
   \code{\link{domain}},   \code{\link{lin.ratio}},    \code{\link{linarpr}},
   \code{\link{linarpt}},  \code{\link{lingini}},      \code{\link{lingini2}},
   \code{\link{lingpg}},   \code{\link{linpoormed}},   \code{\link{linqsr}},
   \code{\link{linrmpg}},  \code{\link{residual_est}}, \code{\link{vardom}},
    \code{\link{vardomh}}, \code{\link{varpoord}},     \code{\link{variance_othstr}}
} 
\keyword{vardpoor}
