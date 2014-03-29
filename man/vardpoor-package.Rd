\name{vardpoor-package}
\alias{vardpoor-package}
\alias{vardpoor}
\docType{package}
\title{
Variance Estimation by the Ultimate Cluster Method
}
\description{
Generation of domain variables,
  linearisation of several nonlinera population statistics
  (the ratio of two totals,
    weighted income percentile,
    at-risk-of-poverty rate,
    at-risk-of-poverty threshold,
    Gini coefficient,
    gender pay gap,
    median income below at-risk-of-poverty gap,
    income quintile share ratio,
    relative median at-risk-of-poverty gap),
  computation of regression residuals in case of weight calibration,
  variance estimation by the ultimate cluster method
    (Hansen, Hurwitz and Madow, 1953).
  Several other precision measures are derived - standard error,
    the coefficient of variation,
    the margin of error,
    confidence interval,
    design effect.
}
\details{
\tabular{ll}{
Package: \tab vardpoor\cr
Version: \tab 0.2.0.7\cr
Date: \tab 2014-03-29\cr
Depends: \tab R (>= 3.0.0), boot, MASS, foreach, laeken, lpSolve, data.table\cr
License: \tab GPL (>= 2)\cr
URL: \tab https://github.com/djhurio/vardpoor\cr
BugReports: \tab https://github.com/djhurio/vardpoor/issues\cr
Classification/MSC: \tab 62D05\cr
Built: \tab R 3.0.3; 2014-03-29 15:56:14 UTC; unix\cr
}

Index:
\preformatted{
domain                  Split data variables by domains
incPercentile           Weighted income percentile
lin.ratio               Linearization of the ratio estimator
linarpr                 Linearization of the at-risk-of-poverty rate
linarpt                 Linearization of the at-risk-of-poverty
                        threshold
lingini                 Linearization of the GINI coefficient I
lingini2                Linearization of the GINI coefficient II
lingpg                  Linearization of the gender pay (wage) gap.
linpoormed              Linearization of the median income below the
                        at-risk-of-poverty gap
linqsr                  Linearization of the income quintile share
                        ratio
linrmpg                 Linearization of the relative median
                        at-risk-of-poverty gap
residual_est            Residual estimation of calibration
var_srs                 The estimation of the simple random sampling.
vardom                  Estimation of the variance in domain
vardom_othstr           Estimation of the variance in domain using two
                        stratification
vardomh                 Estimation of the variance in domain for
                        household surveys
vardpoor-package        Variance Estimation by the Ultimate Cluster
                        Method ~~ package title ~~
variance_est            Estimation of the variance using ultimate
                        cluster method
variance_othstr         Estimation of the variance using the new
                        stratification
varpoord                Estimation of the variance and deff for
                        indicators on social exclusion and poverty
}
}
\author{
Juris Breidaks [aut, cre],
  Martins Liberts [aut]

Maintainer: Juris Breidaks <juris.breidaks@csb.gov.lv>
}
\keyword{ package }
