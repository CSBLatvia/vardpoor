\name{vardpoor-package}
\alias{vardpoor-package}
\alias{vardpoor}
\docType{package}
\title{
Variance estimation for sample surveys by the ultimate cluster method
}
\description{
Generation of domain variables,
  linearization of several nonlinear population statistics
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
  variance estimation of sample surveys by the ultimate cluster method
    (Hansen, Hurwitz and Madow, 1953),
  variance estimation for longitudinal, cross-sectional measures and measures of change for single and multistage stage cluster sampling designs (Berger, Y.G.).
  Several other precision measures are derived - standard error,
    the coefficient of variation,
    the margin of error,
    confidence interval,
    design effect.
}
\details{
\tabular{ll}{
Package: \tab vardpoor\cr
Version: \tab 0.2.0.12\cr
Date: \tab 2014-08-26\cr
Depends: \tab R (>= 3.0.0), data.table (>= 1.9.2), laeken\cr
Imports: \tab reshape2, foreach, lpSolve, plyr, gdata, MASS\cr
License: \tab GPL (>= 2)\cr
URL: \tab https://github.com/djhurio/vardpoor/ \cr
BugReports: \tab https://github.com/djhurio/vardpoor/issues/ \cr
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
lingpg                  Linearization of the gender pay (wage) gap
linpoormed              Linearization of the median income below the
                        at-risk-of-poverty gap
linqsr                  Linearization of the income quintile share
                        ratio
linrmpg                 Linearization of the relative median
                        at-risk-of-poverty gap
residual_est            Residual estimation of calibration
var_srs                 The estimation of the simple random sampling.
vardchanges             Variance estimation for measures of change for
                        single and multistage stage cluster sampling
                        designs
vardcros                Variance estimation for cross-sectional and
                        longitudinal measures for single and multistage
                        stage cluster sampling designs
vardom                  Estimation of the variance in domain by the ultimate
                        cluster method
vardom_othstr           Variance estimation for sample surveys in domain 
                        using two stratification
vardomh                 Variance Estimation in domain for household
                        surveys by the ultimate cluster method
vardpoor-package        Variance estimation for sample surveys by the 
                        ultimate cluster method ~~ package title ~~
variance_est            Variance estimation for sample surveys by the ultimate
                        cluster method
variance_othstr         Variance estimation for sample surveys by the new
                        stratification
varpoord                Estimation of the variance and deff for sample 
                        surveys for indicators on social exclusion and poverty
}
}
\author{
Juris Breidaks [aut, cre],
  Martins Liberts [aut]

Maintainer: Juris Breidaks <Juris.Breidaks@csb.gov.lv>
}
\keyword{ package }
