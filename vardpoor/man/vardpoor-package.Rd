\name{vardpoor-package}
\alias{vardpoor-package}
\alias{vardpoor}
\docType{package}
\title{
Variance Estimation for Sample Surveys by the Ultimate Cluster Method
}
\description{
Generation of domain variables,
  linearization of several nonlinear population statistics
  (the ratio of two totals,
    weighted income percentile,
    at risk of poverty rate,
    at risk of poverty threshold,
    Gini coefficient,
    gender pay gap,
    the aggregate replacement ratio,
    the relative median income ratio, 
    median income of individuals below at risk of poverty gap,
    income quintile share ratio,
    relative median at risk of poverty gap),
  computation of regression residuals in case of weight calibration, 
  variance estimation of sample surveys by the ultimate cluster method
    (Hansen, Hurwitz and Madow, Theory, vol. I: Methods and Applications; vol. II: Theory.  1953,  New York: John Wiley and Sons.),
  variance estimation for longitudinal, cross - sectional measures and measures of change for single and multistage stage cluster sampling designs (Berger, Y. G. ,2015, <doi:10.1111/rssa.12116>).
  Several other precision measures are derived - standard error,
    the coefficient of variation,
    the margin of error,
    confidence interval,
    design effect.
}
\details{
\tabular{ll}{
Package: \tab vardpoor\cr
Version: \tab 0.9.0\cr
Date: \tab 2017-03-17\cr
Depends: \tab R (>= 3.2.3), data.table (>= 1.10.0), laeken, stringr\cr
Imports: \tab reshape, foreach, plyr, gdata, MASS, stats, utils, surveyplanning\cr
License: \tab GPL (>= 2)\cr
URL: \tab https://github.com/CSBLatvia/vardpoor/ \cr
BugReports: \tab https://github.com/CSBLatvia/vardpoor/issues/ \cr
}

Index:
\preformatted{
domain                  Split data variables by domains
incPercentile           Weighted income percentile
lin.ratio               Linearization of the ratio estimator
linarr                  Linearization of the aggregate replacement ratio
linrmi                  Linearization of the relative median income ratio
linarpr                 Linearization of the at-risk-of-poverty rate
linarpt                 Linearization of the at-risk-of-poverty
                        threshold
lingini                 Linearization of the GINI coefficient I
lingini2                Linearization of the GINI coefficient II
lingpg                  Linearization of the gender pay (wage) gap
linpoormed              Linearization of the median income below the
                        at-risk-of-poverty gap
linqsr                  Linearization of the income quintile share ratio
linrmpg                 Linearization of the relative median
                        at-risk-of-poverty gap
residual_est            Residual estimation of calibration
var_srs                 The estimation of the simple random sampling
vardchanges             Variance estimation for measures of change for
                        single and multistage stage cluster sampling designs
vardchangespoor         Variance estimation for measures of change for sample
                        surveys for indicators on social exclusion and poverty
vardcros                Variance estimation for cross-sectional and
                        longitudinal measures for single and multistage
                        stage cluster sampling designs
vardcrospoor            Variance estimation for cross-sectional,
                        longitudinal measures for indicators on
                        social exclusion and poverty
vardom                  Estimation of the variance in domain by the ultimate
                        cluster method
vardom_othstr           Variance estimation for sample surveys in domain 
                        using two stratification
vardomh                 Variance estimation for sample surveys in domain for one
                        or two stage surveys by the ultimate cluster method
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
  Santa Ivanova [aut]

Maintainer: Juris Breidaks <Juris.Breidaks@csb.gov.lv>
}
\keyword{ package }
