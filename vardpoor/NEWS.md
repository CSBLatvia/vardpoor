Changes in vardpoor version 0.20.3 (dev)

+ Corrected bug in function vardcros (thanks to @lilianaroze).

Changes in vardpoor version 0.20.1 (CRAN)

+ Bug fix in vardannual regarding the R version 4.1.0 (get() now signal an error if the first argument has length greater than 1)
+ Function `vardbootstr` has been removed from the package as it is in development (exporting of the function has been disabled, see the .Rbuildignore)
+ Bug fixes for some examples
+ Partial spell check for documentation
+ Change of the package maintainer

Changes in vardpoor version 0.14.0

+ Corrected bug in function varchanges
+ Corrected bug in function vardchangespoor

Changes in vardpoor version 0.10.0

+ Add new function vardchangstrs
+ Updated function vardchangespoor 

Changes in vardpoor version 0.9.11

+ Corrected bug in function vardom_othstr, when calibration is used and not defined variable ind_gr 

Changes in vardpoor version 0.9.10

+ Add confidence level in vardom, vardomh, varpoord, vardcros
+ Bug correction

Changes in vardpoor version 0.9.4

+ Add variable Subperiods checking in function vardannual
+ Corrected some bugs in manual

Changes in vardpoor version 0.9.0

+ Corrected bug in function domain
+ Vardannual contains previous varchangannual and vardcrosannual together

Changes in vardpoor version 0.8.6

+ Add new function for variable checking in 
+ Add possibility turn off checking in many functions

Changes in vardpoor version 0.8.4

+ Corrected mistakes in function varpoord

Changes in vardpoor version 0.8.2

+ Rewrite of CITATION according to the style recomended by the R manual
+ Residual_est check faster, is X matrix numeric
+ Bug correctin vardchangesannual when X is available
+ Corrected mistakes in function vardomh, vardcros

Changes in vardpoor version 0.8.0

+ Corrected mistakes in function vardomh

Changes in vardpoor version 0.7.8

+ In function vardcros, vardchanges, vardchangannual add calibration information, when variable X is defined
+ Add new function vardcrosannual

Changes in vardpoor version 0.7.6

+ Corrected mistake in function vardomh and varpood when variable X is defined

Changes in vardpoor version 0.7.4

+ Corrected mistake when variable Dom or H is factor variable with NA values
+ Some error message corrections

Changes in vardpoor version 0.7.2

+ Add new output argument S2_y_HT, S2_y_ca in functions vardom, vardomh, varpoord
+ Corrected mistakes in linearization functions linrmir and linarr

Changes in vardpoor version 0.7.0

+ Corrected mistakes in linearization functions linrmir and linarr

Changes in vardpoor version 0.6.4

+ Add new functionality for variance_est

Changes in vardpoor version 0.6.3

+ Corrected mistake in R 3.3.0 , when used calibration matrix and g vardomh, varpoord

Changes in vardpoor version 0.6.2

+ Corrected mistake, when used argument period in functions vardom, vardomh, varpoord

Changes in vardpoor version 0.6.0

+ Updated description for functions vardom, vardomh, vardpoor, vardom_othstr
+ Updated variable n_eff in function vardom, and n_eff removed from functions vardomh, vardpoor, vardom_othstr

Changes in vardpoor version 0.5.9

+ Return back variable use.estVar in function vardchanges, vardchangespoor

Changes in vardpoor version 0.5.8

+ Corrected small mistake in function vardchanges, vardchangespoor

Changes in vardpoor version 0.5.6

+ Removed variable use.estVar from function vardchanges, vardcros
+ Corrected mistake in function variance_othstr, when Nh in strata is larger nh, variance is zero in these strata
+ Print out information about those strata

Changes in vardpoor version 0.5.4

+ Corrected mistake in function lin.ratio using percentratio
+ Corrected mistake in function varchanganges using relative changes
+ Add variable which detect significance level in vardchanges, vardchangannual

Changes in vardpoor version 0.5.2

+ Corrected mistake in function varchanganges

Changes in vardpoor version 0.5.0

+ Corrected mistake in function vardchanges

Changes in vardpoor version 0.4.8

+ Corrected mistake in function vardom, vardomh, varpood when used period variable

Changes in vardpoor version 0.4.6

+ Corrected mistakes in function linrmir
+ In all functions changed type of dataset from data.frame to data.table
+ Corrected dataset type in examples

Changes in vardpoor version 0.4.4

+ Corrected mistakes in function lingpg

Changes in vardpoor version 0.4.2

+ Corrected description in function variance_est
       
Changes in vardpoor version 0.4.0

+ Corrected bug in function vardpoor using subfunction linrmi and linarr
+ Add new examples for vardpoor and vardomh using argument fh_zero

Changes in vardpoor version 0.3.6

+ Corrected bug in function vardpoor

Changes in vardpoor version 0.3.4

+ Corrected mistakes in function lingpg

Changes in vardpoor version 0.3.2

+ Add new functions vardcrospoor and vardchangespoor package

Changes in vardpoor version 0.3.0

+ Some small changes with new gdata package

Changes in vardpoor version 0.2.8

+ Update function descriptions

Changes in vardpoor version 0.2.6

+ Corrected mistake in function vardomh when Z is not NULL and period is not NULL.

Changes in vardpoor version 0.2.5

+ Corrected mistake in function vardomh and vardom when period is not NULL show allways error message that H is not equal with period

Changes in vardpoor version 0.2.4

+ Corrected mistake in function vardchanges that the elements of the correlation matrix can not be larger than 1

Changes in vardpoor version 0.2.22

+ Updated functions vardcros

Changes in vardpoor version 0.2.0.14

+ Corrected somes mistakes
+ Add sample size, population size, variable count with nonzero values for functions vardom, vardpoor, vardomh

Changes in vardpoor version 0.2.0.13

+ Add sample size for functions vardom, vardomh, vardpoor

Changes in vardpoor version 0.2.0.11

+ Update function checking

Changes in vardpoor version 0.2.9.2

+ Update manuals

Changes in vardpoor version 0.2.9.1

+ Update manuals

Changes in vardpoor version 0.2.9.0

+ Add new functions vardcros, vardchanges

Changes in vardpoor version 0.2.8.2
   
+ Update manuals

Changes in vardpoor version 0.2.0.0
   
+ Add some new functions

Changes in vardpoor version 0.1.9
   
+ Updated functions vardom, vardomsilc and vardpoor and thoses functions help files

Changes in vardpoor version 0.1.7
   
+ Updated functions vardomsilc and vardpoor and thoses functions help files

Changes in vardpoor version 0.1.5
   
+ Add new function for SILC
+ Updated some functions
+ Updated some help files

Changes in vardpoor version 0.1.4
   
+ Updated some functions
+ Updated some help files
