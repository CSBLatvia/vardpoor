#' Linearization of the gender pay (wage) gap
#'
#' @description Estimation of gender pay (wage) gap and computation of linearized variables for variance estimation.
#'
#' @param Y Study variable (for example the gross hourly earning). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param gender Numerical variable for gender, where 1 is for males, but 2 is for females. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param id Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param weight Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, estimation and linearization of gender pay (wage) gap is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param period Optional variable for survey period. If supplied, estimation and linearization of gender pay (wage) gap is done for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param  var_name A character specifying the name of the linearized variable.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#'
#' @return A list with two objects are returned:
#' \itemize{
#'  \item \code{value} - a \code{data.table} containing the estimated gender pay (wage) gap (in percentage).
#'  \item \code{lin} - a \code{data.table} containing the linearized variables of the gender pay (wage) gap (in percentage) for variance estimation.
#'  }
#'  
#' @references
#' Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap. \emph{EU-SILC 131-rev/04}, Eurostat.  \cr
#' Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}.  \cr
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/1999002/article/4882-eng.pdf}.  \cr
#'
#' @seealso \code{\link{linqsr}}, \code{\link{lingini}},
#'          \code{\link{varpoord}} , \code{\link{vardcrospoor}},
#'           \code{\link{vardchangespoor}}
#' @keywords Linearization
#' 
#' @examples
#' library("data.table")
#' library("laeken")
#' data("ses")
#' dataset1 <- data.table(ID = paste0("V", 1 : nrow(ses)), ses)
#' 
#' dataset1[, IDnum := .I]
#' 
#' setnames(dataset1, "sex", "sexf")
#' dataset1[sexf == "male", sex:= 1]
#' dataset1[sexf == "female", sex:= 2]
#'   
#' # Full population
#' gpgs1 <- lingpg(Y = "earningsHour", gender = "sex",
#'                 id = "IDnum", weight = "weights",
#'                 dataset = dataset1)
#' gpgs1$value
#'   
#' \dontrun{
#' # Domains by education
#' gpgs2 <- lingpg(Y = "earningsHour", gender = "sex",
#'                 id = "IDnum", weight = "weights",
#'                 Dom = "education", dataset = dataset1)
#' gpgs2$value
#'     
#' # Sort variable
#' gpgs3 <- lingpg(Y = "earningsHour", gender = "sex",
#'                 id = "IDnum", weight = "weights",
#'                 sort = "IDnum", Dom = "education",
#'                 dataset = dataset1)
#' gpgs3$value
#'     
#' # Two survey periods
#' dataset1[, year := 2010]
#' dataset2 <- copy(dataset1)
#' dataset2[, year := 2011]
#' dataset1 <- rbind(dataset1, dataset2)
#' 
#' gpgs4 <- lingpg(Y = "earningsHour", gender = "sex",
#'                 id = "IDnum", weight = "weights", 
#'                 sort = "IDnum", Dom = "education",
#'                 period = "year", dataset = dataset1)
#' gpgs4$value
#' names(gpgs4$lin)}
#'   
#' @import data.table
#' @import laeken
#' @export lingpg


lingpg <- function(Y, gender = NULL, id = NULL,
                   weight = NULL, sort = NULL,
                   Dom = NULL, period = NULL,
                   dataset = NULL, var_name = "lin_gpg",
                   checking = TRUE) {

   ## initializations

   if (min(dim(as.data.frame(var_name)) == 1) != 1) {
          stop("'var_name' must have defined name of the linearized variable")}

   if (is.null(gender)) stop("'gender' must be supplied")

   if (checking) {
          Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                         ncols = 1, isnumeric = TRUE,
                         isvector = TRUE, grepls = "__")
          Ynrow <- length(Y)

          gender <- check_var(vars = gender, varn = "gender",
                              dataset = dataset, ncols = 1,
                              Ynrow = Ynrow, isnumeric = TRUE,
                              isvector = TRUE)

          weight <- check_var(vars = weight, varn = "weight",
                              dataset = dataset, ncols = 1,
                              Ynrow = Ynrow, isnumeric = TRUE,
                              isvector = TRUE)

          sort <- check_var(vars = sort, varn = "sort",
                            dataset = dataset, ncols = 1,
                            Ynrow = Ynrow, mustbedefined = FALSE,
                            isnumeric = TRUE, isvector = TRUE)

          period <- check_var(vars = period, varn = "period",
                              dataset = dataset, Ynrow = Ynrow,
                              ischaracter = TRUE, mustbedefined = FALSE,
                              duplicatednames = TRUE)

          Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                           Ynrow = Ynrow, ischaracter = TRUE,
                           mustbedefined = FALSE, duplicatednames = TRUE,
                           grepls = "__")

          id <- check_var(vars = id, varn = "id", dataset = dataset,
                          ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                          periods = period)
     }


  ## computations
  ind0 <- rep.int(1, length(Y))
  period_agg <- period1 <- NULL
  if (!is.null(period)) { period1 <- copy(period)
                          period_agg <- data.table(unique(period))
                      } else period1 <- data.table(ind = ind0)
  period1_agg <- data.table(unique(period1))

  # GPG by domain (if requested)
  gpg_id <- id
  if (!is.null(period)) gpg_id <- data.table(gpg_id, period)

  if(!is.null(Dom)) {
       Dom_agg <- data.table(unique(Dom))
       setkeyv(Dom_agg, names(Dom_agg))

       gpg_v <- c()
       gpg_m <- copy(gpg_id)

       for(i in 1 : nrow(Dom_agg)) {
           g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
           var_nams <- do.call(paste, as.list(c(g, sep = "__")))
           indi <- (rowSums(Dom  ==  Dom_agg[i,][ind0,]) == ncol(Dom))

           gpg_l <- lapply(1 : nrow(period1_agg), function(j) {
                indj <- ((rowSums(period1 == period1_agg[j,][ind0,])  ==  ncol(period1))&(indi))
                if (!is.null(period)) { rown <- cbind(period_agg[j], Dom_agg[i])
                                    } else rown <- Dom_agg[i]
                gpgl <- linGapCalc(x = Y[indj], gend = gender[indj],
                                   ids = gpg_id[indj], weights = weight[indj],
                                   sort = sort[indj])
                list(data.table(rown, gpg = gpgl$gpg_pr), gpgl$lin)
             })

           gpgs <- rbindlist(lapply(gpg_l, function(x) x[[1]]))
           gpglin <- rbindlist(lapply(gpg_l, function(x) x[[2]]))

           setnames(gpglin, names(gpglin), c(names(gpg_id), var_nams))
           gpg_m <- merge(gpg_m, gpglin, all.x = TRUE, by = names(gpg_id))
           gpg_v <- rbind(gpg_v, gpgs)
         }
     } else { gpg_l <- lapply(1 : nrow(period1_agg), function(j) {
                           indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           gpg_l <- linGapCalc(x = Y[indj], gend = gender[indj],
                                               ids = gpg_id[indj], weights = weight[indj],
                                               sort = sort[indj])

                           if (!is.null(period)) {
                                    gpgs <- data.table(period_agg[j], gpg = gpg_l$gpg_pr)
                              } else gpgs <- data.table(gpg = gpg_l$gpg_pr)
                           list(gpg = gpgs, lin = gpg_l$lin)
                       })
               gpg_v <- rbindlist(lapply(gpg_l, function(x) x[[1]]))
               gpg_m <- rbindlist(lapply(gpg_l, function(x) x[[2]]))
               setnames(gpg_m, names(gpg_m), c(names(gpg_id), var_name))
            }
    gpg_m[is.na(gpg_m)] <- 0
    setkeyv(gpg_m, names(gpg_id))
    return(list(value = gpg_v, lin = gpg_m))
 }


  ## workhorse
 linGapCalc <- function(x, gend, ids, weights = NULL, sort = NULL) {
    if(is.null(gend)) stop("'gender' must be supplied")
    if (length(gend) != length(x)) stop("'x' is not the same as 'gend'")
    if (length(gend) != length(weights)) stop("'weights' is not the same as 'gend'")

    if (is.null(weights)) weights <- rep.int(1, length(x))  # equal weights

    indic_men <- ifelse(gend == 1, 1, 0)
    indic_women <- ifelse(gend == 2, 1, 0)

    x[is.na(x)] <- 0

    Nmen <- sum(weights * indic_men)
    Nwomen <- sum(weights * indic_women)
    SINCmen <- sum(weights * x * indic_men)
    SINCwomen <- sum(weights * x * indic_women)

    Num <- SINCmen / Nmen - SINCwomen / Nwomen
    Den <- SINCmen / Nmen
    gpg <- Num / Den # Estimated gender pay gap
    gpg_pr <- gpg * 100

 #-------------------------- Linearized variable (in %) -----------------------
    lin <- 100 * (1 - gpg) * ((indic_women / Nwomen) - (indic_men / Nmen) + ((x * indic_men) / SINCmen) - ((x * indic_women) / SINCwomen))
 #-----------------------------------------------------------------------------

    if (length(unique(gend)) != 2 | is.nan(gpg)) gpg_pr <- lin <- 0

    lin_id <- data.table(ids, lin)

    gpg <- data.table(gpg_pr = gpg_pr)
    return(list(gpg_pr = gpg_pr, lin = lin_id))
 }

