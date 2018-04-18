#' @name medication
#' @title Medication data
#' @description The \code{medication} dataframe consists of 1242 observations within 73 individuals 
#' that were part of a placebo controlled clinical trial, as reported in Tomarken, Shelton, Elkins, and Anderson (1997).
#' 
#' The data were retrieved from the accompanied website of Singer & Willett (2003), at https://stats.idre.ucla.edu/other/examples/alda/.
#' @usage medication
#' @format the following variables are available:
#' \itemize{
#'  \item \code{id}: subject indicator
#'  \item \code{treatment}: either placebo (0) or antidepressant (1)
#'  \item \code{time}: number of days since trial start. 
#'  \item \code{pos}: positive affect. Higher scores indicate a more positive mood.
#' }
#' @docType data
#' @references
#'  \itemize{
#'   \item Singer, J.D., & Willett, J.B. (2003). \emph{Applied longitudinal data analysis. Modeling change and event occurence.} 
#'   NY: Oxford University Press, Inc.
#'   \item Tomarken, A.J., Shelton, R.C., Elkins, L., & Anderson, T (1997). \emph{Sleep deprivation and anti-depressant medication: Unique effects on positive and negative affect.}
#'   Poster session presented at the 9th annual meeting of the American Psychological Society, Washington, DC.
#'  }
NULL