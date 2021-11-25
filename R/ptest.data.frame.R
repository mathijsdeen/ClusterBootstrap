#' @title Permutation tests for group differences at within-subject levels on observed data
#' @description Perform permutation tests for differences between two groups at given within-subject levels in a long-formatted dataframe
#' @param data data frame that contains the data in long format.
#' @param within within-subject variable.
#' @param between between-subjects variable.
#' @param at.within determine for which within-subject levels (e.g., which timepoint) the difference should be tested.
#' @param at.between determine the groups in the difference test (should always be of length 2).
#' @param pn the number of permutations that should be performed.
#' @param progress.bar indicates whether a progress bar will be shown.
#' @param outcome the outcome variable.
#' @param ... placeholder for outcome argument when data class is \code{data.frame}.
#' @return \code{ptest.data.frame} produces an object of class \code{"clusbootptest"}, containing the following relevant components:
#' \item{perm.statistics}{A matrix of \code{length(at.within)} rows and \code{pn} columns, containing the Welch t-test statics for all permutations within the \code{at.within} level in the columns. The first column contains the t statistic for the observed data.}
#' \item{pvalues}{Data frame containing the p values for every \code{at.within} level.}
#' @details In every permutation cycle, the outcome variable gets permutated and the Welch t test statistic is calculated. 
#' @seealso A useful method for the obtained \code{clusbootptest} class object is \code{\link{plot.clusbootptest}}.
#' @examples 
#' \dontrun{
#' meds <- medication[medication$time %% 1 == 0,]
#' set.seed(1)
#' permtest.1 <- ptest(data = meds, outcome = pos, within = time, between = treat, 
#'                     at.within = c(0,2,4,6), at.between = c(0,1), pn = 2000)
#' permtest.1$pvalues}
#' @author Mathijs Deen, Mark de Rooij
#' @import parallel
#' @import utils
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom stats t.test
#' @export
ptest.data.frame <- function(data, outcome, within, between, at.within, at.between, pn=1000, progress.bar=TRUE, ...){
  arguments <- as.list(match.call())
  y <- eval(arguments$outcome, data)
  w <- eval(arguments$within, data)
  b <- eval(arguments$between, data)
  d <- data.frame(y,w,b)
  at_w <- at.within
  at_b <- at.between
  wn <- length(at_w)
  ts <- matrix(NA, nrow=wn, ncol=pn)
  if(progress.bar){
    printpbmsg(pn, arguments, at_w)
    pb <- txtProgressBar(0, pn*length(at_w), style=3)
    c <- 0
  }
  for(i in 1:wn){
    pset <- d %>%
      dplyr::filter(b %in% at_b) %>%
      dplyr::filter(w == at_w[i])
    ts[i,1] <- t.test(y~b,pset)$statistic
    for(p in 2:pn){
      ts[i,p] <- t.test(formula = sample(y)~b, data = pset, alternative="two.sided")$statistic
      if(progress.bar){
        c <- c + 1
        setTxtProgressBar(pb, c)
      }
    }
  }
  ps <- apply(ts,1,function(x) sum(abs(x)>abs(x[1]))/pn)
  pvalues <- data.frame(at.within, round(ps,3))
  colnames(pvalues) <- c(arguments$within,"p")
  out <- list(perm.statistics=ts,pvalues=pvalues)
  class(out) <- "clusbootptest"
  return(out)
}