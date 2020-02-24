#' @title Permutation test for group differences at within-subject levels
#' @description Perform permutation tests for differences between two groups at given within-subject levels in a long-formatted dataframe
#' @param data dataframe that contains the data in long format.
#' @param outcome outcome variable (i.e., the variable for which the difference should be tested).
#' @param within within-subject variable.
#' @param between between-subjects variable.
#' @param at.within determine for which within-subject levels (e.g., which timepoint) the difference should be tested.
#' @param at.between determine the groups in the difference test (should always be of length 2).
#' @param pn the number of permutations that should be performed.
#' @param progress.bar indicates whether a progress bar will be shown.
#' @return \code{ptest} produces an object of class \code{"clusbootptest"}, containing the following relevant components:
#' \item{perm.statistics}{A matrix of \code{length(at.within)} rows and \code{pn} columns, containing the Welch t-test statics for all permutations within the \code{at.within} level in the columns. The first column contains the t statistic for the observed data.}
#' \item{pvalues}{Data frame containing the p values for every \code{at.within} level.}
#' @details In every permutation cycle, the outcome variable gets permutated and the Welch t test statistic is calculated. 
#' @seealso A useful method for the obtained \code{clusbootptest} class object is \code{\link{plot.clusbootptest}}.
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
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
ptest <- function(data, outcome, within, between, at.within, at.between, pn=1000, progress.bar=TRUE){
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

#' @title Plot results of a permutation test
#' @description Plot results of a permutation test performed with ptest
#' @param x object of class \code{clusbootptest}
#' @param pcol color of vertical line indicating the observed Welch t test statistic
#' @param pty type of vertical line indicating the observed Welch t test statistic
#' @param mfrow vector of length 2 indicating the numbers of rows and columns in which the histograms will be drawn on the device.
#' @param ... other arguments to be passed into the \code{hist} function.
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' set.seed(1)
#' permtest.1 <- ptest(data = meds, outcome = pos, within = time, between = treat, 
#'                     at.within = c(0,2,4,6), at.between = c(0,1), pn = 2000)
#' plot(permtest.1, pcol = "red", pty=2, mfrow = c(2,2), breaks="FD")}
#' @author Mathijs Deen, Mark de Rooij
#' @importFrom graphics abline hist par
#' @export
plot.clusbootptest <- function(x, pcol="red", pty=1, mfrow=c(1,1), ...){
  object <- x
  vals <- object$perm.statistics
  pvals <- object$pvalues$p
  nplots <- length(pvals)
  oldmfrow <- par()$mfrow
  par(mfrow=mfrow)
  for(i in 1:nplots){
    hist(vals[i,], 
         main=sprintf("Permutation distribution at %s = %s \n (p = %s)",
                      names(object$pvalues)[1], 
                      prettyNum(object$pvalues[i,1],digits=3), 
                      substr(object$pvalues[i,2], 2, min(5,nchar(as.character(object$pvalues[i,2]))))), 
         xlab="Welch t-statistic",
         ...)
    abline(v=vals[i,1], col=pcol, lty=pty)
  }
  par(mfrow=oldmfrow)
}

printpbmsg <- function(pn, arguments, at_w){
  cat(sprintf("Performing %d permutation tests for %s %s", 
              pn, 
              as.character(arguments$within), 
              ifelse(length(at_w)>1,"values","value")),
      sprintf("%s",
              ifelse(length(at_w)>1,
                     paste0(prettyNum(at_w,digits=3),
                            sep=c(rep(", ",length(at_w)-2), " and ",""),
                            collapse=""),
                     prettyNum(at_w,digits=3))),
      sprintf("\n"))
}