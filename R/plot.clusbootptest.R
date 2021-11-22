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