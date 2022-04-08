#' @title Calculate differences in estimated marginal means for a cluster bootstrap GLM
#' @description Returns differences in estimated marginal means and their confidence intervals of a \code{clusbootglm} object. 
#' @param x Object of class \code{clusbootemm}.
#' @param compare Variable that distinguishes values at which to compare estimated marginal means (e.g., a group variable)
#' @param at Variable that distinguishes different levels at which to compare estimated marginal means (e.g., a time variable).
#' @param compare.vals Values of the \code{compare} variable that should be compared.
#' @param confint.level Level of the confidence interval.
#' @param ... Additional arguments passed to other methods.
#' @return \code{pairs.clusbootemm} returns the differences between estimated marginal means for given values of \code{compare} at different levels of \code{at}.
#' @author Mathijs Deen
#' @examples
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' medication$time <- as.factor(medication$time)
#' set.seed(1)
#' model.1 <- clusbootglm(pos~time*treat, clusterid = id, data = medication)
#' ems <- emmeans(object = model.1)
#' pairs(ems, compare=treat, at=time)}
#' @importFrom methods hasArg
#' @export
pairs.clusbootemm <- function(x, compare, at, compare.vals, confint.level=.95, ...){
  arguments <- as.list(match.call())
  mf <- x$df
  bs <- x$bootstrapsample.emm
  g <- eval(arguments$compare, mf)
  a <- eval(arguments$at, mf)
  ifelse(hasArg(compare.vals), gvals <- compare.vals, gvals <- unique(g))
  if(length(gvals) != 2) stop("This function can only compare two values of compare.vals.", call. = FALSE)
  if(sum(gvals %in% g) < 2) stop("Not all values of argument compare.vals are in argument compare")
  avals <- unique(a)
  confint.pboundaries = c((1-confint.level)/2,1-(1-confint.level)/2)
  bs.1 <- bs[g==gvals[1],]
  bs.2 <- bs[g==gvals[2],]
  bs.diff <- bs.1 - bs.2
  bs.diff.m <- rowMeans(bs.diff)
  CI <- t(apply(bs.diff, 1, quantile, probs = confint.pboundaries))
  out <- data.frame(cbind(avals, difference = bs.diff.m, CI))
  names(out) <- c(as.character(arguments$at), "difference", dimnames(CI)[[2]])
  return(out)
}
