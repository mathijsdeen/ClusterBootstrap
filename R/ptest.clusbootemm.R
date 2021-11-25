#' @title Permutation tests for group differences at within-subject levels on observed data
#' @description The one for cluster bootstrap estimated marginal means
#' @param data object of type \code{clusbootemm}
#' @param within within-subject variable.
#' @param between between-subjects variable.
#' @param at.within determine for which within-subject levels (e.g., which timepoint) the difference should be tested.
#' @param at.between determine the groups in the difference test (should always be of length 2).
#' @param pn the number of permutations that should be performed.
#' @param progress.bar indicates whether a progress bar will be shown.
#' @param ... placeholder for outcome argument when data class is \code{data.frame}.
#' @author Mathijs Deen
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr starts_with
#' @importFrom magrittr %>%
#' @export
ptest.clusbootemm <- function(data, within, between, at.within, at.between, pn=1000, progress.bar=TRUE, ...) {
  bootfits <- data.frame(data$bootstrapsample.emm)
  names(bootfits) <- paste0("bootfit.", 1:ncol(bootfits))
  a <- cbind(data$df, bootfits)
  al <- a %>%
    pivot_longer(cols = starts_with("bootfit."),
                 names_prefix = "bootfit.",
                 names_to = "bootstrap.sample") %>%
    data.frame()
  arguments <- as.list(match.call())
  y <- al$value
  w <- eval(arguments$within, al)
  b <- eval(arguments$between, al)
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
