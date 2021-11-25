#' @title Permutation test for group differences at within-subject levels on observed data
#' @description The one for cluster bootstrap estimated marginal means
#' @param data object of type \code{clusbootemm}
#' @param outcome outcome variable (i.e., the variable for which the difference should be tested).
#' @param within within-subject variable.
#' @param between between-subjects variable.
#' @param at.within determine for which within-subject levels (e.g., which timepoint) the difference should be tested.
#' @param at.between determine the groups in the difference test (should always be of length 2).
#' @param pn the number of permutations that should be performed.
#' @param progress.bar indicates whether a progress bar will be shown.
#' @author Mathijs Deen
#' @export
ptest.clusbootemm <- function(data, outcome, within, between, at.within, at.between, pn=1000, progress.bar=TRUE) {
  print("hi")
  return()
}
