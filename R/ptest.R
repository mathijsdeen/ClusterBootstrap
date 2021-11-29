#' @title Permutation tests for group differences at within-subject levels on observed data
#' @description Generic function for performing permutation tests on either observed data or bootstrap samples
#' @param data data to be used for the permutation test(s).
#' @param ... additional arguments.
#' @author Mathijs Deen
#' @export
ptest <- function(data, ...) {
  UseMethod("ptest")
}