#' Extract in-bag bootstrap sample
#'
#' Extracts the in-bag (bootstrap) sample for a given iteration from a
#' bootstrap object.
#'
#' @param object A bootstrap object
#' @param samplenr Integer. The bootstrap sample number to extract
#' @param ... Additional arguments passed to methods
#'
#' @return A data frame containing the in-bag rows for the requested sample
#' @author Mathijs Deen
#' @export
inbag <- function(object, samplenr, ...) UseMethod("inbag")

#' @rdname inbag
#' @export
inbag.clusterBootstrap <- function(object, samplenr, ...) {
  stopifnot(inherits(object, "clusterBootstrap"),
            !is.null(object$indices),
            samplenr >= 1L,
            samplenr <= length(object$indices))
  
  df <- eval(object$call$df)
  return(df[object$indices[[samplenr]], , drop = FALSE])
}