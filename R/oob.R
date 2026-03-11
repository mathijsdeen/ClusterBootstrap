#' Extract out-of-bag sample
#'
#' Extracts the out-of-bag (OOB) sample for a given iteration from a
#' bootstrap object.
#'
#' @param object A bootstrap object
#' @param samplenr Integer. The bootstrap sample number to extract
#' @param ... Additional arguments passed to methods
#'
#' @return A data frame containing the out-of-bag rows for the requested sample
#' @author Mathijs Deen
#' @export
oob <- function(object, samplenr, ...) UseMethod("oob")

#' @rdname oob
#' @export
oob.clusterBootstrap <- function(object, samplenr, ...) {
  stopifnot(inherits(object, "clusterBootstrap"),
            !is.null(object$indices),
            samplenr >= 1L,
            samplenr <= length(object$indices))
  
  df      <- eval(object$call$df)
  oob_idx <- setdiff(seq_len(nrow(df)), object$indices[[samplenr]])
  return(df[oob_idx, , drop = FALSE])
}