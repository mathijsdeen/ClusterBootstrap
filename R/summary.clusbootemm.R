#' @title Summarize estimated marginal means for cluster bootstrap GLM into a grid
#' @description Returns the summary of the EMM for a \code{clusbootglm} class object.
#' @param object object of class \code{clusbootemm}.
#' @param ... other arguments.
#' @author Mathijs Deen
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' medication$time_f <- as.factor(medication$time)
#' set.seed(1)
#' model.1 <- clusbootglm(pos~time_f*treat, clusterid=id, data=medication)
#' emm.1 <- emm(object = model.1)
#' summary(object = emm.1)}
#' @export
summary.clusbootemm <- function(object,...){
  print(object$grid, row.names = FALSE)
}