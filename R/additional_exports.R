#' @title Return data for specified bootstrap sample
#' @description Returns the full data frame for a specified bootstrap sample in a \code{clusbootglm} object.
#' @param object object of class \code{clusbootglm}, created with the \code{clusbootglm} function.
#' @param samplenr sample number for which the data frame should be returned.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)
#' clusbootsample(cbglm.1, samplenr=1)}
#' @author Mark de Rooij, Mathijs Deen
#' @export
clusbootsample <- function(object, samplenr){
  objname <- match.call()$object
  if(!class(object)=="clusbootglm"){
    options(error=NULL)
    stop(paste("'",objname,"' is not a clusbootglm class object", sep=""), call.=F)
  }
  cluster <- as.character(object$subject.vector)
  clusters <- unique(cluster)
  Obsno <- split(1:nrow(object$data), cluster)
  j <- object$bootstrap.matrix[,samplenr]
  obs <- unlist(Obsno[j])
  return(object$data[obs,])
}
