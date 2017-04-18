#' @title Extract matrix with bootstrap samples
#' @description Obtain a matrix containing the \code{clusterid} values for the bootstrap samples in a \code{clusbootglm} object.
#' @param object object of class \code{clusbootglm}, created with the \code{clusbootglm} function.
#' @param whichsample input which bootstrap sample(s) should be returned. Choose "all" for the complete bootstrap matrix, 
#'        "failed" for bootstrap samples that returned NAs, or a vector of values for specific bootstrap samples.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=opposites$Subject)
#' clusbootmatrix(cbglm.1, whichsample=c(1:5))}
#' @author Mathijs Deen
#' @export
clusbootmatrix <- function(object, whichsample="all"){
  objname <- match.call()$object
  if(!class(object)=="clusbootglm") stop(paste("'",objname,"' is not a clusbootglm class object", sep=""), call.=F)
  if(!is.numeric(whichsample)){
    if(whichsample=="all"){
      out <- with(object,bootstrap.matrix)
    }else if(whichsample=="failed"){
      out <- with(object, bootstrap.matrix[,which(is.na(rowSums(object$coefficients)))])
    }else{
      stop("\"whichsample\" should be \"all\", \"failed\" or numeric", call.=F)
    }
  }else{
    out <- with(object, bootstrap.matrix[,whichsample])
  }
  return(out)
}

#' @title Return data for specified bootstrap sample
#' @description Returns the full data frame for a specified bootstrap sample in a \code{clusbootglm} object.
#' @param object object of class \code{clusbootglm}, created with the \code{clusbootglm} function.
#' @param samplenr sample number for which the data frame should be returned.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=opposites$Subject)
#' clusbootsample(cbglm.1, samplenr=1)}
#' @author Mark de Rooij, Mathijs Deen
#' @export
clusbootsample <- function(object, samplenr){
  objname <- match.call()$object
  if(!class(object)=="clusbootglm") stop(paste("'",objname,"' is not a clusbootglm class object", sep=""), call.=F)
  cluster <- as.character(object$subject.vector)
  clusters <- unique(cluster)
  Obsno <- split(1:nrow(object$data), cluster)
  j <- object$bootstrap.matrix[,samplenr]
  obs <- unlist(Obsno[j])
  return(object$data[obs,])
}
