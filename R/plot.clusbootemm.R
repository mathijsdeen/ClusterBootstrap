#' @title Plot estimated marginal means for a cluster bootstrap GLM
#' @description Plots the estimated marginal means of an \code{clusbootglm} object. Works with one within-subjects and/or one between-subjects variable.
#' @param x object of class \code{clusbootemm}.
#' @param within within-subjects variable. Should be numeric or numerically labeled factor.
#' @param between between-subjects variable.
#' @param pch point character. Length must be equal to the number of between-subjects levels.
#' @param lty linetype. Length must be equal to the number of between-subjects levels.
#' @param ylab label for y-axis.
#' @param xlab label for x-axis.
#' @param ... other arguments to be passed to the \code{plot} function (see \code{\link{par}}). 
#' @author Mathijs Deen
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' medication$time_f <- as.factor(medication$time)
#' set.seed(1)
#' model.1 <- clusbootglm(pos~time_f*treat, clusterid=id, data=medication)
#' emm.1 <- emm(object = model.1)
#' plot(x = emm.1, within = time_f, between = treat, pch = c(15,17), lty = c(1,2))}
#' @importFrom graphics plot points lines arrows
#' @export
plot.clusbootemm <- function(x, within, between, pch, lty, ylab="Estimated marginal mean", xlab="Within subject", ...){
  object <- x
  arguments <- as.list(match.call())
  grid <- object$grid
  within <- eval(arguments$within, grid)
  between <- eval(arguments$between, grid)
  if(is.factor(within)) within <- as.numeric(levels(within))[within]
  if(is.null(between)) between <- rep(1, length(within))
  if(length(unique(between)) != length(pch) | length(unique(between)) != length(lty)){
    stop(sprintf("Arguments pch and lty should be of length %d",length(unique(between))), call.=FALSE)
  }
  emmean <- grid$emmean
  ylims <- c(min(grid$lower.CL),max(grid$upper.CL))
  for(i in unique(between)){
    m <- which(unique(between) %in% i)
    if(m==1) {
      plot(within[between==i],emmean[between==i], ylim=ylims, pch=pch[m], 
           xlab=xlab, ylab=ylab, ...)
    }else{
      points(within[between==i],emmean[between==i], pch=pch[m])
    }
    lines(within[between==i],emmean[between==i], lty=lty[m])
    for(i in 1:length(within)){
      arrows(within[i],grid[i,]$emmean,within[i],grid[i,]$lower.CL,angle=90,length=.05)
      arrows(within[i],grid[i,]$emmean,within[i],grid[i,]$upper.CL,angle=90,length=.05)
    }
  }
}