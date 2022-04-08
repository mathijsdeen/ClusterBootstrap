#' @title Plot estimated marginal means for a cluster bootstrap GLM
#' @description Plots the estimated marginal means of an \code{clusbootglm} object. Works with one within-subjects and/or one between-subjects variable.
#' @param x object of class \code{clusbootemm}.
#' @param within within-subjects variable. Should be numeric or numerically labeled factor.
#' @param between between-subjects variable.
#' @param pch point character. Length must be equal to the number of between-subjects levels.
#' @param lty linetype. Length must be equal to the number of between-subjects levels.
#' @param pcol point color. Length must be equal to the number of between-subjects levels.
#' @param lcol line color. Length must be equal to the number of between-subjects levels.
#' @param ylim limits of the y axis. If omitted, it will be based on the lowest and highest values within the confidence intervals of the estimated marginal means.
#' @param ylab label for y-axis.
#' @param xlab label for x-axis.
#' @param ... other arguments to be passed to the \code{plot} function (see \code{\link{par}}). 
#' @author Mathijs Deen
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' medication$time_f <- as.factor(medication$time)
#' set.seed(1)
#' model.1 <- clusbootglm(pos~time_f*treat, clusterid = id, data = medication)
#' emm.1 <- emm(object = model.1)
#' plot(x = emm.1, within = time_f, between = treat, pch = c(15,17), lty = c(1,2), 
#'      lcol = c("blue", "red"), pcol = c("blue","red"), )}
#' @importFrom graphics plot points lines arrows
#' @importFrom methods hasArg
#' @export
plot.clusbootemm <- function(x, within, between, pch, lty, pcol, lcol, ylim,
                             ylab="Estimated marginal mean", xlab="Within subject", ...){
  object <- x
  arguments <- as.list(match.call())
  grid <- object$grid
  within <- eval(arguments$within, grid)
  between <- eval(arguments$between, grid) #NULL if not specified
  if(is.factor(within)) within <- as.numeric(levels(within))[within]
  if(is.null(between)) between <- rep(1, length(within)) 
  if(!hasArg(pch)) pch <- rep(1,length(unique(between)))
  if(!hasArg(lty)) lty <- rep(1,length(unique(between)))
  if(!hasArg(lcol)) lcol <- rep("black",length(unique(between)))
  if(!hasArg(pcol)) pcol <- rep("black",length(unique(between)))
  if(!hasArg(ylim)) ylim <- c(min(grid$lower.CL),max(grid$upper.CL))
  if(length(unique(between)) != length(pch) | length(unique(between)) != length(lty) |
     length(unique(between)) != length(lcol) | length(unique(between)) != length(pcol)){
    stop(sprintf("Specified arguments pch, lty, lcol and pcol should be of length %d",
                 length(unique(between))), call.=FALSE)
  }
  emmean <- grid$emmean
  for(i in unique(between)){
    m <- which(unique(between) %in% i)
    if(m==1) {
      plot(within[between==i],emmean[between==i], ylim=ylim, pch=pch[m], 
           xlab=xlab, ylab=ylab, col=pcol[m])
    }else{
      points(within[between==i],emmean[between==i], pch=pch[m], col=pcol[m])
    }
    lines(within[between==i],emmean[between==i], lty=lty[m], col=lcol[m])
    arrows(within[between==i],grid$emmean[between==i], within[between==i],
           grid$lower.CL[between==i],angle=90,length=.05, col=lcol[m])
    arrows(within[between==i],grid$emmean[between==i], within[between==i],
           grid$upper.CL[between==i],angle=90,length=.05, col=lcol[m])
  }
}
