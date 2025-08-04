#' Confidence intervals for clusterBootstrap objects
#'
#' Computes confidence intervals for estimates obtained via clustered bootstrap resampling.
#' Supported interval types are: percentile (default), normal approximation (parametric),
#' and bias-corrected (BC).
#'
#' @param object An object of class \code{"clusterBootstrap"}, as returned by \code{\link{clusterBootstrap}}.
#' @param parm A character vector of parameter names to compute confidence intervals for.
#'   If \code{NULL} (default), intervals are computed for all parameters.
#' @param level Confidence level, e.g., \code{0.95} for a 95\% confidence interval.
#' @param type Type of confidence interval. One of \code{"percentile"}, \code{"parametric"}, or \code{"bc"}.
#'   The default is \code{"percentile"}.
#' @param ... Currently ignored. Included for method compatibility.
#'
#' @details
#' \itemize{
#'   \item \strong{Percentile}: uses the empirical quantiles of the bootstrap estimates.
#'   \item \strong{Parametric}: uses the bootstrap standard error and assumes normality.
#'   \item \strong{Bias-corrected (BC)}: adjusts for bias in the bootstrap distribution.
#'     Note: acceleration (BCa) is not implemented.
#' }
#'
#' @return A \code{data.frame} with one row per parameter and the following columns:
#' \describe{
#'   \item{term}{The name of the parameter.}
#'   \item{type}{The type of confidence interval used.}
#'   \item{conf.low}{The lower bound of the confidence interval.}
#'   \item{conf.high}{The upper bound of the confidence interval.}
#' }
#' @examples 
#' \dontrun{
#' set.seed(2025)
#' n_school  <- 30
#' n_class   <- 8
#' n_student <- 15
#' 
#' demo <- expand.grid(school  = paste0("S", 1:n_school),
#'                     class   = paste0("C", 1:n_class),
#'                     student = paste0("P", 1:n_student)) |>
#'   mutate(score1 = rnorm(n()),
#'   score2 = rnorm(n())) |>
#'   arrange(school, class, student) |>
#'   slice(1:(n() - 3)) # slightly unbalanced data
#' bootFun2 <- function(d) lm(score1 ~ score2, data = d)$coef
#' clusterBootstrap(df       = demo, 
#'                  clusters = c("school", "class", "student"),
#'                  replace  = c(TRUE, FALSE, TRUE),
#'                  stat_fun = bootFun2,
#'                  B        = 1000) |>
#'   confint()
#' }
#' @seealso \code{\link{clusterBootstrap}}, \code{\link[stats]{confint}}
#' @export
confint.clusterBootstrap <- function(object,
                                     parm  = NULL,
                                     level = 0.95,
                                     type  = c("percentile", "parametric", "bc"),
                                     ...){
  type   <- match.arg(type)
  t0     <- object$estimates$originalEstimates
  tBoot  <- object$estimates$bootstrapEstimates
  seBoot <- object$estimates$bootstrapSE
  alpha  <- 1 - level
  lower  <- alpha / 2
  upper  <- 1 - alpha / 2
  
  if(!is.null(parm)){
    t0     <- t0[, parm, drop=FALSE]
    tBoot  <- tBoot[, parm, drop=FALSE]
    seBoot <- seBoot[parm]
  }
  
  termNames <- names(t0)

  ciMat <- vapply(X         = seq_along(t0), 
                  FUN       = function(j){
                                theta0 <- t0[[j]]
                                boots  <- tBoot[[j]]
                                if(type == "percentile"){
                                  quantile(x     = boots, 
                                           probs = c(lower, upper), 
                                           na.rm = TRUE)
                                }else if(type == "parametric"){
                                  theta0 + qnorm(c(lower, upper)) * seBoot[[j]]
                                }else if(type == "bc"){
                                  z0       <- qnorm(mean(boots < theta0, na.rm = TRUE))
                                  zAlpha   <- qnorm(c(lower, upper))
                                  adjProbs <- pnorm(2 * z0 + zAlpha)
                                  quantile(x     = boots, 
                                           probs = adjProbs, 
                                           na.rm = TRUE)
                                }
                              }, 
                  FUN.VALUE = numeric(2L))
  
  data.frame(term      = termNames,
             conf.low  = ciMat[1, ],
             conf.high = ciMat[2, ],
             row.names = NULL)
}
