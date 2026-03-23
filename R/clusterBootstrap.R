#' Cluster Bootstrap
#'
#' Performs bootstrapping on hierarchically structured data
#' using clustered or nested resampling at any level of the hierarchy. Allows bootstrapping of arbitrary
#' statistics computed from the resampled dataset.
#'
#' @param df A data frame. The original dataset.
#' @param clusters A character vector of variable names that define the nested
#'   structure of the data, ordered from highest to lowest level.
#' @param replace A logical vector indicating whether sampling should be with
#'   replacement at each level. Should be of the same length as \code{clusters}.
#' @param statFun A function that takes a data frame (a bootstrap sample) and returns a numeric vector 
#'   of statistics.
#' @param B Integer. The number of bootstrap samples to generate.
#' @param keepIndices A logical value indicating whether the row numbers from \code{df} for the bootstrap 
#'   sample should be returned. See Value element \code{indices}.
#' @param nCores The number of cores that are used for parallel computing.
#' @param clusterExports A named list of objects and packages to export to 
#'   worker nodes when using parallel computing (\code{nCores > 1}). 
#'   Character elements are treated as package names and loaded via 
#'   \code{library()} on each worker. Non-character elements are treated as 
#'   objects and exported via \code{clusterExport()}. Ignored when 
#'   \code{nCores = 1}. 
#' @param ... Additional arguments passed to \code{statFun}.
#'
#' @return \code{clusterBootstrap} returns an object of class \code{clusterBootstrap}, containing the 
#' following elements:
#' \item{call}{The function call}
#' \item{args}{Arguments passed to the function}
#' \item{estimates}{A list with the following elements:
#'   \itemize{
#'     \item \code{originalEstimates}: a \code{data.frame} with one row, 
#'     containing the return of \code{statFun} on the original data.
#'     \item \code{bootstrapEstimates}: a \code{data.frame} with B rows, 
#'     containing the return of \code{statFun} on each of the bootstrap samples.
#'     \item \code{bootstrapSE}: the bootstrap standard error(s) for all rows in \code{bootstrapEstimates}. 
#'   }
#' }
#' \item{indices}{If \code{keepIndices=TRUE}, a list of length \code{B} where each element contains the row numbers
#' of \code{df} that is in the corresponding bootstrap sample. If \code{keepIndices=FALSE}, this element is \code{NULL}.}
#' 
#' @details
#' If out-of-bag (OOB) statistics (e.g., prediction error) are of interest, set 
#' \code{keepIndices = TRUE}. The resulting indices can be used post hoc with the \code{\link{inbag}} and 
#' \code{\link{oob}} methods to extract the bootstrap and OOB samples for any given iteration.
#'  
#' @seealso \code{\link{clusterResample}} for the underlying resampling mechanism. 
#' \code{\link{confint.clusterBootstrap}} for cluster bootstrap confidence intervals.
#' \code{\link{inbag}} and \code{\link{oob}} for extracting bootstrap and out-of-bag samples.
#' 
#' @note The argument \code{stat_fun} got renamed softly to \code{statFun} in \code{ClusterBootstrap 2.1.0}. 
#' The old argument still works but will be fully deprecated in the future.
#' @examples 
#' \dontrun{
#' library(dplyr)
#' medData <- medication |>
#' filter(time %% 1 == 0, time < 4)
#' bootFun <- function(d) lm(pos ~ treat*time, data = d)$coefficients
#' 
#' # Resampling on the person level only
#' clusterBootstrap(df       = medData, 
#'                  clusters = "id", 
#'                  replace  = TRUE, 
#'                  statFun  = bootFun, 
#'                  B        = 5000)
#'
#' # Resampling on the person level and the repeated measures level
#' clusterBootstrap(df       = medData, 
#'                  clusters = c("id", "time"), 
#'                  replace  = c(TRUE, TRUE), 
#'                  statFun  = bootFun, 
#'                  B        = 5000)
#' 
#' Resampling at multiple levels 
#' # (e.g., by design all classes in a probed school are included, 
#' # but not all students in a class)
#' set.seed(2025)
#' n_school  <- 30
#' n_class   <- 8
#' n_student <- 15
#' 
#' demo <- expand.grid(
#' school  = paste0("S", 1:n_school),
#' class   = paste0("C", 1:n_class),
#' student = paste0("P", 1:n_student)) |>
#'   mutate(score1 = rnorm(n()),
#'          score2 = rnorm(n())) |>
#'   arrange(school, class, student) |>
#'   slice(1:(n() - 3)) # slightly unbalanced data
#' bootFun2 <- function(d) lm(score1 ~ score2, data = d)$coef
#' clusterBootstrap(df       = demo, 
#'                  clusters = c("school", "class", "student"),
#'                  replace  = c(TRUE, FALSE, TRUE),
#'                  statFun  = bootFun2,
#'                  B        = 1000)
#' }
#' @exportPattern "^[^\\.]"
#' @importFrom parallel makeCluster stopCluster clusterSetRNGStream clusterExport clusterEvalQ parLapply detectCores clusterCall
#' @export
#' @author Mathijs Deen
clusterBootstrap <- function(df, 
                             clusters, 
                             replace,
                             statFun        = NULL, 
                             B              = 5000, 
                             keepIndices    = FALSE,
                             nCores         = 1L, 
                             clusterExports = list(),
                             ...){
  dots <- list(...)
  if (!is.null(dots$stat_fun) && is.null(statFun)) { 
    warning("Argument `stat_fun` is deprecated, please use `statFun` instead.", call. = FALSE)
    statFun <- dots$stat_fun
    dots$stat_fun <- NULL
  }
  if (length(clusterExports) > 0 && is.null(names(clusterExports))) {
    stop("`clusterExports` must be a named list.", call. = FALSE)
  }
  if (is.null(statFun)) stop("Argument `statFun` is missing with no default.", call. = FALSE)
  stopifnot(is.data.frame(df),
            length(clusters) == length(replace),
            is.function(statFun), 
            length(B) == 1L,
            is.numeric(B),
            B > 0,
            is.logical(keepIndices))
  
  tt_cores <- detectCores()
  if (is.na(tt_cores)) tt_cores <- 1L
  if (nCores > tt_cores) {
    message(sprintf("Note: nCores was set to %d, but only %d are available. Using all cores.", nCores, tt_cores))
    nCores <- tt_cores
  }
  if (!(nCores > 0 & nCores %% 1 == 0)) {
    stop(paste0("nCores must be a non-fractional number between 1 and ", tt_cores), call. = FALSE)
  }
  
  t0 <- do.call(statFun, c(list(df), dots))
  if(!(is.numeric(t0) && is.null(dim(t0)))){
    stop("`statFun()` must return a numeric vector (not a matrix, array, or data frame).",
         call. = FALSE)
  }
  
  one_rep <- function(){
    boot <- clusterResample(df, clusters, replace)
    list(stat    = do.call(statFun, c(list(boot$sample), dots)),
         indices = boot$indices)
  }
  
  if(nCores == 1L){
    res <- replicate(B, one_rep(), simplify = FALSE)
  } else{
    cl <- makeCluster(nCores)
    on.exit(stopCluster(cl))
    clusterSetRNGStream(cl, iseed = sample.int(.Machine$integer.max, 1L))
    
    # exports for clusterBootstrap
    clusterExport(cl, 
                  varlist = c("one_rep", "df", "clusters", "replace",
                              "statFun", "clusterResample", "dots"),
                  envir   = environment())
    clusterEvalQ(cl, library(data.table))
    
    # exports defined by user
    if (length(clusterExports) > 0) {
      pkg_mask <- sapply(clusterExports, is.character)
      obj_mask <- !pkg_mask
      if (any(obj_mask)) {
        list2env(clusterExports[obj_mask], envir = environment())
        clusterExport(cl, varlist = names(clusterExports)[obj_mask], envir = environment())
      }
      if (any(pkg_mask)) {
        clusterCall(cl, function(p) lapply(p, library, character.only = TRUE), 
                    unlist(clusterExports[pkg_mask]))
      }
    }
    
    res <- parLapply(cl, seq_len(B), function(i) one_rep())
  }
  
  stats_mat <- do.call(rbind, lapply(res, `[[`, "stat"))
  col_names <- names(t0)
  if (is.null(col_names)){
    col_names <- paste0("stat", seq_along(t0))
  }
  colnames(stats_mat)  <- col_names
  
  bootstrapEstimates   <- as.data.frame(stats_mat)
  originalEstimates    <- as.data.frame(t(t0))
  bootstrapSE          <- apply(bootstrapEstimates, 2, sd, na.rm = TRUE)
  
  outlist <- list(call      = match.call(),
                  args      = as.list(match.call()),
                  estimates = list(originalEstimates  = originalEstimates,
                                   bootstrapEstimates = bootstrapEstimates,
                                   bootstrapSE        = bootstrapSE),
                  indices   = if (keepIndices) lapply(res, `[[`, "indices") else NULL)
  
  class(outlist) <- "clusterBootstrap"
  return(outlist)
}
