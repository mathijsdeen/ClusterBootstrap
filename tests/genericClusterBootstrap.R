# 
# 
# library(data.table)
# 
# clusterResample <- function(df, clusters, replace){
#   stopifnot(is.data.frame(df),
#             length(clusters) == length(replace))
#   
#   dt_original <- as.data.table(df)
#   dt_resampled <- copy(dt_original)
#   
#   for(level in seq_along(clusters)){
#     cl_var     <- clusters[level]
#     with_rep   <- replace[level]
#     group_vars <- if (level == 1L) character(0) else clusters[seq_len(level - 1L)]
#     
#     id_table <- unique(dt_original[, c(group_vars, cl_var), with = FALSE])
#     
#     # fix error when within group with n=1 you get a double which should be numeric
#     original_type <- typeof(dt_original[[cl_var]])
#     coerce_to_type <- switch(original_type,
#                              character = as.character,
#                              integer   = as.integer,
#                              double    = as.numeric,
#                              factor    = as.character, # safer as well
#                              stop("Unsupported cluster variable type: ", original_type))
#     
#     if(length(group_vars) == 0){
#       sampled_vec <- coerce_to_type(sample(x       = id_table[[cl_var]],
#                                            size    = nrow(id_table),
#                                            replace = with_rep))
#       sampled_ids <- data.table(tmp = sampled_vec)
#       setnames(sampled_ids, "tmp", cl_var)
#     } else{
#       sampled_ids <- id_table[, 
#                               .(sampled = coerce_to_type(sample(x       = get(cl_var), 
#                                                                 size    = .N, 
#                                                                 replace = with_rep))), 
#                               by = group_vars]
#       setnames(sampled_ids, "sampled", cl_var)
#     }
#     
#     dt_resampled <- merge(x               = sampled_ids,
#                           y               = dt_resampled,
#                           by              = c(group_vars, cl_var),
#                           allow.cartesian = TRUE,
#                           sort            = FALSE)
#   }
#   
#   setcolorder(dt_resampled, names(dt_original))
#   dt_resampled[]
# }
# 
# 
# 
# 
# clusterBootstrap <- function(df, clusters, replace,
#                              stat_fun, B = 5000, ...){
#   stopifnot(is.data.frame(df),
#             length(clusters) == length(replace),
#             is.function(stat_fun),
#             length(B) == 1L,
#             is.numeric(B),
#             B > 0)
#   
#   t0 <- stat_fun(df, ...)
#   if (!(is.numeric(t0) && is.null(dim(t0)))) {
#     stop("`stat_fun()` must return a numeric vector (not a matrix, array, or data frame).", 
#          call. = FALSE)
#   }
#   
#   one_rep <- function(){
#     boot_sample <- clusterResample(df, clusters, replace)
#     stat_fun(boot_sample, ...)
#   }
#   
#   res <- replicate(B, one_rep(), simplify = FALSE)
#   stats_mat <- do.call(rbind, res)
#   
#   col_names <- names(t0)
#   if (is.null(col_names)){
#     col_names <- paste0("stat", seq_along(t0))
#   }
#   
#   colnames(stats_mat) <- col_names
#   bootstrapEstimates <- as.data.frame(stats_mat)
#   originalEstimates <- as.data.frame(t(t0))
#   bootstrapSE <- apply(bootstrapEstimates, 2, sd, na.rm = TRUE)
#   outlist <- list(call               = match.call(),
#                   args               = as.list(match.call()),
#                   estimates          = list(originalEstimates  = originalEstimates,
#                                             bootstrapEstimates = bootstrapEstimates))
#   class(outlist) <- "clusterBootstrap"
#   return(outlist)
# }
