# purr- & dplyr-based  -----------------------------------------------
library(dplyr)
library(rlang)   # voor .data pronouns
library(tibble)

clusterResample <- function(df, clusters, replace) {
  stopifnot(length(clusters) == length(replace),
            is.data.frame(df))
  
  resampleRecursive <- function(data, cl, rep) {
    if (length(cl) == 0) return(data)
    
    cl_var   <- cl[1]
    with_rep <- rep[1]
    
    ids <- unique(data[[cl_var]])
    
    ids |>
      sample(size    = length(ids),
             replace = with_rep) |>
      lapply(function(id) {
        data |>
          dplyr::filter(.data[[cl_var]] == id) |>
          resampleRecursive(cl[-1], rep[-1]) # recursion
      }) |>
      bind_rows()
  }
  
  resampleRecursive(df, clusters, replace)
}

library(data.table)

clusterResampleDT <- function(df, clusters, replace) {
  stopifnot(is.data.frame(df),
            length(clusters) == length(replace))
  
  dt_original <- as.data.table(df)
  dt <- copy(dt_original)
  
  # Voeg een index toe om op het eind de volgorde/structuur te herstellen
  dt[, .orig_row := .I]
  
  # Start met een kolom die alles selecteert
  dt_resampled <- dt
  
  for (level in seq_along(clusters)) {
    cl_var <- clusters[level]
    with_rep <- replace[level]
    
    if (level == 1L) {
      # Top-niveau: sample over hele dataset
      ids <- unique(dt_resampled[[cl_var]])
      sampled_ids <- sample(ids, length(ids), replace = with_rep)
      sampled <- data.table(sampled_ids, .sample_id = seq_along(sampled_ids))
      setnames(sampled, "sampled_ids", cl_var)
      
      dt_resampled <- merge(
        sampled,
        dt_resampled,
        by = cl_var,
        allow.cartesian = TRUE
      )
      setorder(dt_resampled, .sample_id)
      dt_resampled[, .sample_id := NULL]
    } else {
      # Lagere niveaus: sample binnen groep van hogere niveaus
      group_vars <- clusters[seq_len(level - 1L)]
      
      dt_resampled <- dt_resampled[,
                                   .SD[sample(.N, .N, replace = with_rep)],
                                   by = group_vars]
    }
  }
  
  # Zet kolommen terug in originele volgorde (zonder .orig_row)
  setcolorder(dt_resampled, names(dt_original))
  dt_resampled[]
}




clusterBootstrap <- function(df, clusters, replace,
                             stat_fun, R = 1000, ...) {
  stopifnot(
    is.data.frame(df),
    length(clusters) == length(replace),
    is.function(stat_fun),
    length(R) == 1L, 
    is.numeric(R), 
    R > 0,
    is.numeric(stat_fun(df, ...))
  )
  
  t0 <- stat_fun(df, ...)
  
  one_rep <- function() {
    bootSample <- clusterResample(df, clusters, replace)
    stat_fun(bootSample, ...)
  }
  
  res <- replicate(R, one_rep(), simplify = FALSE)
  stats_mat <- do.call(rbind, res)
  
  stats_tbl <- tibble::as_tibble(stats_mat,
                                 .name_repair = ~ names(t0) %||%
                                   paste0("stat", seq_along(t0)))
  
  stats_tbl
}

