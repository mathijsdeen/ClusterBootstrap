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
  
  ## nette kolomnamen behouden (indien aanwezig) en als tibble teruggeven
  stats_tbl <- tibble::as_tibble(stats_mat,
                                 .name_repair = ~ names(t0) %||%
                                   paste0("stat", seq_along(t0)))
  
  stats_tbl
}

set.seed(2025)

# Speeldata met drie niveaus: school → klas → leerling
n_school  <- 3
n_class   <- 3
n_student <- 3

demo <- expand.grid(
  school  = paste0("S", 1:n_school),
  class   = paste0("C", 1:n_class),
  student = paste0("P", 1:n_student)
) |>
  mutate(score1 = rnorm(n()),
         score2 = rnorm(n())) |>
  arrange(school, class, student)

boot_fun <- function(d) c(a = mean(d$score1), b = mean(d$score2))
boot_fun2 <- function(d) matrix(c(a = mean(d$score1), b = mean(d$score2),
                                  c = mean(d$score1), d = mean(d$score2)),
                                nrow=4)

set.seed(1)
clusterBootstrap(df       = demo, 
                 clusters = c("school", "class", "student"),
                 replace  = c(TRUE, TRUE, TRUE),
                 stat_fun = boot_fun,
                 R        = 100)

out <- cluster_resample(
  demo,
  clusters = c("school", "class", "student"),
  replace  = c(FALSE, FALSE, TRUE)
)
out |>
  arrange(school, class, student)

