library(dplyr)
library(data.table)
library(microbenchmark)
library(ClusterBootstrap)
library(parallel)
set.seed(2026)
generateData <- function(nSchools, nClasses, nStudents){
  expand.grid(school  = paste0("S", seq_len(nSchools)),
              class   = paste0("C", seq_len(nClasses)),
              student = paste0("S", seq_len(nStudents))) |>
    mutate(score1 = rnorm(n()),
           score2 = rnorm(n()))
}

set.seed(1)
d <- generateData(5,5,5)
dr <- clusterResample(d, c("school","class","student"), c(T,T,F), T)
dr
oob_mse <- function(inbag, oob, outcome, predictors){
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  fit     <- lm(formula, data = as.data.frame(inbag))
  
  inbag_pred <- predict(fit, newdata = inbag)
  oob_pred   <- predict(fit, newdata = oob)
  
  c(inbag_mse = mean((inbag[[outcome]] - inbag_pred)^2),
    oob_mse   = mean((oob[[outcome]]   - oob_pred)^2))
}

set.seed(1)
df_test <- generateData(10,5,20)
clusterResample(df_test, c("school","class","student"), c(T,T,F))
result <- clusterBootstrap(df       = df_test,
                           clusters = c("school", "class", "student"),
                           replace  = c(TRUE, TRUE, TRUE),
                           stat_fun = oob_mse,
                           B        = 500,
                           oob      = TRUE,
                           ncores   = 9,
                           outcome    = "score1",
                           predictors = "score2")

head(result$estimates$bootstrapEstimates)
result$estimates$bootstrapSE
hist(result$estimates$bootstrapEstimates$inbag_mse, breaks = 20)
hist(result$estimates$bootstrapEstimates$oob_mse, breaks = 20)
with(result$estimates$bootstrapEstimates, cor(inbag_mse, oob_mse))

### benchmark parallellization
library(microbenchmark)

set.seed(1)
df_small  <- generateData( 5,  4, 10)
df_medium <- generateData(10,  8, 20)
df_large  <- generateData(20, 15, 50)

clusters <- c("school", "class", "student")
replace  <- c(TRUE, TRUE, TRUE)

bm_parallel <- microbenchmark(
  small_1  = clusterBootstrap(df_small,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 1L, outcome = "score1", predictors = "score2"),
  small_2  = clusterBootstrap(df_small,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 2L, outcome = "score1", predictors = "score2"),
  small_3  = clusterBootstrap(df_small,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 3L, outcome = "score1", predictors = "score2"),
  small_4  = clusterBootstrap(df_small,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 4L, outcome = "score1", predictors = "score2"),
  small_5  = clusterBootstrap(df_small,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 5L, outcome = "score1", predictors = "score2"),
  small_6  = clusterBootstrap(df_small,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 6L, outcome = "score1", predictors = "score2"),
  small_7  = clusterBootstrap(df_small,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 7L, outcome = "score1", predictors = "score2"),
  small_8  = clusterBootstrap(df_small,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 8L, outcome = "score1", predictors = "score2"),
  small_9  = clusterBootstrap(df_small,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 9L, outcome = "score1", predictors = "score2"),
  medium_1 = clusterBootstrap(df_medium, clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 1L, outcome = "score1", predictors = "score2"),
  medium_2 = clusterBootstrap(df_medium, clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 2L, outcome = "score1", predictors = "score2"),
  medium_3 = clusterBootstrap(df_medium, clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 3L, outcome = "score1", predictors = "score2"),
  medium_4 = clusterBootstrap(df_medium, clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 4L, outcome = "score1", predictors = "score2"),
  medium_5 = clusterBootstrap(df_medium, clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 5L, outcome = "score1", predictors = "score2"),
  medium_6 = clusterBootstrap(df_medium, clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 6L, outcome = "score1", predictors = "score2"),
  medium_7 = clusterBootstrap(df_medium, clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 7L, outcome = "score1", predictors = "score2"),
  medium_8 = clusterBootstrap(df_medium, clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 8L, outcome = "score1", predictors = "score2"),
  medium_9 = clusterBootstrap(df_medium, clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 9L, outcome = "score1", predictors = "score2"),
  large_1  = clusterBootstrap(df_large,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 1L, outcome = "score1", predictors = "score2"),
  large_2  = clusterBootstrap(df_large,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 2L, outcome = "score1", predictors = "score2"),
  large_3  = clusterBootstrap(df_large,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 3L, outcome = "score1", predictors = "score2"),
  large_4  = clusterBootstrap(df_large,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 4L, outcome = "score1", predictors = "score2"),
  large_5  = clusterBootstrap(df_large,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 5L, outcome = "score1", predictors = "score2"),
  large_6  = clusterBootstrap(df_large,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 6L, outcome = "score1", predictors = "score2"),
  large_7  = clusterBootstrap(df_large,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 7L, outcome = "score1", predictors = "score2"),
  large_8  = clusterBootstrap(df_large,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 8L, outcome = "score1", predictors = "score2"),
  large_9  = clusterBootstrap(df_large,  clusters, replace, stat_fun = oob_mse, B = 500, oob = TRUE, ncores = 9L, outcome = "score1", predictors = "score2"),
  times = 25
)

print(bm_parallel, unit = "s")


####trying things out with prediction error

generateData <- function(nSchools, nClasses, nStudents){
  expand.grid(school  = paste0("S", seq_len(nSchools)),
              class   = paste0("C", seq_len(nClasses)),
              student = paste0("S", seq_len(nStudents))) |>
    mutate(score1 = rnorm(n()),
           score2 = rnorm(n()))
}

oob_mse <- function(inbag, oob, outcome, predictors){
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  fit     <- lm(formula, data = as.data.frame(inbag))
  
  inbag_pred <- predict(fit, newdata = inbag)
  oob_pred   <- predict(fit, newdata = oob)
  
  c(inbag_mse = mean((inbag[[outcome]] - inbag_pred)^2),
    oob_mse   = mean((oob[[outcome]]   - oob_pred)^2))
}

bootstrapError <- function(boot_obj, oob_stat) {
  stopifnot(inherits(boot_obj, "clusterBootstrap"))
  
  apparent_err <- as.numeric(boot_obj$estimates$originalEstimates[, oob_stat])
  oob_err      <- mean(boot_obj$estimates$bootstrapEstimates[, oob_stat])
  err_632      <- 0.368 * apparent_err + 0.632 * oob_err
  
  data.frame(apparent = apparent_err,
             oob      = oob_err,
             err_632  = err_632,
             row.names = oob_stat)
}

library(dplyr)

# ---- 1. Generate fake three-level data --------------------------------------
set.seed(42)
df <- generateData(nSchools  = 10,
                   nClasses  = 5,
                   nStudents = 20) |>
  arrange(school,class,student) #|>
  #slice(1:(n()-3))

# ---- 2. Check the data ------------------------------------------------------
str(df)
head(df)

# ---- 3. Run the cluster bootstrap -----------------------------------------
library(tictoc)
set.seed(42)
tic()
result <- clusterBootstrap(df          = df,
                           clusters    = c("school", "class", "student"),
                           replace     = c(TRUE, TRUE, TRUE),
                           stat_fun    = oob_mse,
                           B           = 1000000,
                           oob         = TRUE,
                           ncores      = 9L,
                           keepIndices = TRUE,
                           outcome     = "score1",
                           predictors  = "score2")
toc()
beepr::beep(5)
# ---- 4. Inspect the results -------------------------------------------------

# Original (apparent) estimates
result$estimates$originalEstimates

# Bootstrap standard errors
result$estimates$bootstrapSE

# Distribution of bootstrap estimates
summary(result$estimates$bootstrapEstimates)

# ---- 6. Compute prediction error estimates ----------------------------------
bootstrapError(result, oob_stat = "oob_mse")

# ---- 7. Visualise the bootstrap distribution --------------------------------
par(mfrow = c(1, 2))

hist(result$estimates$bootstrapEstimates$inbag_mse,
     main = "In-bag MSE",
     xlab = "MSE",
     col  = "steelblue",
     border = "white")
abline(v = result$estimates$originalEstimates$inbag_mse,
       col = "red", lwd = 2, lty = 2)

hist(result$estimates$bootstrapEstimates$oob_mse,
     main = "OOB MSE",
     xlab = "MSE",
     col  = "steelblue",
     border = "white")
abline(v = bootstrapError(result, oob_stat = "oob_mse")$err_632,
       col = "red", lwd = 2, lty = 2)

par(mfrow = c(1, 1))

inbag(result, 1)

nrows <- rep(0, 100000)
for(i in seq_len(100000)) nrows[i] <- nrow(oob(result, i))

mean(nrows)

mean(sapply(result$indices, function(idx) {
  length(setdiff(seq_len(nrow(df)), idx))
}))

result$
