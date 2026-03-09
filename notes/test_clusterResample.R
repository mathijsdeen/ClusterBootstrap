library(dplyr)
library(data.table)
library(microbenchmark)
library(ClusterBootstrap)
set.seed(2026)
n_school  <- 5
n_class   <- 3
n_student <- 8

demo <- expand.grid(
  school  = paste0("S", 1:n_school),
  class   = paste0("C", 1:n_class),
  student = paste0("P", 1:n_student)) |>
  mutate(score1 = rnorm(n()),
         score2 = rnorm(n())) |>
  arrange(school, class, student) |>
  slice(1:(n() - 3)) # slightly unbalanced data

View(demo)

set.seed(1)
res_package <- ClusterBootstrap::clusterResample(data.table(demo), c("school","class","student"), 
                                                 replace = c(TRUE, TRUE, TRUE)) #|>
  #arrange(school, class, student) 
set.seed(1)
res_base <- clusterResample2(demo, c("school","class","student"), 
                             replace = c(TRUE, TRUE, TRUE)) #|> 
  #arrange(school, class, student) 
set.seed(1)
res_dplyr <- clusterResample3(demo, c("school","class","student"), 
                              replace = c(TRUE, TRUE, TRUE))
library(Rcpp)
sourceCpp("src/clusterResample.cpp")
set.seed(1)
res_cpp <- clusterResample_cpp(demo, c("school","class","student"), 
                               replace = c(TRUE, TRUE, TRUE))
setdiff(res_package, res_base)

all.equal(as.data.frame(res_package), as.data.frame(res_base))

generateData <- function(nSchools, nClasses, nStudents){
  expand.grid(school  = paste0("S", seq_len(nSchools)),
              class   = paste0("C", seq_len(nClasses)),
              student = paste0("S", seq_len(nStudents))) |>
    mutate(score1 = rnorm(n()),
           score2 = rnorm(n()))
}

set.seed(1)
dfSmall  <- generateData( 5,  4, 10)
dfMedium <- generateData(10,  8, 20)
dfLarge  <- generateData(20, 100, 5)

clusters <- c("school","class","student")
replace  <- c(TRUE, TRUE, FALSE)

bm <- microbenchmark(packSmall   =  clusterResample(dfSmall,  clusters, replace),
                     baseSmall   = clusterResample2(dfSmall,  clusters, replace),
                     dplyrSmall  = clusterResample3(dfSmall,  clusters, replace),
                     cppSmall    = clusterResample_cpp(dfSmall,  clusters, replace),
                     packMedium  =  clusterResample(dfMedium, clusters, replace),
                     baseMedium  = clusterResample2(dfMedium, clusters, replace),
                     dplyrMedium = clusterResample3(dfMedium, clusters, replace),
                     cppMedium    = clusterResample_cpp(dfMedium,  clusters, replace),
                     packLarge   =  clusterResample(dfLarge, clusters, replace),
                     baseLarge   = clusterResample2(dfLarge, clusters, replace),
                     dplyrLarge  = clusterResample3(dfLarge, clusters, replace),
                     cppLarge    = clusterResample_cpp(dfLarge,  clusters, replace),
                     times = 10)

print(bm, unit = "ms")


#################################
# non numeric cluster variables #
#################################

library(data.table)

# Build a dataset where cluster variables are of different types
df_types <- expand.grid(
  school  = factor(paste0("S", 1:3), levels = paste0("S", 1:3)),  # factor
  class   = as.integer(1:4),                                        # integer
  student = paste0("ST", 1:5)                                       # character
) |> transform(
  score1 = rnorm(60),
  score2 = rnorm(60)
)

df_types <- data.frame(
  school  = factor(c(rep("S1", 40), rep("S2", 15), rep("S3", 5)),
                   levels = c("S1", "S2", "S3", "S4")),  # S4 exists as a level but has NO rows at all
  class   = as.integer(c(rep(1:4, 10), rep(1:3, 5), rep(1:2, 2), 1)),
  student = paste0("ST", 1:60),
  score1  = rnorm(60),
  score2  = rnorm(60)
)


# Confirm the imbalance
table(df_types$school, useNA = "ifany")



# Check the types going in
str(df_types)

# Run both implementations with the same seed
set.seed(1)
res_dt   <- clusterResample(df_types, 
                            clusters = c("school", "class", "student"), 
                            replace  = c(TRUE, TRUE, TRUE))

set.seed(1)
res_base <- clusterResample2(df_types, 
                             clusters = c("school", "class", "student"), 
                             replace  = c(TRUE, TRUE, TRUE))

# Check that types are preserved in the output
str(res_dt)
str(res_base)

# Check factor levels are intact
levels(res_dt$school)
levels(res_base$school)

# Check no NAs were introduced
anyNA(res_dt)
anyNA(res_base)

table(res_dt$school)
table(res_base$school)
table(df_types$school)

# Run many resamples and check that factor levels are always preserved
all_levels_preserved <- replicate(500, {
  res <- clusterResample2(df_types, 
                          clusters = c("school", "class", "student"),
                          replace  = c(TRUE, TRUE, TRUE))
  identical(levels(res$school), levels(df_types$school))
})

all(all_levels_preserved)  # should be TRUE

# Also check no NAs ever introduced across many resamples
no_nas <- replicate(500, {
  res <- clusterResample2(df_types,
                          clusters = c("school", "class", "student"),
                          replace  = c(TRUE, TRUE, TRUE))
  !anyNA(res)
})

all(no_nas)  # should be TRUE
