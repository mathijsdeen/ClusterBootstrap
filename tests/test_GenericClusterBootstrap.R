set.seed(2025)

# Speeldata met drie niveaus: school → klas → leerling
n_school  <- 4
n_class   <- 4
n_student <- 5

demo <- expand.grid(
  school  = paste0("S", 1:n_school),
  class   = paste0("C", 1:n_class),
  student = paste0("P", 1:n_student)
) |>
  mutate(score1 = rnorm(n()),
         score2 = rnorm(n())) |>
  arrange(school, class, student) |>
  slice(1:(n() - 3))




boot_fun <- function(d) lm(score1 ~ score2, data = d)$coef
boot_fun2 <- function(d) matrix(c(a = mean(d$score1), b = mean(d$score2),
                                  c = mean(d$score1), d = mean(d$score2)),
                                nrow=4)

library(tictoc)
set.seed(1)
tic()
clusterBootstrap(df       = demo, 
                 clusters = c("school", "class", "student"),
                 replace  = c(TRUE, TRUE, TRUE),
                 stat_fun = boot_fun,
                 R        = 100)
toc()

#set.seed(1)
out1 <- clusterResample(
  demo,
  clusters = c("school", "class", "student"),
  replace  = c(TRUE, TRUE, TRUE)
) |>
  arrange(school, class, student)

#set.seed(1)
out2 <- clusterResampleDT(
  demo,
  clusters = c("school", "class", "student"),
  replace  = c(TRUE, TRUE, TRUE)
) |>
  arrange(school, class, student)

View(out1)
View(out2)

#####

set.seed(1)
ids <- unique(demo$school)
sampled_ids <- sample(ids, length(ids), replace=TRUE)
sampled <- data.table(sampled_ids)
setnames(sampled, "school")
sampled[, .row := .I]
sampled
merge(sampled, demo, by = "school")

