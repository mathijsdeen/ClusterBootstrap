# library(dplyr)
# 
# set.seed(2025)
# 
# # Speeldata met drie niveaus: school → klas → leerling
# n_school  <- 4 
# n_class   <- 4
# n_student <- 5
# 
# demo <- expand.grid(
#   school  = paste0("S", 1:n_school),
#   class   = paste0("C", 1:n_class),
#   student = paste0("P", 1:n_student)
# ) |>
#   mutate(score1 = rnorm(n()),
#          score2 = rnorm(n())) |>
#   arrange(school, class, student) |>
#   slice(1:(n() - 3))
# 
# boot_fun <- function(d) lm(score1 ~ score2, data = d)$coef
# boot_fun2 <- function(d) matrix(c(a = mean(d$score1), b = mean(d$score2),
#                                   c = mean(d$score1), d = mean(d$score2)),
#                                 ncol=4)
# library(tictoc)
# set.seed(1)
# tic()
# clusterBootstrap(df       = demo, 
#                  clusters = c("school", "class", "student"),
#                  replace  = c(TRUE, TRUE, TRUE),
#                  stat_fun = boot_fun,
#                  B        = 1000)
# toc()
# 
# #set.seed(1)
# out1 <- clusterResampleDT(
#   demo,
#   clusters = c("school", "class", "student"),
#   replace  = c(TRUE, TRUE, FALSE)
# ) #|>
#   #arrange(school, class, student)
# 
# clusterResample(demo, clusters = "student", replace = FALSE) |>
#   dplyr::arrange(school, class, student)
# 
# #set.seed(1)
# out2 <- clusterResampleDT(
#   demo,
#   clusters = c("school", "class", "student"),
#   replace  = c(TRUE, TRUE, TRUE)
# ) |>
#   arrange(school, class, student)
# 
# View(out1)
# View(out2)
# 
# #####
# 
# set.seed(1)
# ids <- unique(demo$school)
# sampled_ids <- sample(ids, length(ids), replace=TRUE)
# sampled <- data.table(sampled_ids)
# setnames(sampled, "school")
# sampled[, .row := .I]
# sampled
# merge(sampled, demo, by = "school")
# 
# #########
# 
# set.seed(2025)
# demo <- CJ(
#   school = paste0("S", 1:3),
#   class  = paste0("C", 1:3),
#   student = paste0("P", 1:4)
# )[
#   , `:=`(score1 = rnorm(.N), score2 = rnorm(.N))
# ]
# 
# clusterResample(
#   demo,
#   clusters = c("school", "class", "student"),
#   replace = c(TRUE, TRUE, FALSE)
# ) 
# 
# library(ClusterBootstrap)
# library(data.table)
# data("medication")
# medData <- medication |>
#   filter(time %% 1 == 0, time < 4)
# bootFun <- function(d) {
#   lm(pos ~ treat*time, data = d)$coefficients
# }
# 
# clusterResampleDT(medData, c("id", "time"), c(TRUE,FALSE)) |> View()
# library(tictoc)
# tic()
# ddd <- clusterBootstrap(medData, "id", TRUE, bootFun, 10)
# ddd
# toc()
# 
# tic()
# clusbootglm(pos ~ treat*time, medData, id, B = 10)
# toc()
# unique(medData$id)[61]
# 
# set.seed(2025)
# 
# demo <- CJ(
#   id = paste0("P", 1:100),
#   time = c("T1", "T2")
# )[
#   , y := rnorm(.N)
# ]
# 
# # Maak id = P61 slechts 1 rij (échte test op jouw bug)
# demo <- demo[!(id == "P61" & time == "T2")]
# 
# # Testfunctie: cluster op id (met teruglegging) en time (zonder)
# res <- clusterResampleDT(
#   demo,
#   clusters = c("id", "time"),
#   replace = c(TRUE, FALSE)
# )
# View(res)
# 
# # Controle: per id exact 1 rij per time
# res[, .N, by = .(id, time)][, .N, by = id][N != 2]
# res
