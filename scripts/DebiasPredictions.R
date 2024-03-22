
dir("utils", full.names = T) |>
  sort(decreasing = T) |>
  lapply(source)

loadData()

seq_ranger <- readRDS("sequential_ranger_trained")

test_task <- as_task_regr(test[,-c(3,4)], target = "ClaimAmount")

test2 <- test[,-c(3,4)]

test2$Gender <- as.factor(levels(test2$Gender)[1])

test2$MariStat <- as.factor(levels(test2$MariStat)[1])

task11 <- as_task_regr(test2, target = "ClaimAmount")

test2 <- test[,-c(3,4)]

test2$Gender <- as.factor(levels(test2$Gender)[1])

test2$MariStat <- as.factor(levels(test2$MariStat)[2])

task12 <- as_task_regr(test2, target = "ClaimAmount")

test2 <- test[,-c(3,4)]

test2$Gender <- as.factor(levels(test2$Gender)[2])

test2$MariStat <- as.factor(levels(test2$MariStat)[1])

task21 <- as_task_regr(test2, target = "ClaimAmount")

test2 <- test[,-c(3,4)]

test2$Gender <- as.factor(levels(test2$Gender)[2])

test2$MariStat <- as.factor(levels(test2$MariStat)[2])

task22 <- as_task_regr(test2, target = "ClaimAmount")

rpred11 <- seq_ranger$predict(task11)

rpred12 <- seq_ranger$predict(task12)

rpred21 <- seq_ranger$predict(task21)

rpred22 <- seq_ranger$predict(task22)

rpred_de_bias_df <- cbind(rpred11$response,rpred12$response,rpred21$response,rpred22$response)

rpred_seq_ranger_db <- rowMeans(rpred_de_bias_df)

boxplot(rpred_seq_ranger_db)

seq_gam <- readRDS("modeller/seq_gam_lrn.rds")

gpred11 <- seq_gam$predict(task11)

gpred12 <- seq_gam$predict(task12)

gpred21 <- seq_gam$predict(task21)

gpred22 <- seq_gam$predict(task22)

gpred_de_bias_df <- cbind(gpred11$response,gpred12$response,gpred21$response,gpred22$response)

pred_seq_gam_db <- rowMeans(gpred_de_bias_df)

boxplot(pred_seq_gam_db)

