
test_new <- test[,-c(3,4)]
test_new$ClaimInd<-as.factor(test_new$ClaimInd)

test_new$pred_claim_amount<-pred_seq_gam_db[,1]

mean_predictions <- test_new %>%
  group_by(interaction(Gender,MariStat)) %>%
  summarise(MeanPrediction = mean(pred_claim_amount, na.rm = TRUE))

# Step 2: Calculate standard error and confidence intervals manually
summary_df <- test_new %>%
  group_by(interaction(Gender,MariStat)) %>%
  do({
    data_group <- .
    n_size <- nrow(data_group)
    mean_pred <- mean(data_group$pred_claim_amount, na.rm = TRUE)
    std_error <- sd(data_group$pred_claim_amount, na.rm = TRUE) / sqrt(n_size)
    lower_ci <- mean_pred - (std_error * 1.96)
    upper_ci <- mean_pred + (std_error * 1.96)
    
    data.frame(MeanPrediction = mean_pred, StdError = std_error, Lower = lower_ci, Upper = upper_ci)
  })

library(ggplot2)

summary_df$`interaction(Gender, MariStat)`

ggplot(summary_df) +
  # Points with increased size and color differentiation
  geom_point(mapping=aes(x=`interaction(Gender, MariStat)`, y=MeanPrediction, color=`interaction(Gender, MariStat)`), size=4, alpha=0.8) +
  
  # Error bars with adjusted width and color
  geom_errorbar(mapping=aes(x=`interaction(Gender, MariStat)`, y=MeanPrediction, ymin=Lower, ymax=Upper), 
                width=0.2, color="darkgray", size=0.8) +
  
  # Customizing the plot with labels and title
  labs(x = "interaction(Gender,MariStat)", 
       y = "Mean Prediction", 
       title = "Mean claim amount by Gender",
       subtitle = "Includes 95% confidence intervals") +
  
  # Theme customization for a cleaner look
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",  # Remove legend if color differentiation by Gender is not needed
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(vjust = -0.2),
        axis.title.y = element_text(vjust = 1.2)) +
  
  # Optional: Customize colors manually if desired
  scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e"))

abe <- cbind(test_new,pred_seq_gam_db)

pred_full <- data.frame( prediction = seq_gam$predict(test_task)$response)

abe <- cbind(test_new,pred_seq_gam_db)

abe$model <- "unbiased"

abe2 <- cbind(test_new,pred_full)

abe2$model <- "biased"

abe3 <- rbind(abe,abe2)

ggplot(abe3)+
  geom_boxplot(mapping = aes(y=prediction, x=interaction(Gender, MariStat),fill = model))

rforest <- readRDS("modeller/random_forest_trained")

pred_rforest <- data.frame(prediction = rforest$predict(test_task)$response)

abe4 <- cbind(test_new, pred_rforest)

abe4$model <- "rforest"

abe5 <- rbind(abe3,abe4)

ggplot(abe5)+
  geom_boxplot(mapping = aes(y=prediction, x=interaction(Gender, MariStat),fill = model))+
  ylim(min(abe5$prediction),max(abe3$prediction))
