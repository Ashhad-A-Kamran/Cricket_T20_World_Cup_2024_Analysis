
# Linear Regression Models 

linear_model <- lm(Final_Team_Total ~ PPRR + Powerplay_Wickets_Lost, data = powerplay_summary)
summary(linear_model)


# Plotting

# Actual vs. Predicted Plot
powerplay_summary$predicted_FTT <- predict(linear_model, newdata = powerplay_summary)

ggplot(powerplay_summary, aes(x = Final_Team_Total, y = predicted_FTT)) +
  geom_point(alpha = 0.6, color = "dodgerblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Linear Model: Actual vs. Predicted Final Team Total",
    subtitle = paste0("R-squared: ", round(summary(linear_model)$r.squared, 3)),
    x = "Actual Final Team Total",
    y = "Predicted Final Team Total"
  ) +
  theme_minimal() +
  coord_fixed()

# Effect Plot for PPRR on Final_Team_Total (controlling for Powerplay_Wickets_Lost)

# This is an approximation as 'effects' does it more formally
mean_wickets <- mean(powerplay_summary$Powerplay_Wickets_Lost, na.rm = TRUE)
pprr_seq <- seq(min(powerplay_summary$PPRR, na.rm = TRUE), max(powerplay_summary$PPRR, na.rm = TRUE), length.out = 100)
pred_data_lm_pprr <- data.frame(PPRR = pprr_seq, Powerplay_Wickets_Lost = mean_wickets)
pred_data_lm_pprr$predicted_FTT <- predict(linear_model, newdata = pred_data_lm_pprr)

ggplot(pred_data_lm_pprr, aes(x = PPRR, y = predicted_FTT)) +
  geom_line(color = "forestgreen", size = 1.2) +
  geom_point(data = powerplay_summary, aes(x = PPRR, y = Final_Team_Total), alpha = 0.3, color = "gray50") +
  labs(
    title = "Predicted Final Team Total vs. PPRR",
    subtitle = paste("Holding Powerplay Wickets Lost at its mean (", round(mean_wickets,1), ")", sep=""),
    x = "Powerplay Run Rate (PPRR)",
    y = "Predicted Final Team Total"
  ) +
  theme_minimal()


# PLot
mean_pprr <- mean(powerplay_summary$PPRR, na.rm = TRUE)
wickets_seq <- sort(unique(powerplay_summary$Powerplay_Wickets_Lost)) # Use actual unique values
pred_data_lm_wickets <- data.frame(PPRR = mean_pprr, Powerplay_Wickets_Lost = wickets_seq)
pred_data_lm_wickets$predicted_FTT <- predict(linear_model, newdata = pred_data_lm_wickets)

ggplot(pred_data_lm_wickets, aes(x = Powerplay_Wickets_Lost, y = predicted_FTT)) +
  geom_line(color = "orangered", size = 1.2) +
  geom_point(aes(x = Powerplay_Wickets_Lost, y = predicted_FTT), color = "orangered", size = 3) + # Show predicted points
  geom_jitter(data = powerplay_summary, aes(x = Powerplay_Wickets_Lost, y = Final_Team_Total), alpha = 0.3, color = "gray50", width = 0.1) +
  labs(
    title = "Predicted Final Team Total vs. Powerplay Wickets Lost",
    subtitle = paste("Holding PPRR at its mean (", round(mean_pprr,1), ")", sep=""),
    x = "Powerplay Wickets Lost",
    y = "Predicted Final Team Total"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = wickets_seq)

