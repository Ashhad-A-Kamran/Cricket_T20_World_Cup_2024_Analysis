
# Predicting PPRR of team which uses 6 bowlers in PP


lm_model <- lm(PPRR ~ unique_bowlers, data = powerplay_summary)

# Predict for 6 bowlers
new_data$predicted_PPRR <- predict(lm_model, newdata = new_data)

print("PPRR Prediction:")
print(new_data)

# PLot
ggplot(new_data, aes(x = factor(unique_bowlers), y = predicted_PPRR)) +
  geom_col(fill = "darkgreen", width = 0.5) +
  geom_text(aes(label = round(predicted_PPRR, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Predicted PPRR for 6 Bowlers",
    x = "Number of Unique Bowlers in Powerplay",
    y = "Predicted PPRR (Runs per Over)"
  ) +
  theme_minimal()