

# Predicting percentage of winning for a team if they use 6 unique bowlers in PP

logit_model <- glm(winner_flag ~ unique_bowlers, data = powerplay_summary, family = "binomial")

# Bowlers = 6
new_data <- data.frame(unique_bowlers = c(6))
new_data$win_probability <- predict(logit_model, newdata = new_data, type = "response")

print("Win Probability Prediction:")
print(new_data)


# Plotting the win probability
ggplot(new_data, aes(x = factor(unique_bowlers), y = win_probability)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_text(aes(label = round(win_probability, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Predicted Win Probability for 6 Bowlers",
    x = "Number of Unique Bowlers in Powerplay",
    y = "Win Probability"
  ) +
  ylim(0, 1) +
  theme_minimal()


