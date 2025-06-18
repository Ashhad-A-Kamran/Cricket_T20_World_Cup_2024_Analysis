

# Logistic Regression Plot: Predicted Win Probability by PPRR and PWL

# 
logistic_model <- glm(winner_flag ~ PPRR + PPWL, data = powerplay_summary, family = binomial)
summary(logistic_model)

pp_wickets_levels <- sort(unique(powerplay_summary$PPWL))
pp_wickets_levels <- pp_wickets_levels[pp_wickets_levels <= 4]

# Check if pp_wickets_levels is empty or has NA only
if(length(pp_wickets_levels) == 0 || all(is.na(pp_wickets_levels))){
  warning("No valid levels for Powerplay_Wickets_Lost.")
  
} else {
  new_data_logistic <- expand.grid(
    PPRR = seq(min(powerplay_summary$PPRR, na.rm = TRUE), max(powerplay_summary$PPRR, na.rm = TRUE), length.out = 50),
    PPWL = pp_wickets_levels
  )
  
  new_data_logistic$predicted_prob_win <- predict(logistic_model, newdata = new_data_logistic, type = "response")
  # a
  ggplot(new_data_logistic, aes(x = PPRR, y = predicted_prob_win, color = as.factor(PPWL))) +
    geom_line(size = 1.2) +
    labs(
      title = "Logistic Model: Predicted Win Probability (Bowling Team)",
      subtitle = "Based on Powerplay Runs Conceded and Wickets Taken",
      x = "PPRR (Runs Conceded by Bowling Team in Powerplay)",
      y = "Predicted Win Probability for Bowling Team",
      color = "Wickets Taken\nin Powerplay"
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = "top")
}


# b) Predicted Probabilities vs. Wickets Taken, faceted by PPRR (runs conceded)
# For this, it's better to pick a few illustrative PPRR values
pprr_levels_viz <- quantile(powerplay_summary$PPRR, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

if(length(pp_wickets_levels) == 0 || all(is.na(pp_wickets_levels))){
  warning("No valid levels for Powerplay_Wickets_Lost. Skipping plot.")
} else {
  new_data_logistic_wickets <- expand.grid(
    PPRR = pprr_levels_viz,
    PPWL = pp_wickets_levels
  )
  new_data_logistic_wickets$predicted_prob_win <- predict(logistic_model, newdata = new_data_logistic_wickets, type = "response")
  
  ggplot(new_data_logistic_wickets, aes(x = PPWL, y = predicted_prob_win, color = as.factor(round(PPRR,1)))) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Logistic Model: Predicted Win Probability (Bowling Team)",
      subtitle = "Based on Powerplay Wickets Taken, for different Runs Conceded levels",
      x = "Powerplay Wickets Taken by Bowling Team",
      y = "Predicted Win Probability for Bowling Team",
      color = "PPRR Conceded\n(Approx.)"
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = pp_wickets_levels) +
    theme(legend.position = "top")
}


# c) Distribution of Predicted Probabilities for Actual Wins vs. Losses
powerplay_summary$predicted_prob_win <- predict(logistic_model, newdata = powerplay_summary, type = "response")

ggplot(powerplay_summary, aes(x = predicted_prob_win, fill = factor(winner_flag))) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Distribution of Predicted Win Probabilities by Actual Outcome",
    subtitle = "Bowling Team's Perspective",
    x = "Predicted Win Probability for Bowling Team",
    y = "Density",
    fill = "Bowling Team Won Match"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "salmon", "1" = "skyblue"), labels = c("0" = "Lost", "1" = "Won")) +
  scale_x_continuous(labels = scales::percent_format())


# d) Actual vs. Predicted Probability (as you had, with slight tweaks for clarity)
ggplot(powerplay_summary, aes(x = predicted_prob_win, y = as.numeric(winner_flag))) +
  geom_jitter(height = 0.05, width = 0.01, alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "red", linetype="dashed") +
  labs(
    title = "Logistic Model: Actual Outcome vs. Predicted Win Probability",
    subtitle = "Bowling Team's Perspective",
    x = "Predicted Win Probability for Bowling Team",
    y = "Actual Win Flag (1 = Won, 0 = Lost)"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(breaks = c(0,1))