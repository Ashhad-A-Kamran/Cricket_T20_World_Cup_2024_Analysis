

# Correlation of number of bowlers to the outcome of the match

cor_bowlers_pprr <- cor(powerplay_summary$unique_bowlers, powerplay_summary$PPRR, use = "complete.obs")
cor_bowlers_win <- cor(powerplay_summary$unique_bowlers, powerplay_summary$winner_flag, use = "complete.obs")
print(paste("Corr: Unique Bowlers vs PPRR =", round(cor_bowlers_pprr, 3)))
print(paste("Corr: Unique Bowlers vs Winning =", round(cor_bowlers_win, 3)))


# Plot
ggplot(powerplay_summary, aes(x = unique_bowlers, y = winner_flag)) +
  geom_jitter(width = 0.2, height = 0.02, alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(
    title = "Correlation between Number of Bowlers and Match Outcome",
    x = "Number of Unique Bowlers in Powerplay",
    y = "Match Outcome (1 = Won, 0 = Lost)"
  ) +
  theme_minimal()