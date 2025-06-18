

# Do teams batting first and second differ in their powerplay run rates,
# and how does the powerplay run rate influence the final innings score for each innings?

library(dplyr)
library(ggplot2)


# Summarize powerplay runs per team per innings
powerplay_df$total_runs <- powerplay_df$runs_of_bat + powerplay_df$extras
pp_summary <- powerplay_df %>%
  group_by(match_id, innings, batting_team) %>%
  summarise(pp_runs = sum(total_runs), .groups = "drop") %>%
  mutate(pp_runrate = pp_runs / 6)

# Final score per innings (from full dataset)
scores$total_runs <- scores$runs_of_bat + scores$extras

final_scores <- scores %>%
  group_by(match_id, innings, batting_team) %>%
  summarise(final_score = sum(total_runs), .groups = "drop")

# adding in final score column
merged <- merge(pp_summary, final_scores, by = c("match_id", "innings", "batting_team"))

# Batting order label
merged$innings_label <- ifelse(merged$innings == 1, "Batting First", "Batting Second")
merged$innings_label <- factor(merged$innings_label, levels = c("Batting First", "Batting Second"))


# T-test: Is the difference significant?
t_test_result <- t.test(pp_runrate ~ innings_label, data = merged)
print(t_test_result)


###############################################################################

# Boxplot: Powerplay Run Rate by Innings
ggplot(merged, aes(x = innings_label, y = pp_runrate, fill = innings_label)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Powerplay Run Rate by Innings",
    x = "Innings",
    y = "Powerplay Run Rate (Runs per Over)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plotting PPRR and final scores
ggplot(merged, aes(x = pp_runrate, y = final_score, color = innings_label)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Powerplay Run Rate vs Final Score",
    x = "Powerplay Run Rate (Runs per Over)",
    y = "Final Score",
    color = "Innings"
  ) +
  theme_minimal()


