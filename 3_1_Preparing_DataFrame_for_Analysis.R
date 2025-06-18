

# During the powerplay, if a team uses many different bowlers, 
# does this affect their chance of winning?


library(dplyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)

# powerplay_df : cleaned ball-by-ball dataframe with only 6 overs from start



# getting no of bowlers, batsmen, and their match-ups
powerplay_summary <- powerplay_df %>%
  group_by(match_id, innings) %>%
  summarise(
    unique_bowlers = n_distinct(bowler),
    total_runs = sum(runs_of_bat + coalesce(extras, 0), na.rm = TRUE),
    PPRR = total_runs / 6, # exactly 6 overs in PP
    .groups = "drop"
  )

View(powerplay_summary)


# plotting boxplot with X= number_of_bowlers; 
#                       y= PPRR
ggplot(powerplay_summary, aes(x = factor(unique_bowlers), y = PPRR)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Distribution of Powerplay Run Rates by Number of Bowlers Used",
    x = "Number of Unique Bowlers in Powerplay",
    y = "Powerplay Run Rate (Runs per Over)"
  ) +
  theme_minimal()


# Extracting batting teams from innings 1 and 2
batting_teams_1 <- powerplay_df %>%
  filter(innings == 1) %>%
  select(match_id, batting_team) %>%
  distinct()
batting_teams_2 <- powerplay_df %>%
  filter(innings == 2) %>%
  select(match_id, batting_team) %>%
  distinct()
batting_teams <- bind_rows(
  batting_teams_1 %>% mutate(innings = 1),
  batting_teams_2 %>% mutate(innings = 2)
)
# Joining powerplay_summary to assign batting_team
powerplay_summary <- powerplay_summary %>%
  left_join(batting_teams, by = c("match_id", "innings"))


# Extracting bowlers from innings 1 and 2
bowling_teams_1 <- powerplay_df %>%
  filter(innings == 1) %>%
  select(match_id, bowling_team) %>%
  distinct() %>%
  mutate(innings = 1)
bowling_teams_2 <- powerplay_df %>%
  filter(innings == 2) %>%
  select(match_id, bowling_team) %>%
  distinct() %>%
  mutate(innings = 2)
# Combine bowling teams
bowling_teams <- bind_rows(bowling_teams_1, bowling_teams_2)
# Join into powerplay_summary
powerplay_summary <- powerplay_summary %>%
  left_join(bowling_teams, by = c("match_id", "innings"))


# Adding a column called winners where winners is a `CHAR` (in the form, USA, PAK  etc)
powerplay_summary <- powerplay_summary %>%
  left_join(winners_df %>% select(match_id, winner = Winners), by = "match_id")

# making a winner flag for each inning, so if bowling_team == winner; winner_flag = 1
powerplay_summary <- powerplay_summary %>%
  mutate(winner_flag = ifelse(bowling_team == winner, 1, 0))



