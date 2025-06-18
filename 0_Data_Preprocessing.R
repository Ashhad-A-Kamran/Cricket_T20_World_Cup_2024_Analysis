library(dplyr)
library(readxl)

winners <- read_excel("T20_match_winners.xlsx")
scores <- read_excel("T20_Scores.xlsx")

# Keeping only matches with exactly 2 innings
valid_matches <- scores %>%
  group_by(match_id) %>%
  summarise(innings_count = n_distinct(innings), .groups = "drop") %>%
  filter(innings_count == 2) %>%
  pull(match_id)

scores <- scores %>% filter(match_id %in% valid_matches)

# Filter for powerplay overs (first 6)
powerplay_df <- scores %>% filter(over < 6)

# Removing duplicate entry for match_id 202453
powerplay_df <- powerplay_df %>%
  filter(!(match_id == 202453 & phase == "Super 8 Group 1"))
View(powerplay_df)


# getting valid matches from the winners dataset
winners <- readxl::read_excel("T20_match_winners.xlsx")
valid_matches <- unique(powerplay_df$match_id)
winners_df <- winners %>% filter(match_id %in% valid_matches)
View(winners_df)


# Quick check
powerplay_ball_count <- powerplay_df %>%
  group_by(match_id, innings) %>%
  summarise(powerplay_ball_count = n(), .groups = 'drop')
View(powerplay_df)
print(powerplay_ball_count)