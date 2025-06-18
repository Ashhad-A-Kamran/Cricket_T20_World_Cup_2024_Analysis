



# How do both a team's Powerplay Run Rate (PPRR) and the number of wickets 
# they lose in the powerplay together influence their final score or their 
# chance of winning?


#install.packages("janitor")

library(dplyr)
library(ggplot2)


# Counting number of wickets lost in powerplay per match and innings
powerplay_wickets <- powerplay_df %>%
  filter(!is.na(player_dismissed)) %>%
  group_by(match_id, innings) %>%
  summarise(wickets_lost = n(), .groups = "drop")
# adding it to our powerplay_summary dataframe
powerplay_summary <- powerplay_summary %>%
  left_join(powerplay_wickets, by = c("match_id", "innings")) %>%
  mutate(PPWL = ifelse(is.na(wickets_lost), 0, wickets_lost)) %>%
  select(-wickets_lost)

# Calculating Final scores
final_scores <- scores %>%
  group_by(match_id, innings) %>%
  summarise(
    FTS = sum(runs_of_bat + coalesce(extras, 0), na.rm = TRUE),
    .groups = "drop"
  )
powerplay_summary <- powerplay_summary %>%
  left_join(final_scores, by = c("match_id", "innings"))

View(powerplay_summary)