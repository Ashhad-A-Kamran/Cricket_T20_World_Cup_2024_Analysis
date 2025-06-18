

# How does powerplay runrate differ accross venues?

library(dplyr)
library(ggplot2)

# Filter and calculate powerplay run rates per innings per venue
pp_per_match <- powerplay_df %>%
  group_by(venue, match_id, innings) %>%
  summarise(pp_runs = sum(total_runs), .groups = "drop") %>%
  mutate(pp_runrate = pp_runs / 6) %>%
  mutate(
    city = case_when(
      grepl("Tarouba", venue, ignore.case = TRUE) ~ "Trinidad",
      grepl("Kingstown", venue, ignore.case = TRUE) ~ "St Vincent",
      grepl("Lauderhill", venue, ignore.case = TRUE) ~ "Florida",
      grepl("Gros Islet", venue, ignore.case = TRUE) ~ "St Lucia",
      grepl("Dallas", venue, ignore.case = TRUE) ~ "Dallas",
      grepl("Bridgetown", venue, ignore.case = TRUE) ~ "Barbados",
      grepl("New York", venue, ignore.case = TRUE) ~ "New York",
      grepl("Guyana", venue, ignore.case = TRUE) ~ "Guyana",
      grepl("Antigua", venue, ignore.case = TRUE) ~ "Antigua",
      TRUE ~ "Other"
    )
  )

# Set custom city order for visuals
pp_per_match$city <- factor(pp_per_match$city, levels = c(
  "Trinidad", "St Vincent", "Florida", "St Lucia",
  "Dallas", "Barbados", "New York", "Guyana", "Antigua"
))

# Bar chart - average run rate by city
venue_avg <- pp_per_match %>%
  group_by(city) %>%
  summarise(avg_pp_runrate = mean(pp_runrate), .groups = "drop")

ggplot(venue_avg, aes(x = city, y = avg_pp_runrate, fill = city)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Powerplay Run Rate by Venue (T20 WC 2024)",
    x = "Venue",
    y = "Run Rate (Runs per Over)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  guides(fill = "none")  # removes legend

# ANOVA - Does city significantly affect PP run rate?
anova_city <- aov(pp_runrate ~ city, data = pp_per_match)
summary(anova_city)  # Check p-value

# Tukey HSD post-hoc test for pairwise differences
TukeyHSD(anova_city)


###############################################################################

# Boxplot to show distribution of run rates per city
ggplot(pp_per_match, aes(x = city, y = pp_runrate, fill = city)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Powerplay Run Rates by Venue",
    x = "Venue (City)",
    y = "Run Rate (per Over)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  guides(fill = "none")
