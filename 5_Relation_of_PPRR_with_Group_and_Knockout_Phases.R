

# Does the PPRR change for group stages versus semifinal/grand final?


powerplay_summary <- readxl::read_excel("powerplay_summary.xlsx")
# powerplay_df: from 0_Data_Preprocessing


powerplay_summary <- powerplay_df %>%
  group_by(match_id, innings) %>%
  summarise(
    PPRR = sum(runs_of_bat + coalesce(extras, 0), na.rm = TRUE) / 6,  # runs per over in powerplay
    bowling_team = first(bowling_team),
    phase = first(phase),
    .groups = "drop"
  ) %>%
  left_join(winners_df %>% select(match_id, winner = Winners), by = "match_id") %>%
  mutate(winner_flag = ifelse(!is.na(winner) & bowling_team == winner, 1, 0)) %>%  # mark if bowling team won
  filter(!is.na(winner_flag))


powerplay_summary <- powerplay_summary %>%
  mutate(stage_type = factor(case_when(
    phase %in% c("Group A", "Group B", "Group C", "Group D", "Super 8 Group 1", "Super 8 Group 2") ~ "Group Stage",
    phase %in% c("Semifinal 1", "Semifinal 2", "Final") ~ "Knockout Stage",
    TRUE ~ NA_character_
  ), levels = c("Group Stage", "Knockout Stage"))) %>%
  filter(!is.na(stage_type))  # dropping any weird phases we don’t care about


# Attach phase info from original datafram called powerplay_df
powerplay_summary <- powerplay_summary %>%
  left_join(
    powerplay_df %>%
      select(match_id, innings, phase) %>%
      distinct(),
    by = c("match_id", "innings")
  )

# Distributing PPRR into 3 categories: <7 |7-9| >9
powerplay_summary <- powerplay_summary %>%
  mutate(
    PPRR_cat = factor(cut(PPRR,
                          breaks = c(-Inf, 6.99, 9.00, Inf),
                          labels = c("<7 PPRR", "7-9 PPRR", ">9 PPRR"),
                          right = TRUE, include.lowest = TRUE),
                      levels = c("<7 PPRR", "7-9 PPRR", ">9 PPRR"))
  ) %>%
  filter(!is.na(PPRR_cat))
cat("Counts per PPRR category and stage:\n")
print(table(powerplay_summary$PPRR_cat, powerplay_summary$stage_type))



# Quick boxplot of PPRR by stage — eyeball the spread and means
print(
  ggplot(powerplay_summary, aes(x = stage_type, y = PPRR, fill = stage_type)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21) +
    geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      title = "Powerplay Run Rate (PPRR Conceded) by Match Stage",
      subtitle = "Runs conceded by bowling team during powerplay",
      x = "Match Stage",
      y = "Powerplay Run Rate (RPO)"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Pastel1")
)
# Tests -------------------------------------------------------------------

# Mann-Whitney U Test for group stage and knockout
mann_whitney_pprr_stage <- wilcox.test(PPRR ~ stage_type, data = powerplay_summary)
print("mann Whitney test for PPRR by Stage Type:")
print(mann_whitney_pprr_stage)

# Summarize wins by stage and PPRR category — checking where bowling teams succeed
win_summary <- powerplay_summary %>%
  group_by(stage_type, PPRR_cat) %>%
  summarise(
    matches = n(),
    wins = sum(winner_flag),
    win_percentage = ifelse(matches > 0, wins / matches, 0),
    .groups = "drop"
  )
cat("Win summary by stage and PPRR category:\n")
print(win_summary)


# Run Fisher's Exact Test for each stage to check association of PPRR category and winning
association_results_list <- list()

for (current_stage in levels(powerplay_summary$stage_type)) {
  cat("\n--- Fisher's Exact Test for:", current_stage, "---\n")
  df_subset <- filter(powerplay_summary, stage_type == current_stage)
  
  # Skip if data too sparse or categories missing
  if (nrow(df_subset) < 5 || n_distinct(df_subset$PPRR_cat) < 1 || n_distinct(df_subset$winner_flag) < 2) {
    cat("Not enough data or categories, skipping test.\n")
    association_results_list[[current_stage]] <- list(test = NULL, message = "Insufficient data")
    next
  }
  
  df_subset$PPRR_cat <- factor(df_subset$PPRR_cat, levels = levels(powerplay_summary$PPRR_cat))
  tbl <- table(df_subset$PPRR_cat, df_subset$winner_flag, dnn=c("PPRR_Cat", "Won_by_Bowling_Team"))
  tbl <- tbl[rowSums(tbl) > 0, , drop = FALSE]  # drop empty rows
  
  cat("Contingency table:\n")
  print(tbl)
  
  if(nrow(tbl) < 2 || ncol(tbl) < 2){
    cat("Table too small after filtering, skipping test.\n")
    association_results_list[[current_stage]] <- list(test = NULL, message = "Table too small")
    next
  }
  
  # Simulate p-value for bigger tables, classic otherwise
  fisher_result <- fisher.test(tbl, simulate.p.value = (nrow(tbl) > 2 || ncol(tbl) > 2), B = 10000)
  print(fisher_result)
  association_results_list[[current_stage]] <- list(test = fisher_result, table = tbl)
}


# Plot bowling team win percentage by PPRR category and phase (group or knockout)
print(
  ggplot(win_summary, aes(x = PPRR_cat, y = win_percentage, fill = stage_type)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    geom_text(aes(label = scales::percent(win_percentage, accuracy = 0.1)),
              position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Bowling Team Win % by PPRR Conceded & Match Stage",
      subtitle = "Runs conceded in powerplay vs. bowling team success",
      x = "PPRR Category (Runs Per Over)",
      y = "Win Percentage",
      fill = "Match Stage"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", axis.text.x = element_text(hjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")
)

# Saving dataframe and all the plots
# install.packages("openxlsx")
library(openxlsx)
write.xlsx(powerplay_summary, file = "C:/Users/Ashhad/Documents/powerplay_summary.xlsx", rowNames = FALSE)

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to="C:/Users/Ashhad/Documents")