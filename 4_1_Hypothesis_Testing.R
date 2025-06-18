




library(dplyr)
library(ggplot2)
library(scales) # For percent_format
# Filter out any rows where PPRR or winner_flag is NA from the main powerplay_summary

View(powerplay_summary)
powerplay_summary <- powerplay_summary %>%
  filter(!is.na(PPRR) & !is.na(winner_flag))

if (nrow(powerplay_summary) < 10) {
  stop("Not enough valid data in powerplay_summary to proceed after NA filtering.")
}

# Create PPRR categories using quantiles (e.g., 3 groups: Low, Medium, High)
pprr_quantiles <- quantile(powerplay_summary$PPRR, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)


powerplay_summary <- powerplay_summary %>%
  mutate(
    PPRR_conceded_cat = factor(cut(PPRR,
                                   breaks = unique(pprr_quantiles),
                                   labels = c("Low PPRR Conceded (Good Bowling)",
                                              "Medium PPRR Conceded",
                                              "High PPRR Conceded (Poor Bowling)"),
                                   include.lowest = TRUE,
                                   ordered_result = TRUE),
                               levels = c("Low PPRR Conceded (Good Bowling)",
                                          "Medium PPRR Conceded",
                                          "High PPRR Conceded (Poor Bowling)"))
  )


# --- 3. Calculate Win Proportions for Each PPRR Group ---
# We are looking at the win chance of the BOWLING team.
win_summary_by_pprr_cat <- powerplay_summary %>%
  filter(!is.na(PPRR_conceded_cat)) %>% # Ensure category is not NA
  group_by(PPRR_conceded_cat) %>%
  summarise(
    total_innings = n(),
    bowling_team_wins = sum(winner_flag),
    bowling_team_win_prop = ifelse(total_innings > 0, bowling_team_wins / total_innings, 0),
    .groups = "drop"
  )

cat("\nWin Proportions by PPRR Conceded Category (Bowling Team Perspective):\n")
print(win_summary_by_pprr_cat)

# --- 4. Statistical Test: Do win proportions differ across groups? ---
# We'll use a Chi-squared test of independence or Fisher's Exact Test.
# H0: The proportion of wins (for the bowling team) is the same across all PPRR_conceded_cat groups.
# H1: The proportion of wins differs for at least one PPRR_conceded_cat group.

cat("\nPerforming Association Test (PPRR Category vs. Bowling Team Win):\n")
# Create a contingency table: PPRR_conceded_cat vs. winner_flag
contingency_table_main <- table(powerplay_summary$PPRR_conceded_cat, powerplay_summary$winner_flag,
                                dnn = c("PPRR_Conceded_Category", "Bowling_Team_Outcome (0=Lost, 1=Won)"))

# Remove rows/columns that are all zeros to avoid issues with tests
contingency_table_main_filtered <- contingency_table_main[rowSums(contingency_table_main) > 0, colSums(contingency_table_main) > 0, drop = FALSE]

print("Contingency Table:")
print(contingency_table_main_filtered)

association_test_result_q <- NULL 
if (nrow(contingency_table_main_filtered) < 2 || ncol(contingency_table_main_filtered) < 2) {
  cat("Contingency table dimensions too small for a meaningful test after filtering.\n")
} else {
  expected_counts_check <- tryCatch(chisq.test(contingency_table_main_filtered)$expected, error = function(e) NULL)
  
  if (!is.null(expected_counts_check) && any(expected_counts_check < 5)) {
    cat("Using Fisher's Exact Test due to small expected cell counts.\n")
    association_test_result_q <- fisher.test(contingency_table_main_filtered, simulate.p.value = TRUE, B = 10000)
  } else if (!is.null(expected_counts_check)) {
    cat("Using Pearson's Chi-squared Test.\n")
    association_test_result_q <- chisq.test(contingency_table_main_filtered, correct = FALSE)
  } else {
    cat("Could not determine appropriate test due to issues with contingency table structure.\n")
  }
  
  if (!is.null(association_test_result_q)) {
    print(association_test_result_q)
  }
}

# --- 5. Visualization (Using Cleaned Plot Code) ---
# Bar plot of win percentages for each PPRR group

# Add confidence intervals to win_summary for plotting
win_summary_by_pprr_cat_ci <- win_summary_by_pprr_cat %>%
  mutate(
    lower_ci = NA_real_,
    upper_ci = NA_real_
  )

for(i in 1:nrow(win_summary_by_pprr_cat_ci)) {
  if (win_summary_by_pprr_cat_ci$total_innings[i] > 0 &&
      win_summary_by_pprr_cat_ci$bowling_team_wins[i] >= 0 &&
      win_summary_by_pprr_cat_ci$bowling_team_wins[i] <= win_summary_by_pprr_cat_ci$total_innings[i]) {
    prop_test_res <- tryCatch(prop.test(win_summary_by_pprr_cat_ci$bowling_team_wins[i],
                                        win_summary_by_pprr_cat_ci$total_innings[i]),
                              error = function(e) NULL)
    if (!is.null(prop_test_res)) {
      win_summary_by_pprr_cat_ci$lower_ci[i] <- prop_test_res$conf.int[1]
      win_summary_by_pprr_cat_ci$upper_ci[i] <- prop_test_res$conf.int[2]
    }
  }
}
win_summary_by_pprr_cat_ci <- win_summary_by_pprr_cat_ci %>%
  mutate(
    lower_ci = ifelse(is.na(lower_ci), 0, pmax(0, lower_ci)),
    upper_ci = ifelse(is.na(upper_ci), 1, pmin(1, upper_ci))
  )


plot_win_prop_by_pprr_cat_clean <- ggplot(win_summary_by_pprr_cat_ci,
                                          aes(x = PPRR_conceded_cat,
                                              y = bowling_team_win_prop,
                                              fill = PPRR_conceded_cat)) +
  geom_col(alpha = 0.9, width = 0.65) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                width = 0.2, color = "gray20", linewidth = 0.6,
                na.rm = TRUE) +
  geom_text(aes(label = paste0(scales::percent(bowling_team_win_prop, accuracy = 1))),
            vjust = -0.5, size = 3.75, color = "black", fontface = "bold",
            position = position_nudge(y = 0.02),
            na.rm = TRUE) +
  geom_text(aes(label = paste0("n=", total_innings)),
            vjust = 1.5, size = 3.0, color = "gray40",
            position = position_nudge(y = -0.01),
            na.rm = TRUE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(win_summary_by_pprr_cat_ci$upper_ci, win_summary_by_pprr_cat_ci$bowling_team_win_prop, na.rm=TRUE) * 1.15),
                     breaks = seq(0, 1, 0.2), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("< 7 )" = "#7FC97F",
                               "7-9" = "#FDBF6F",
                               "> 9" = "#EF5350")) +
  labs(
    title = "Bowling Team Win %",
    subtitle = paste("Based on PPRR Conceded by Bowling Team.",
                     if(!is.null(association_test_result_q) && !is.null(association_test_result_q$p.value)) {
                       paste0("p-value: ", scales::pvalue(association_test_result_q$p.value, accuracy = 0.001, add_p = TRUE))
                     } else {""}),
    x = NULL,
    y = "Bowling Team Win %"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b=5)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30", margin = margin(b=15)),
    axis.text.x = element_text(size = 9.5, face="bold"),
    axis.title.y = element_text(size = 11, face="bold", margin = margin(r=10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 15, 10, 10)
  )

print(plot_win_prop_by_pprr_cat_clean)



# PPWL and chance of winning

powerplay_summary$PPWL_group <- cut(powerplay_summary$PPWL,
                                    breaks = c(-1, 0, 2, 6),  # e.g., 0, 1-2, 3+
                                    labels = c("0", "1-2", "3+"))

# Contingency table
table_ppwl <- table(powerplay_summary$PPWL_group, powerplay_summary$winner_flag)

# Chi-squared test
chisq.test(table_ppwl)

contingency_table <- table(powerplay_summary$PPWL_group, powerplay_summary$winner_flag)

# Step 3: Print the table with labels
dimnames(contingency_table) <- list(
  "PPWL Group" = c("0", "1-2", "3+"),
  "Match Outcome (0 = Lost, 1 = Won)" = c("Lost", "Won")
)

print(contingency_table)