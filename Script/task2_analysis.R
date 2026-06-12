# ==========================
# CAG DATA ANALYST TEST
# TEST 2 ANALYSIS
# ==========================

# --------------------------
# LOAD PACKAGES
# --------------------------

library(tidyverse)
library(janitor)
library(scales)
library(broom)

theme_set(
  theme_minimal(base_size = 12)
)

# --------------------------
# CHART THEME
# --------------------------

cag_theme <-
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 14
      ),
    plot.subtitle =
      element_text(
        size = 11
      ),
    axis.title =
      element_text(
        face = "bold"
      ),
    axis.text =
      element_text(
        colour = "black"
      ),
    legend.title =
      element_text(
        face = "bold"
      )
  )
# --------------------------
# IMPORT DATA
# --------------------------

debt <- read_csv(
  "data/Test 2 - Client Debt Data.csv"
)

# --------------------------
# CLEAN COLUMN NAMES
# --------------------------

debt <- clean_names(debt)

# --------------------------
# DATA QUALITY CHECKS
# --------------------------

cat("\nDATA QUALITY CHECKS\n")

cat(
  "\nDuplicate rows:",
  sum(duplicated(debt))
)

cat("\n\nMissing values\n")

print(
  colSums(is.na(debt))
)

# Remove duplicates

debt <- distinct(debt)

# --------------------------
# OUTPUT FOLDERS
# --------------------------

dir.create(
  "outputs",
  showWarnings = FALSE
)

dir.create(
  "outputs/task2",
  showWarnings = FALSE
)

dir.create(
  "outputs/task2/charts",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/task2/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

# ==================================================
# QUESTION 1
# DOES AVERAGE OUTSTANDING DEBT DIFFER BY REGION?
# ==================================================

debt_by_region <-
  debt %>%
  group_by(region) %>%
  summarise(
    mean_debt =
      mean(
        outstanding_debt,
        na.rm = TRUE
      ),
    median_debt =
      median(
        outstanding_debt,
        na.rm = TRUE
      ),
    sd_debt =
      sd(
        outstanding_debt,
        na.rm = TRUE
      ),
    n = n()
  ) %>%
  arrange(desc(mean_debt))

debt_by_region

write_csv(
  debt_by_region,
  "outputs/task2/tables/debt_by_region.csv"
)

# --------------------------
# MEAN DEBT BAR CHART
# --------------------------

mean_debt_chart <-
  debt_by_region %>%
  ggplot(
    aes(
      reorder(
        region,
        mean_debt
      ),
      mean_debt
    )
  ) +
  geom_col(
    fill = "#005A9C"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = dollar_format(
      prefix = "£"
    )
  ) +
  labs(
    title = "Average Outstanding Debt by Region",
    subtitle = "Comparison of mean debt levels",
    x = "",
    y = "Average Debt (£)"
  ) +
  cag_theme
mean_debt_chart

ggsave(
  "outputs/task2/charts/mean_debt_region.png",
  mean_debt_chart,
  width = 8,
  height = 5,
  dpi = 300
)

# --------------------------
# BOXPLOT
# --------------------------

boxplot_region_debt <-
  ggplot(
    debt,
    aes(
      region,
      outstanding_debt
    )
  ) +
  geom_boxplot(
    fill = "#005A9C",
    alpha = 0.7
  ) +
  scale_y_continuous(
    labels =
      dollar_format(
        prefix = "£"
      )
  ) +
  labs(
    title = "Outstanding Debt by Region",
    subtitle = "Distribution of debt across regions",
    x = "Region",
    y = "Outstanding Debt (£)"
  ) +
  cag_theme

boxplot_region_debt

ggsave(
  "outputs/task2/charts/debt_by_region_boxplot.png",
  boxplot_region_debt,
  width = 8,
  height = 5,
  dpi = 300
)

# --------------------------
# ANOVA
# --------------------------

anova_model <-
  aov(
    outstanding_debt ~ region,
    data = debt
  )
shapiro_test <-
  shapiro.test(
    residuals(anova_model)
  )

bartlett_test <-
  bartlett.test(
    outstanding_debt ~ region,
    data = debt
  )

capture.output(
  shapiro_test,
  file =
    "outputs/task2/tables/shapiro_test.txt"
)

capture.output(
  bartlett_test,
  file =
    "outputs/task2/tables/bartlett_test.txt"
)
anova_results <-
  summary(anova_model)

anova_results

anova_table <-
  broom::tidy(anova_model)

write_csv(
  anova_table,
  "outputs/task2/tables/anova_results.csv"
)

# --------------------------
# TUKEY POST-HOC TEST
# --------------------------

tukey_results <-
  TukeyHSD(
    anova_model
  )

tukey_results

tukey_table <-
  as.data.frame(
    tukey_results$region
  )

write_csv(
  tibble::rownames_to_column(
    tukey_table,
    "comparison"
  ),
  "outputs/task2/tables/tukey_results.csv"
)

# ==================================================
# QUESTION 2
# DOES THE PROPORTION OF CUSTOMERS
# WITH A DEBT ISSUE DIFFER BY REGION?
# ==================================================

debt <- debt %>%
  mutate(
    debt_issue =
      ifelse(
        issue_type == "Debt",
        "Debt Issue",
        "Other Issue"
      )
  )

# --------------------------
# COUNTS
# --------------------------

debt_proportion <-
  debt %>%
  group_by(
    region,
    debt_issue
  ) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )

debt_proportion

write_csv(
  debt_proportion,
  "outputs/task2/tables/debt_issue_counts.csv"
)

# --------------------------
# PERCENTAGES
# --------------------------

debt_percentages <-
  debt %>%
  group_by(region) %>%
  summarise(
    debt_issue_pct =
      round(
        mean(
          issue_type == "Debt"
        ) * 100,
        1
      )
  )

debt_percentages

write_csv(
  debt_percentages,
  "outputs/task2/tables/debt_issue_percentages.csv"
)

# --------------------------
# STACKED BAR CHART
# --------------------------

debt_issue_chart <-
  ggplot(
    debt,
    aes(
      region,
      fill = debt_issue
    )
  ) +
  geom_bar(
    position = "fill"
  ) +
  scale_fill_manual(
    values = c(
      "#005A9C",
      "#8CB4D9"
    )
  ) +
  scale_y_continuous(
    labels = percent
  ) +
  labs(
    title =
      "Proportion of Customers with Debt Issues",
    subtitle =
      "Comparison across regions",
    x = "Region",
    y = "Percentage"
  ) +
  cag_theme

debt_issue_chart

ggsave(
  "outputs/task2/charts/debt_issue_proportion.png",
  debt_issue_chart,
  width = 8,
  height = 5,
  dpi = 300
)

# --------------------------
# CHI-SQUARE TEST
# --------------------------

chi_table <-
  table(
    debt$region,
    debt$debt_issue
  )

chi_test <-
  chisq.test(
    chi_table
  )

chi_test

chi_summary <-
  broom::tidy(chi_test)

write_csv(
  chi_summary,
  "outputs/task2/tables/chisquare_results.csv"
)

# ==================================================
# QUESTION 3
# DOES REGION STILL MATTER
# WHEN INCOME IS HELD CONSTANT?
# ==================================================

# --------------------------
# SCATTERPLOT
# --------------------------

income_debt_plot <-
  ggplot(
    debt,
    aes(
      annual_income,
      outstanding_debt,
      colour = region
    )
  ) +
  geom_point(
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE
  ) +
  scale_y_continuous(
    labels =
      dollar_format(
        prefix = "£"
      )
  ) +
  scale_x_continuous(
    labels =
      dollar_format(
        prefix = "£"
      )
  ) +
  labs(
    title =
      "Income and Outstanding Debt by Region",
    subtitle =
      "Relationship between income and debt",
    x = "Annual Income (£)",
    y = "Outstanding Debt (£)"
  ) +
  cag_theme

income_debt_plot

ggsave(
  "outputs/task2/charts/income_vs_debt.png",
  income_debt_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# --------------------------
# MULTIPLE REGRESSION
# --------------------------

regression_model <-
  lm(
    outstanding_debt ~
      annual_income +
      region,
    data = debt
  )

summary(
  regression_model
)

capture.output(
  summary(
    regression_model
  ),
  file =
    "outputs/task2/tables/regression_results.txt"
)

# --------------------------
# REGRESSION COEFFICIENTS
# --------------------------

regression_model <-
  lm(
    outstanding_debt ~ annual_income + region,
    data = debt
  )

regression_summary <-
  tidy(
    regression_model
  )

write_csv(
  regression_summary,
  "outputs/task2/tables/regression_summary.csv"
)

# ==================================================
# EXECUTIVE SUMMARY TABLE
# ==================================================

summary_table <- tibble(
  Metric = c(
    "Number of Customers",
    "Average Debt",
    "Median Debt"
  ),
  Value = c(
    nrow(debt),
    round(
      mean(
        debt$outstanding_debt
      ),
      2
    ),
    median(
      debt$outstanding_debt
    )
  )
)

write_csv(
  summary_table,
  "outputs/task2/tables/summary_table.csv"
)

# ==================================================
# FINAL OUTPUT
# ==================================================

cat(
  "\nTASK 2 ANALYSIS COMPLETE\n"
)

cat(
  "\nAll charts and tables saved to outputs/task2 folder.\n"
)
