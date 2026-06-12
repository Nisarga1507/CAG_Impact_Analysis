# ==========================

# CAG DATA ANALYST TEST

# TEST 1 ANALYSIS

# ==========================

# --------------------------

# LOAD PACKAGES

# --------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(scales)

# --------------------------

# IMPORT DATA

# --------------------------

issues <- read_csv(
  "data/Test 1 - CAG Issues Data.csv"
)

outcomes <- read_csv(
  "data/Test 1 - CAG Outcomes Data.csv"
)

# --------------------------

# CLEAN COLUMN NAMES

# --------------------------

issues <- clean_names(issues)
outcomes <- clean_names(outcomes)

# --------------------------

# DATA CLEANING

# --------------------------

issues <- issues %>%
  mutate(date = dmy(date))

outcomes <- outcomes %>%
  mutate(date = dmy(date))

outcomes$total_value_num <-
  outcomes$total_value %>%
  str_remove_all("£") %>%
  str_remove_all(",") %>%
  as.numeric()

issues <- distinct(issues)

outcomes <- distinct(outcomes)

# --------------------------
# CREATE OUTPUT FOLDERS
# --------------------------

dir.create(
  "outputs",
  showWarnings = FALSE
)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/charts",
  recursive = TRUE,
  showWarnings = FALSE
)

# --------------------------

# DATA QUALITY CHECKS

# --------------------------

cat("\nDATA QUALITY CHECKS\n")

cat(
  "Duplicate issue rows:",
  sum(duplicated(issues)),
  "\n"
)

cat(
  "Duplicate outcome rows:",
  sum(duplicated(outcomes)),
  "\n"
)

cat("\nMissing values in Issues\n")
print(colSums(is.na(issues)))

cat("\nMissing values in Outcomes\n")
print(colSums(is.na(outcomes)))

# --------------------------
# FUNDER VALIDATION
# --------------------------

funder_issues_check <-
  issues %>%
  count(funder) %>%
  arrange(desc(n))

funder_outcomes_check <-
  outcomes %>%
  count(funder) %>%
  arrange(desc(n))

funder_issues_check
funder_outcomes_check

# --------------------------

# FILTER PROJECT

# --------------------------

project_name <-
  "Gateshead Council – Advice in Community Project"

issues_project <-
  issues %>%
  filter(
    funder == project_name
  )

outcomes_project <-
  outcomes %>%
  filter(
    funder == project_name
  )

cat(
  "\nIssues Project Records:",
  nrow(issues_project),
  "\n"
)

cat(
  "Outcomes Project Records:",
  nrow(outcomes_project),
  "\n"
)

# --------------------------

# FILTER 2025

# --------------------------

issues_2025 <-
  issues_project %>%
  filter(
    year(date) == 2025
  )

outcomes_2025 <-
  outcomes_project %>%
  filter(
    year(date) == 2025
  )

# --------------------------

# KPI 1 - NUMBER OF CLIENTS

# --------------------------

num_clients <-
  n_distinct(
    issues_2025$client_reference_number
  )

# --------------------------

# KPI 2 - NUMBER OF ISSUES

# --------------------------

num_issues <-
  nrow(issues_2025)

# --------------------------

# KPI 3 - ISSUES PER CLIENT

# --------------------------

issues_per_client <-
  num_issues / num_clients

cat(
  "\nAverage Issues Per Client:",
  round(issues_per_client, 2),
  "\n"
)

# --------------------------

# EXECUTIVE SUMMARY TABLE

# --------------------------

summary_table <- tibble(
  Metric = c(
    "Number of Clients",
    "Number of Issues",
    "Issues per Client"
  ),
  Value = c(
    num_clients,
    num_issues,
    round(
      issues_per_client,
      2
    )
  )
)

write_csv(
  summary_table,
  "outputs/tables/summary_metrics.csv"
)

# --------------------------

# KPI 4 - CLIENTS BY ISSUE AREA

# --------------------------

clients_by_issue_area <-
  issues_2025 %>%
  group_by(issue_part_1) %>%
  summarise(
    clients =
      n_distinct(
        client_reference_number
      )
  ) %>%
  arrange(desc(clients))

write_csv(
  clients_by_issue_area,
  "outputs/tables/clients_by_issue_area.csv"
)

# --------------------------

# KPI 5 - ISSUES BY ISSUE AREA

# --------------------------

issues_by_issue_area <-
  issues_2025 %>%
  group_by(issue_part_1) %>%
  summarise(
    issues = n()
  ) %>%
  arrange(desc(issues))


write_csv(
  issues_by_issue_area,
  "outputs/tables/issues_by_issue_area.csv"
)

top_issue_chart <-
  issues_by_issue_area %>%
  slice_max(
    issues,
    n = 10
  ) %>%
  ggplot(
    aes(
      reorder(issue_part_1, issues),
      issues
    )
  ) +
  geom_col(
    fill = "#005A9C"
  ) +
  coord_flip() +
  labs(
    title = "Top 10 Issue Areas",
    x = "",
    y = "Number of Issues"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title =
      element_text(face = "bold"),
    axis.title =
      element_text(face = "bold")
  )

ggsave(
  "outputs/charts/top_issue_areas.png",
  top_issue_chart,
  width = 8,
  height = 5
)

# --------------------------

# MONTHLY TREND

# --------------------------

monthly_issues <-
  issues_2025 %>%
  mutate(
    month =
      floor_date(
        date,
        "month"
      )
  ) %>%
  group_by(month) %>%
  summarise(
    issues = n()
  )
monthly_plot <-
  ggplot(
    monthly_issues,
    aes(month, issues)
  ) +
  geom_line(
    linewidth = 1.4,
    colour = "#005A9C"
  ) +
  geom_point(
    size = 3.5,
    colour = "#005A9C"
  ) +
  scale_x_date(
    date_labels = "%b"
  ) +
  labs(
    title = "Monthly Number of Issues (2025)",
    subtitle = "Advice in Community Project",
    x = "Month",
    y = "Number of Issues"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title =
      element_text(face = "bold"),
    axis.title =
      element_text(face = "bold")
  )

ggsave(
  "outputs/charts/monthly_issues_trend.png",
  monthly_plot,
  width = 8,
  height = 5
)

# --------------------------
# KPI 6 - FINANCIAL GAIN BY CATEGORY
# --------------------------

gain_by_category <-
  outcomes_2025 %>%
  group_by(
    financial_outcome_category
  ) %>%
  summarise(
    total_gain =
      sum(
        total_value_num,
        na.rm = TRUE
      )
  ) %>%
  arrange(desc(total_gain))

gain_chart <-
  gain_by_category %>%
  ggplot(
    aes(
      reorder(
        financial_outcome_category,
        total_gain
      ),
      total_gain
    )
  ) +
  geom_col(
    fill = "#005A9C"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Financial Gain by Outcome Category",
    x = "",
    y = "Financial Gain (£)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title =
      element_text(face = "bold")
  )

write_csv(
  gain_by_category,
  "outputs/tables/gain_by_category.csv"
)

ggsave(
  "outputs/charts/financial_gain_category.png",
  gain_chart,
  width = 8,
  height = 5
)
# --------------------------

# LOOKUP TABLE

# --------------------------

issue_lookup <-
  issues_2025 %>%
  select(
    issue_reference_number,
    parliamentary_constituency,
    local_authority
  )

# --------------------------

# KPI 7 - FINANCIAL GAIN BY CONSTITUENCY

# --------------------------

gain_constituency <-
  outcomes_2025 %>%
  left_join(
    issue_lookup,
    by =
      "issue_reference_number"
  ) %>%
  filter(
    local_authority ==
      "Gateshead"
  ) %>%
  group_by(
    parliamentary_constituency
  ) %>%
  summarise(
    total_gain =
      sum(
        total_value_num,
        na.rm = TRUE
      )
  ) %>%
  arrange(desc(total_gain))

write_csv(
  gain_constituency,
  "outputs/tables/gain_constituency.csv"
)
constituency_chart <-
  gain_constituency %>%
  ggplot(
    aes(
      reorder(
        parliamentary_constituency,
        total_gain
      ),
      total_gain
    )
  ) +
  geom_col(
    fill = "#005A9C"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Financial Gain by Constituency",
    x = "",
    y = "Financial Gain (£)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title =
      element_text(face = "bold"),
    axis.title =
      element_text(face = "bold")
  )

ggsave(
  "outputs/charts/financial_gain_constituency.png",
  constituency_chart,
  width = 8,
  height = 5
)
# --------------------------

# KPI 8 - BOXPLOT

# --------------------------

gain_boxplot_data <-
  outcomes_2025 %>%
  left_join(
    issue_lookup,
    by =
      "issue_reference_number"
  ) %>%
  filter(
    local_authority ==
      "Gateshead"
  )

cat(
  "\nUnmatched records:",
  sum(
    is.na(
      gain_boxplot_data$
        parliamentary_constituency
    )
  ),
  "\n"
)

boxplot_gain <-
  ggplot(
    gain_boxplot_data,
    aes(
      parliamentary_constituency,
      total_value_num
    )
  ) +
  geom_boxplot(
    fill = "#005A9C",
    alpha = 0.7
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Distribution of Financial Gain by Constituency",
    subtitle = "Advice in Community Project (2025)",
    x = "Parliamentary Constituency",
    y = "Financial Gain (£)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x =
      element_text(
        angle = 25,
        hjust = 1
      ),
    plot.title =
      element_text(face = "bold")
  )

ggsave(
  "outputs/charts/financial_gain_boxplot.png",
  boxplot_gain,
  width = 10,
  height = 6
)
# --------------------------

# FINAL OUTPUT

# --------------------------

cat("\nEXECUTIVE SUMMARY\n")

cat(
  "\nNumber of Clients:",
  num_clients
)

cat(
  "\nNumber of Issues:",
  num_issues
)

cat(
  "\nIssues per Client:",
  round(
    issues_per_client,
    2
  )
)

cat(
  "\n\nKEY FINDINGS"
)

cat(
  "\n- Food bank support represented the largest area of demand, suggesting continued cost-of-living pressures among residents."
)

cat(
  "\n- Debt and benefits enquiries accounted for a substantial proportion of advice activity, indicating ongoing financial vulnerability among service users."
)

cat(
  "\n- Income-related interventions generated the largest financial gains, demonstrating the value of welfare and income-maximisation support."
)

cat(
  "\n- The average number of issues per client indicates that many residents required support across multiple areas of need."
)


# --------------------------
# FINANCIAL OUTCOME SUMMARY
# --------------------------

financial_summary <- outcomes_2025 %>%
  summarise(
    mean_gain = mean(total_value_num, na.rm = TRUE),
    median_gain = median(total_value_num, na.rm = TRUE),
    min_gain = min(total_value_num, na.rm = TRUE),
    max_gain = max(total_value_num, na.rm = TRUE),
    total_gain = sum(total_value_num, na.rm = TRUE)
  )

financial_summary

financial_summary_table <- tibble(
  Metric = c(
    "Total Financial Gain",
    "Average Financial Gain",
    "Median Financial Gain",
    "Maximum Financial Gain"
  ),
  Value = c(
    sum(outcomes_2025$total_value_num),
    mean(outcomes_2025$total_value_num),
    median(outcomes_2025$total_value_num),
    max(outcomes_2025$total_value_num)
  )
)

write_csv(
  financial_summary_table,
  "outputs/tables/financial_summary.csv"
)

gain_distribution <-
  ggplot(
    outcomes_2025,
    aes(total_value_num)
  ) +
  geom_histogram(
    bins = 40,
    fill = "#005A9C"
  ) +
  labs(
    title = "Distribution of Financial Outcomes",
    x = "Financial Gain (£)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title =
      element_text(face = "bold"),
    axis.title =
      element_text(face = "bold")
  )

ggsave(
  "outputs/charts/gain_distribution.png",
  gain_distribution,
  width = 8,
  height = 5
)

cat(
  "\nFinancial outcomes appear positively skewed, indicating a small number of high-value outcomes contributed substantially to the total financial gain."
)

