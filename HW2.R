# Homework 2
# Due October 11
# Run this entire script from top to bottom in a fresh R session

packages <- c("dplyr", "tidyr", "tibble", "readr", "stringr", "forcats", "purrr")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

# Problem 1 setup
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# Problem 1 Part A
sales_clean <- sales_raw %>%
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

print(sales_clean %>%
        select(sale_id, item_clean, amount, discount_rate, net_amount, size))
# All eight rows remain and the original columns are preserved
# Missing amounts stay missing after applying discounts
# Large sales have net amounts of at least 20

# Problem 1 Part B
sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(sale_id, code_raw, code_clean)
print(invalid_codes)
# Invalid sale IDs are 5 and 7
# Sale 8 becomes valid after converting on-108 to uppercase

valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  select(sale_id, code_clean) %>%
  separate_wider_delim(code_clean, delim = "-", names = c("channel", "number"),
                       cols_remove = FALSE) %>%
  select(sale_id, code_clean, channel, number)
print(valid_code_parts)

sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  separate_longer_delim(tags, delim = ";")
print(sale_tags)
# separate_wider_delim splits a string across new columns while separate_longer_delim splits its entries into new rows

# Problem 1 Part C
customer_summary <- sales_clean %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(!is.na(net_amount) & net_amount >= 20),
    large_share = if (sum(!is.na(net_amount)) == 0) NA_real_ else
      sum(!is.na(net_amount) & net_amount >= 20) / sum(!is.na(net_amount)),
    .by = customer_id
  ) %>%
  arrange(customer_id)
print(customer_summary)
# Expected large shares are C1 0, C2 2/3, C3 0, C4 1, C5 NA
# For C1 the numerator is zero known large sales and the denominator is one known sale
# C1 has a second sale but its net amount is missing so it is excluded from the denominator

sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )
print(levels(sales_factors$item_factor))
print(count(sales_factors, item_factor, .drop = FALSE))
print(levels(sales_factors$item_group))
print(count(sales_factors, item_group, .drop = FALSE))
# Item counts are pen 4, folder 2, notebook 1, eraser 1
# Changing factor order rearranges categories without changing membership
# Lumped categories combine less common items into Other
# Pen and folder remain separate while notebook and eraser become Other

# Problem 2 setup
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# Problem 2 Part A
reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(customer_id, .direction = "down") %>%
  ungroup()
print(reports_labeled)

reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )
print(reports_clean %>%
        select(form, line, customer_id, round, primary_amount, backup_amount, amount))
# coalesce selects the first nonmissing value
# A recorded primary zero is a real value so the backup of 99 is not used

reports_complete <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE) %>%
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE)) %>%
  arrange(customer_id, round)
print(reports_complete)

original_rows <- nrow(reports_clean)
added_rows <- nrow(reports_complete) - original_rows
missing_before <- sum(is.na(reports_clean$amount))
missing_after <- sum(is.na(reports_complete$amount))
print(tibble(original_rows, added_rows, missing_before, missing_after))
# Expected counts are 6 original rows, 6 added rows, 1 missing before and 7 missing after
# Explicit missing value is customer C3 in round 1
# Implicit missing row is customer C2 in round 2 before completion
# Missing reports are not treated as zero amounts

# Problem 2 Part B
customer_id_check <- tibble(
  duplicate_ids = anyDuplicated(customers$customer_id) > 0,
  missing_ids = anyNA(customers$customer_id)
)
print(customer_id_check)
# Both results are FALSE

duplicate_customer_rounds <- reports_clean %>%
  count(customer_id, round, name = "row_count") %>%
  filter(row_count > 1)
print(duplicate_customer_rounds)
# No customer and round combination occurs more than once

customers_marked <- customers %>% mutate(customer_found = TRUE)
reports_joined <- reports_clean %>%
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(customer_found = coalesce(customer_found, FALSE))
print(reports_joined %>%
        select(customer_id, round, amount, region, customer_found))

only_in_reports <- reports_clean %>%
  distinct(customer_id) %>%
  anti_join(customers %>% distinct(customer_id),
            by = join_by(customer_id), na_matches = "never")
only_in_customers <- customers %>%
  distinct(customer_id) %>%
  anti_join(reports_clean %>% distinct(customer_id),
            by = join_by(customer_id), na_matches = "never")
print(only_in_reports)
print(only_in_customers)
# C4 appears only in reports and C5 appears only in customers
# C3 has customer_found TRUE even though its region is missing
# C4 has customer_found FALSE because it has no customer metadata record
# Therefore missing region alone cannot distinguish unknown region from unmatched ID

# Problem 2 Part C
# IMPORTANT the PDF references customers_bad but never defines it
# The following labeled example duplicates C2 with a conflicting region of East
# Replace this example if your instructor provides an official customers_bad setup
customers_bad <- bind_rows(
  customers,
  tibble(customer_id = "C2", region = "East")
)

reports_duplicated <- reports_clean %>%
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many",
    na_matches = "never"
  )

join_comparison <- tibble(
  stage = c("Before join", "After join"),
  rows = c(nrow(reports_clean), nrow(reports_duplicated)),
  total_amount = c(sum(reports_clean$amount, na.rm = TRUE),
                   sum(reports_duplicated$amount, na.rm = TRUE))
)
print(join_comparison)

repeated_reports <- reports_duplicated %>%
  count(customer_id, round, name = "row_count") %>%
  filter(row_count > 1)
print(repeated_reports)
# Under the explicitly labeled example the C2 round 1 report is repeated
# The join changes 6 rows to 7 and changes the amount total from 75 to 95
# The added 20 is a duplicate of an existing report rather than a new sale
# With relationship many-to-one the join would error because C2 matches twice
# Allowing many-to-many suppresses the relationship error but does not resolve conflicting regions

reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  pivot_wider(names_from = round, values_from = amount, names_prefix = "round_") %>%
  select(customer_id, round_1, round_2, round_3)
print(reports_wide)

long_nonmissing <- reports_wide %>%
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    names_prefix = "round_",
    values_to = "amount",
    values_drop_na = TRUE
  ) %>%
  mutate(round = as.integer(round))
print(long_nonmissing)
print(nrow(long_nonmissing))
# The long table keeps 5 rows with known amounts
# Dropping NA omits both previously recorded unknown amounts and unrecorded rounds
# Removing missing entries cannot recover the values that were never known

# Problem 3 Part A
# Independent two sided samples from normal populations
# H0 means are equal and HA means differ

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x)
  ny <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)

  if (equal_var) {
    pooled_variance <- ((nx - 1) * var_x + (ny - 1) * var_y) / (nx + ny - 2)
    se <- sqrt(pooled_variance * (1 / nx + 1 / ny))
    df <- nx + ny - 2
    method <- "Pooled"
  } else {
    a <- var_x / nx
    b <- var_y / ny
    se <- sqrt(a + b)
    df <- (a + b)^2 / (a^2 / (nx - 1) + b^2 / (ny - 1))
    method <- "Welch"
  }

  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  reject <- p_value < alpha
  list(method = method, t_stat = t_stat, df = df,
       p_value = p_value, reject = reject)
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch_result <- two_sample_t(x_check, y_check)
pooled_result <- two_sample_t(x_check, y_check, equal_var = TRUE)
print(welch_result)
print(pooled_result)

welch_reference <- t.test(x_check, y_check, var.equal = FALSE)
pooled_reference <- t.test(x_check, y_check, var.equal = TRUE)
print(welch_reference)
print(pooled_reference)

verification <- tibble(
  method = c("Welch", "Pooled"),
  t_difference = c(abs(welch_result$t_stat - unname(welch_reference$statistic)),
                   abs(pooled_result$t_stat - unname(pooled_reference$statistic))),
  df_difference = c(abs(welch_result$df - unname(welch_reference$parameter)),
                    abs(pooled_result$df - unname(pooled_reference$parameter))),
  p_difference = c(abs(welch_result$p_value - welch_reference$p.value),
                   abs(pooled_result$p_value - pooled_reference$p.value))
) %>%
  mutate(
    t_within_1e8 = t_difference < 1e-8,
    df_within_1e8 = df_difference < 1e-8,
    p_within_1e8 = p_difference < 1e-8
  )
print(verification, width = Inf)
# Both methods should agree with their corresponding t test within 1e-8
# Welch estimates degrees of freedom while the pooled method uses nx plus ny minus 2

# Problem 3 Part B
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# Seed is set exactly once immediately before simulation loops
set.seed(2026)
simulation_storage <- vector("list", nrow(scenarios) * B * 2)
row_index <- 1L

for (scenario_index in seq_len(nrow(scenarios))) {
  current <- scenarios[scenario_index, ]
  for (replicate_index in seq_len(B)) {
    x <- rnorm(n_x, mean = current$mean_x, sd = current$sd_x)
    y <- rnorm(n_y, mean = current$mean_y, sd = current$sd_y)

    for (equal_variances in c(FALSE, TRUE)) {
      test_result <- two_sample_t(x, y, equal_var = equal_variances, alpha = alpha)
      simulation_storage[[row_index]] <- tibble(
        scenario = current$scenario,
        replicate = replicate_index,
        method = test_result$method,
        t_stat = test_result$t_stat,
        df = test_result$df,
        p_value = test_result$p_value,
        reject = test_result$reject
      )
      row_index <- row_index + 1L
    }
  }
}

simulation_results <- bind_rows(simulation_storage)
print(head(simulation_results, 6))
print(nrow(simulation_results))
# Exactly 4 scenarios times 1000 repetitions times 2 methods produces 8000 rows

simulation_summary <- simulation_results %>%
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) %>%
  arrange(scenario, method)
print(simulation_summary %>%
        mutate(across(c(mean_t, sd_t, rejection_rate), ~ round(.x, 3))),
      n = Inf)

# Comparison plot of estimated rejection rates
plot_values <- xtabs(rejection_rate ~ method + scenario, data = simulation_summary)
barplot(plot_values, beside = TRUE, ylim = c(0, 1),
        main = "Rejection rates by scenario and method",
        ylab = "Rejection rate", xlab = "Scenario", legend.text = TRUE)
abline(h = alpha, lty = 2)

# Short report limited to four sentences
# For equal_null both tests should reject near 0.05 with some Monte Carlo variation
# For unequal_null Type I error can differ from 0.05 especially for the pooled test because the smaller sample has the larger variance
# When population means differ rejection rates measure power and should generally rise compared with the corresponding null scenarios
# Higher power is not enough to prefer a test if it also inflates Type I error by rejecting too frequently when the null is true
