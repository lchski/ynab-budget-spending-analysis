library(slider)

monthly_income_mas <- register %>%
  filter(is_income) %>%
  group_by(ymonth = floor_date(date, "1 month")) %>%
  summarize(income = sum(spend, na.rm = TRUE)) %>%
  mutate(income_ma = slide_dbl(income, mean, .before = 12))

monthly_spend_pct <- register %>%
  filter(! is_income, ! is_reconciliation, ! is_transfer) %>%
  group_by(ymonth = floor_date(date, "1 month"), category) %>%
  summarize(spend = sum(spend, na.rm = TRUE) * -1) %>%
  left_join(monthly_income_mas) %>%
  ungroup %>%
  mutate(spend_pct = round(spend / income_ma, 2)) %>%
  select(-spend:-income_ma)

monthly_spend_pct %>%
  filter(
    year(ymonth) > 2019,
    year(ymonth) < 2021
  ) %>%
  group_by(ymonth) %>%
  top_n(3) %>%
  arrange(ymonth, -spend_pct) %>%
  View()




# Spending
register %>%
  filter(
    ymonth == "2022-02-01",
    account_type == "budget"
  ) %>%
  filter(! is_income) %>%
  count(wt = spend, name = "spend")

# Spending by category
register %>%
  filter(
    ymonth == "2022-02-01",
    account_type == "budget"
  ) %>%
  count(category_type, wt = spend, name = "spend")

# Net income
register %>%
  filter(
    ymonth == "2022-02-01",
    account_type == "budget"
  ) %>%
  filter(is_income) %>%
  mutate(income_type = case_when(
    payee == "Government of Canada" ~ "Employment",
    payee == "EQ Bank" ~ "Interest",
    TRUE ~ "Other"
  )) %>%
  count(income_type, wt = spend, name = "spend")

# Employment income
read_csv("../pay-stubs-analysis/data/out/monthly-summaries.csv") %>%
  filter(
    month == "2022-02-01"
  )

summarize_net_worth <- function(transactions, month) {
  net_worth_transactions <- transactions %>%
    filter(ymonth <= month)
  
  tibble(
    total = list(
      net_worth_transactions %>%
        filter(! str_detect(account, "^Pension –"))
    ),
    liquid = list(
      net_worth_transactions %>%
        filter(! str_detect(account, "^Pension –")) %>%
        filter(account_type == "budget")
    ),
    illiquid = list(
      net_worth_transactions %>%
        filter(! str_detect(account, "^Pension –")) %>%
        filter(account_type == "tracking")
    ),
    incl_pension_total = list(
      net_worth_transactions
    ),
    incl_pension_illiquid = list(
      net_worth_transactions %>%
        filter(account_type == "tracking")
    )
  ) %>%
    mutate(across(
      everything(),
      ~ map_dbl(.x, ~ .x %>%
                  count(wt = spend, name = "spend") %>%
                  pull(spend))
    ))
}

summarize_month <- function(transactions, month_to_summarize) {
  tibble(
    net_worth = summarize_net_worth(transactions, month_to_summarize)
  )
}
