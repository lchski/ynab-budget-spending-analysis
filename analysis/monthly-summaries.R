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



# Employment income
pay_stub_summaries <- read_csv("../pay-stubs-analysis/data/out/monthly-summaries.csv")

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

summarize_spending <- function(transactions, month) {
  spending_transactions <- transactions %>%
    filter(
      ymonth == month,
      account_type == "budget",
      ! is_income
    )

  tibble(
    total = list(
      spending_transactions
    ),
    needs = list(
      spending_transactions %>%
        filter(category_type == "Needs")
    ),
    wants = list(
      spending_transactions %>%
        filter(category_type == "Wants")
    ),
    savings = list(
      spending_transactions %>%
        filter(category_type == "Savings")
    )
  ) %>%
    mutate(across(
      everything(),
      ~ map_dbl(.x, ~ .x %>%
                  count(wt = spend, name = "spend") %>%
                  pull(spend))
    ))
}

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

summarize_net_income <- function(transactions, month) {
  net_income_transactions <- register %>%
    filter(
      ymonth == month,
      account_type == "budget",
      is_income
    ) %>%
    mutate(income_type = case_when(
      payee == "Government of Canada" ~ "employment_salaried",
      str_detect(memo, fixed("Invoice", ignore_case = TRUE)) ~ "employment_self_employed",
      payee == "EQ Bank" ~ "interest",
      TRUE ~ "other"
    ))
  
  bind_cols(
    net_income_transactions %>%
      count(wt = spend, name = "total"),
    net_income_transactions %>%
      count(income_type, wt = spend, name = "spend") %>%
      pivot_wider(names_from = income_type, values_from = spend)
  )
}

summarize_employment_income <- function(month_to_summarize) {
  pay_stub_summaries %>%
    filter(month == month_to_summarize) %>%
    select(gross, deductions = total_taxes_and_deductions, net, cpp_qpp, pspp = pension)
}

summarize_month <- function(transactions, month_to_summarize) {
  tibble(
    net_worth = summarize_net_worth(transactions, month_to_summarize),
    spending = summarize_spending(transactions, month_to_summarize),
    net_income = summarize_net_income(transactions, month_to_summarize),
    employment_income = summarize_employment_income(month_to_summarize)
  )
}

tibble(
  month = c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01")
) %>%
  mutate(summary = map(
    month, 
    ~ register %>%
      summarize_month(.x) %>%
      unnest_wider(col = everything(), names_sep = "___")
  )) %>%
  unnest(summary) %>%
  glimpse()

register %>%
  summarize_month("2022-02-01") %>%
  unnest_wider(col = everything(), names_sep = "___") %>%
  glimpse
