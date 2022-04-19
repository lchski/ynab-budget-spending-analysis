library(tidyverse)
library(lubridate)
library(janitor)

accounts <- read_csv("data/indices/accounts.csv")

budget <- read_csv("data/source/budget.csv") %>% clean_names
register <- read_csv("data/source/register.csv") %>%
  clean_names %>%
  mutate_at(
    c("outflow", "inflow"),
    ~ as.numeric(str_remove_all(.x, "[^0-9\\.]"))
  ) %>%
  mutate(spend = inflow - outflow) %>%
  mutate(
    is_income = category == "Ready to Assign", ## used to be "To be Budgeted"
    is_reconciliation = payee == "Reconciliation Balance Adjustment",
    is_transfer = str_detect(payee, "^Transfer : "),
    category_type = str_extract(category_group, "^(\\w*)") # first word in category
  ) %>%
  mutate(ymonth = floor_date(date, "1 month")) %>%
  left_join(accounts)
