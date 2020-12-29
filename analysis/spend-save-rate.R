register %>%
  filter(account_type == "budget") %>%
  filter(! is.na(is_income)) %>% ## filter out budget<->budget transfers
  mutate(transaction_type = if_else(
    is_income,
    "income",
    "expense"
  )) %>%
  group_by(month = floor_date(date, "1 month"), transaction_type) %>%
  summarize(
    amount = sum(spend)
  )
