transaction_types_by_month <- register %>%
  filter(account_type == "budget") %>%
  filter(! is.na(is_income)) %>% ## filter out budget<->budget transfers
  filter(! str_detect(payee, "EQ GIC")) %>%
  mutate(transaction_type = case_when(
    category %in% c("Savings", "Car Insurance") ~ "saving",
    is_income ~ "income",
    ! is_income ~ "expense"
  )) %>%
  mutate(spend = if_else(
    transaction_type %in% c("saving", "expense"),
    spend * -1,
    spend
  )) %>%
  group_by(month = floor_date(date, "1 month"), transaction_type) %>%
  summarize(
    amount = sum(spend)
  ) %>%
  mutate(amount = if_else( ## fix for September 2017, when we had income of -$2.02 (due to a wallet cash reconciliation)
    transaction_type == "income" & amount < 0,
    0,
    amount
  ))

transaction_types_by_month %>%
  ggplot(aes(x = month, y = amount, color = transaction_type, fill = transaction_type)) +
  geom_col(position = "fill")

transaction_types_by_month %>%
  group_by(month) %>%
  mutate(amount_pct = amount / sum(amount)) %>%
  ggplot(aes(x = month, y = amount_pct, color = transaction_type, fill = transaction_type)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(transaction_type), ncol = 1) +
  scale_y_continuous(limits = c(0, 1))



## NOTES TO SELF
# - "saving" is actually a bit more complicated than just what's put away
#     - see, we also "save" by budgeting for future months
#     - so any month's savings is really more like `implicit_savings = income - expense - explicit_savings`
#     - but then we want to account for the growing pile of savings somehow? maybe? not sure

transaction_types_by_month %>%
  pivot_wider(names_from = transaction_type, values_from = amount) %>%
  select(month, income, expense, explicit_saving = saving) %>%
  mutate_at(vars(income:explicit_saving), ~ if_else(is.na(.x), 0, .x)) %>%
  mutate(funds_not_spent = income - expense - explicit_saving) %>%
  ungroup() %>%
  mutate(available_funds = income + lag(funds_not_spent, n = 1, default = 0)) %>%
  mutate(implicit_saving = available_funds - expense - explicit_saving)

transaction_types_by_month %>%
  pivot_wider(names_from = transaction_type, values_from = amount) %>%
  select(month, income, expense, explicit_saving = saving) %>%
  mutate_at(vars(income:explicit_saving), ~ if_else(is.na(.x), 0, .x)) %>%
  ungroup() %>%
  mutate(unspent_funds = income - expense - explicit_saving) %>%
  mutate(available_funds = lag(unspent_funds, default = 0) + unspent_funds + income)

transaction_types_by_month %>%
  pivot_wider(names_from = transaction_type, values_from = amount) %>%
  select(month, income, expense, explicit_saving = saving) %>%
  mutate_at(vars(income:explicit_saving), ~ if_else(is.na(.x), 0, .x)) %>%
  ungroup() %>%
  mutate(implicit_saving = income - expense - explicit_saving) %>%
  mutate(available_funds_for_future = cumsum(implicit_saving)) %>%
  pivot_longer(cols = income:available_funds_for_future) %>%
  filter(name %in% c("available_funds_for_future", "implicit_saving", "explicit_saving")) %>%
  ggplot(aes(x = month, y = value, color = name)) +
  geom_point() +
  geom_line()



