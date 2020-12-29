investment_accounts <- register %>%
  filter(str_detect(account, "RRSP|TFSA")) %>%
  distinct(account) %>%
  pull

investments_by_year <- register %>%
  filter(account %in% investment_accounts) %>%
  filter(! is_reconciliation) %>%
  group_by(year = year(date)) %>%
  summarize(investments = sum(spend))

employment_income_by_year <- register %>%
  filter(
    (account == "EQ Bank" & payee == "Government of Canada") |
    (str_detect(memo, "Invoice"))
  ) %>%
  group_by(year = year(date)) %>%
  summarize(earnings = sum(spend))
  

employment_income_by_year %>%
  left_join(investments_by_year) %>%
  mutate(investment_pct = round(investments / earnings * 100, 1))

employment_income_by_year %>%
  left_join(investments_by_year) %>%
  mutate(
    earnings_ma3 = slide_dbl(earnings, ~mean(.x), .before = 1, .after = 1),
    investments_ma3 = slide_dbl(investments, ~mean(.x), .before = 1, .after = 1),
  ) %>%
  mutate(
    investment_pct = round(investments / earnings * 100, 1),
    investment_ma3_pct = round(investments_ma3 / earnings_ma3 * 100, 1)
  )

zzz <- employment_income_by_year %>%
  left_join(investments_by_year) %>%
  mutate(
    earnings_ma3 = slide_dbl(earnings, ~mean(.x), .before = 1, .after = 1),
    investments_ma3 = slide_dbl(investments, ~mean(.x), .before = 1, .after = 1),
  ) %>%
  rename(
    earnings_actual = earnings,
    investments_actual = investments
  ) %>%
  pivot_longer(
    cols = c(earnings_actual:investments_ma3),
    names_sep = "_",
    names_to = c("type", "precision")
  )

ggplot(zzz, aes(x = year)) +
  geom_col(
    data = zzz %>%
      filter(precision == "actual"),
    mapping = aes(y = value, fill = type)
  ) +
  geom_point(
    data = zzz %>%
      filter(precision == "ma3"),
    mapping = aes(y = value)
  ) +
  geom_line(
    data = zzz %>%
      filter(precision == "ma3"),
    mapping = aes(y = value, color = type)
  )

employment_income_by_year %>%
  left_join(investments_by_year) %>%
  mutate(
    earnings_ma3 = slide_dbl(earnings, ~mean(.x), .before = 1, .after = 1),
    investments_ma3 = slide_dbl(investments, ~mean(.x), .before = 1, .after = 1),
  ) %>% summarize_at(vars(matches("investments|earnings")), sum) ## see effects of moving averages
