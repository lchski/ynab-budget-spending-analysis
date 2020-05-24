library(tidyverse)
library(lubridate)
library(janitor)


library(helpers)

budget <- read_csv("data/source/budget.csv") %>% clean_names
register <- read_csv("data/source/register.csv") %>%
  clean_names %>%
  mutate_at(
    c("outflow", "inflow"),
    ~ as.numeric(str_remove_all(.x, "[^0-9\\.]"))
  ) %>%
  mutate(spend = inflow - outflow)
