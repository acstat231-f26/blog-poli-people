
library(tidyverse)
democracy_raw <- read_csv("EIU_DI_WIDEF.csv")
glimpse(democracy_raw)



democracy_countries <- democracy_raw |>
  filter(
    str_detect(REF_AREA, "^[A-Z]{3}$"),
    REF_AREA != "WLD"
  ) |>
  select(
    country_code = REF_AREA,
    country_name = REF_AREA_LABEL,
    matches("^[0-9]{4}$")
  )

glimpse(democracy_countries)


democracy_tidy <- democracy_countries |>
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "democracy_index"
  ) |>
  mutate(year = as.integer(year)) |>
  arrange(country_name, year)

View(democracy_tidy)
nrow(democracy_tidy)
n_distinct(democracy_tidy$country_code)
sum(is.na(democracy_tidy$democracy_index))

democracy_tidy |>
  count(country_code, year) |>
  filter(n > 1)


dir.create("data", showWarnings = FALSE)

write_csv(
  democracy_tidy,
  "data/democracy_index_tidy.csv"
)

