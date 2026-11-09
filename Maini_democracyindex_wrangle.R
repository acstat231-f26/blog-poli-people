
library(tidyverse)
democracy_raw <- read_csv("data/EIU_DI_WIDEF.csv")
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
# create csv
write_csv(
  democracy_tidy,
  "data/democracy_index_tidy.csv"
)

#clustering below, November 9th.
imports <- read_csv("data/Annual_Merchandise_Imports.csv") |>
  select(Economy_Label, 
         imports = '2024_Millions_of_US_at_current_prices_Value')

exports <- read_csv("data/Annual_Merchandise_Exports.csv") |>
  select(Economy_Label,
         exports = `2024_Millions_of_US_at_current_prices_Value`)

population <- read_csv("data/Annual_Population.csv") |>
  select(Economy_Label,
         population = `2024_Absolute_value_in_thousands_Value`)


trade_data <- democracy_tidy |>
  filter(year == 2024) |>
  inner_join(imports, by = c("country_name" = "Economy_Label")) |>
  inner_join(exports, by = c("country_name" = "Economy_Label")) |>
  inner_join(population, by = c("country_name" = "Economy_Label")) |>
  filter(!is.na(democracy_index),
         !is.na(imports),
         !is.na(exports),
         !is.na(population),
         population > 0,
         imports + exports > 0) |>
  mutate(
    trade_per_person = 1000 * (imports + exports) / population,
    export_share = exports / (imports + exports),
    log_trade = log10(trade_per_person)
  )


set.seed(231)

trade_data$cluster <- as.factor(kmeans(
  scale(trade_data |> select(log_trade, export_share)),
  centers = 3,
  nstart = 20
)$cluster)


ggplot(trade_data,
       aes(x = trade_per_person, y = democracy_index,
           color = cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_x_log10() +
  labs(
    title = "International Trade and Democracy (2024)",
    x = "Merchandise Trade per Person (USD, log scale)",
    y = "Democracy Index",
    color = "Cluster"
  ) +
  theme_minimal()

