library(tidyverse)

# Load the original data
un_votes <- read_csv("data/2026_10_15_votes_ga.csv")

# Keep useful variables
un_votes_clean <- un_votes |>
  select(
    resolution,
    date,
    country = ms_code,
    vote = ms_vote,
    title,
    subjects
  )

# Prepare for subjects for wrangling
subject_text <- un_votes_clean$subjects |>
  replace_na("") |>
  str_to_lower()

# Prepare titles for wrangling
title_text <- un_votes_clean$title |>
  replace_na("") |>
  str_to_lower()

# Check subjects for keywords
subject_match <- str_detect(subject_text, "econom") |
  str_detect(subject_text, "trade") |
  str_detect(subject_text, "financ") |
  str_detect(subject_text, "debt") |
  str_detect(subject_text, "poverty") |
  str_detect(subject_text, "sustainable development")

# Check titles for keywords
title_match <- str_detect(title_text, "econom") |
  str_detect(title_text, "trade") |
  str_detect(title_text, "financ") |
  str_detect(title_text, "debt") |
  str_detect(title_text, "poverty") |
  str_detect(title_text, "sustainable development")

# Final csv
economic_votes <- un_votes_clean |>
  filter(subject_match | title_match) |>
  arrange(date, resolution, country)

# Save one final CSV
write_csv(economic_votes, "data/economic_votes.csv")
