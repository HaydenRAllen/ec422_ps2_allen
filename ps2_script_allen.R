library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"), 
#  get = "economic.data",
#  from = "1976-01-01"
# )

# head(unemp_raw)

# dir.create("data", showWarnings = FALSE)

# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace( 
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[data == as.Date("2025-09-01")]
      )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(unemployment_rate, date) |>
  group_by(date) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_recent <- filter(unemp, date >= as.Date("2015-01-01"))

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )

unemp_top_increases <- unemp |>
  group_by(region) |>
  slice_max(unemployment_rate_change, n = 5) |>
  ungroup() |>
  arrange(desc(unemployment_rate_change))

unemp_top_decreases <- unemp |>
  group_by(region) |>
  slice_min(unemployment_rate_change, n = 5) |>
  ungroup() |>
  arrange(unemployment_rate_change)

unemp_top_increases_no_covid <- unemp |>
  filter(format(date, "%Y") != "2020") |>
  group_by(region) |>
  slice_max(unemployment_rate_change, n = 5) |>
  ungroup() |>
  arrange(desc(unemployment_rate_change))

unemp_top_decreases_no_covid <- unemp |>
  filter(format(date, "%Y") != "2020") |>
  group_by(region) |>
  slice_min(unemployment_rate_change, n = 5) |>
  ungroup() |>
  arrange(unemployment_rate_change)


ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Unemployment Rate Over Time",
       x = "Date",
       y = "Unemployment Rate (%)") +
  theme_minimal()

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(title = "Unemployment Rate Over Time by Region",
       x = "Date",
       y = "Unemployment Rate (%)") +
  theme_minimal()

ggplot(unemp_summary, aes(x = (fct_reorder(region, mean_rate)), y = mean_rate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Unemployment Rate by Region",
       x = "Region",
       y = "Average Unemployment Rate (%)") +
  theme_minimal()

unemp_covid <- unemp |>
  filter(format(date, "%Y") == "2020")
ggplot(unemp_covid, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Unemployment Rate During COVID-19 Pandemic",
       x = "Date",
       y = "Unemployment Rate (%)") +
  theme_minimal()





