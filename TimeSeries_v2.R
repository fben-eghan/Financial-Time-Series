#Attempt to improve the original code 
library(tidyverse)
library(lubridate)

# function to generate a time series plot
ts_plot <- function(data, title, xlab, ylab) {
  ggplot(data, aes(x = date, y = value)) + 
    geom_line() +
    labs(title = title, x = xlab, y = ylab) +
    scale_x_date(date_labels = "%Y") +
    scale_y_continuous()
}

# read in data
soi <- read.delim("SOI.txt", sep = "", dec = ".") %>%
  select(-1) %>%
  pivot_longer(cols = everything(), names_to = "month", values_to = "value") %>%
  mutate(date = ymd(paste0("1876-", month, "-01"))) %>%
  select(-month)

sunspots <- read_csv("sunspots.csv", col_names = c("year", "value")) %>%
  mutate(date = ymd(paste0(year, "-01-01"))) %>%
  select(-year)

temp <- read_csv("temperature.csv", col_names = c("year", "value")) %>%
  mutate(date = ymd(paste0(year, "-01-01"))) %>%
  select(-year)

sp500 <- read_table("sp500.txt", col_names = "value") %>%
  mutate(date = ymd(paste0("1926-", 1:780, "-01")))

fx <- read_csv("FX.csv", col_names = c("date", "eur", "gbp")) %>%
  mutate(date = dmy(date), value = eur/gbp) %>%
  select(-eur, -gbp)

# generate plots
soi_plot <- ts_plot(soi, "Southern Oscillation Index", "Date", "Value")
sunspots_plot <- ts_plot(sunspots, "Average Numbers of Sunspots", "Date", "Sunspots")
temp_plot <- ts_plot(temp, "Yearly Temperature Anomalies", "Date", "Yearly Temperature Anomaly")
sp500_plot <- ts_plot(sp500, "S&P500 Monthly Returns", "Date", "Monthly Return")
fx_plot <- ts_plot(fx, "EUR/GBP Daily Exchange Rates", "Date", "Exchange Rate")

# print plots
soi_plot
sunspots_plot
temp_plot
sp500_plot
