#importing needed libraries
library(fredr)
library(httr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(dbplyr)
library(sf)
library(lubridate)
library(readr)
library(zoo)

#importing api key
api_key <- readLines("/Users/angelasnider/Desktop/Econ2020/Econ2020-Final-Project/apikey.txt") # nolint
#Replication:replace this with your api key
fredr_set_key(api_key)



# Fetching Data
sticky_cpi <- fredr(series_id = "CORESTICKM159SFRBATL",
                    frequency = "m",
                    observation_start = as.Date("2007-01-01"))

flexible_cpi <- fredr(series_id = "FLEXCPIM679SFRBATL",
                      frequency = "m",
                      observation_start = as.Date("2007-01-01"))

real_dpi <- fredr(series_id = "DSPIC96",
                  frequency = "m",
                  observation_start = as.Date("2007-01-01"))

fed_funds_rate <- fredr(series_id = "FEDFUNDS",
                                   frequency = "m",
                                   observation_start = as.Date("2007-01-01"))


#Cleaning Data

clean_data <- function(data, series_id) {
  data %>%
    mutate(date = as.Date(date),  # Ensure date format is correct
           value = as.numeric(value)) %>%  # Convert value to numeric
    filter(!is.na(value)) %>%  # Remove rows with NA values
    mutate(value = ifelse(value < -999, NA, value)) %>%  # Convert extreme values to NA
    drop_na() %>%  # Drop any NAs that have been created
    rename_with(~ paste0(series_id, "_value"), value)  # Rename the value column
}

# Clean and rename each dataset
sticky_cpi_cleaned <- clean_data(sticky_cpi, "sticky_cpi")
flexible_cpi_cleaned <- clean_data(flexible_cpi, "flexible_cpi")
real_dpi_cleaned <- clean_data(real_dpi, "real_dpi")
fed_funds_rate_cleaned <- clean_data(fed_funds_rate, "fed_funds_rate")

# Combine the cleaned datasets
combined_data <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE),
                        list(sticky_cpi_cleaned, flexible_cpi_cleaned,
                             real_dpi_cleaned,
                             fed_funds_rate_cleaned))

all_data_table <- combined_data %>%
  select(date, sticky_cpi_value, flexible_cpi_value, real_dpi_value, fed_funds_rate_value)

scaling_factor <- 10000

# Scale down the real_dpi_value column
all_data_df_scaled <- all_data_table %>%
  mutate(scaled_real_dpi_value = real_dpi_value / scaling_factor)

# Define recession period data
recession_periods <- data.frame(
  start_date = as.Date(c("2008-12-01", "2020-03-01")),  # Example recession start dates
  end_date = as.Date(c("2009-06-01", "2020-06-01"))      # Example recession end dates
)

alldata_graph <- ggplot(all_data_df_scaled, aes(x = date)) +
  geom_rect(aes(xmin = as.Date("2008-12-01"), xmax = as.Date("2009-06-01"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +  # Shaded region for recession period
  geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +  # Shaded region for recession period
  geom_line(aes(y = sticky_cpi_value, color = "Sticky CPI")) +
  geom_line(aes(y = flexible_cpi_value, color = "Flexible CPI")) +
  geom_line(aes(y = scaled_real_dpi_value, color = "Real DPI (Scaled)")) +
  geom_line(aes(y = fed_funds_rate_value, color = "Fed Funds Rate")) +
  labs(title = "Time Series Data",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Sticky CPI" = "red", "Flexible CPI" = "blue", "Real DPI (Scaled)" = "green", "Fed Funds Rate" = "orange")) +
  theme_minimal()

noflex_graph <- ggplot(all_data_df_scaled, aes(x = date)) +
  geom_rect(aes(xmin = as.Date("2008-12-01"), xmax = as.Date("2009-06-01"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +  # Shaded region for recession period
  geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +  # Shaded region for recession period
  geom_line(aes(y = sticky_cpi_value, color = "Sticky CPI")) +
  geom_line(aes(y = scaled_real_dpi_value, color = "Real DPI (Scaled)")) +
  geom_line(aes(y = fed_funds_rate_value, color = "Fed Funds Rate")) +
  labs(title = "Time Series Data",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Sticky CPI" = "red", "Real DPI (Scaled)" = "green", "Fed Funds Rate" = "blue")) +
  theme_minimal()


# Graph with all variables and shaded recession periods
alldata_graph_short <- ggplot(all_data_df_scaled, aes(x = date)) +
  geom_rect(aes(xmin = as.Date("2008-12-01"), xmax = as.Date("2009-06-01"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +  # Shaded region for recession period
  geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +  # Shaded region for recession period
  geom_line(aes(y = sticky_cpi_value, color = "Sticky CPI")) +
  geom_line(aes(y = flexible_cpi_value, color = "Flexible CPI")) +
  geom_line(aes(y = scaled_real_dpi_value, color = "Real DPI (Scaled)")) +
  geom_line(aes(y = fed_funds_rate_value, color = "Fed Funds Rate")) +
  labs(title = "Time Series Data",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Sticky CPI" = "red", "Flexible CPI" = "blue", "Real DPI (Scaled)" = "green", "Fed Funds Rate" = "pink")) +
  theme_minimal() +
  xlim(as.Date("2018-01-01"), as.Date("2022-01-01")) +
  annotate("text", x = as.Date("2018-01-01"), y = -Inf, label = "Start Date", vjust = -0.5) +
  annotate("text", x = as.Date("2022-01-01"), y = -Inf, label = "End Date", vjust = -0.5)


#Graphs limited during time periods
graph_short_2020 <- ggplot(all_data_df_scaled, aes(x = date)) +
  geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +  # Shaded region for recession period
  geom_line(aes(y = sticky_cpi_value, color = "Sticky CPI")) +
  geom_line(aes(y = scaled_real_dpi_value, color = "Real DPI (Scaled)")) +
  geom_line(aes(y = fed_funds_rate_value, color = "Fed Funds Rate"))+
  labs(title = "Time Series Data",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Sticky CPI" = "red", "Real DPI (Scaled)" = "green", "Fed Funds Rate" = "blue")) +
  theme_minimal()+
  xlim(as.Date("2019-01-01"), as.Date("2024-01-01")) +
  annotate("text", x = as.Date("2019-01-01"), y = -Inf, label = "Start Date", vjust = -0.5) +
  annotate("text", x = as.Date("2024-01-01"), y = -Inf, label = "End Date", vjust = -0.5)

graph_short_2008 <- ggplot(all_data_df_scaled, aes(x = date)) +
  geom_rect(aes(xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-01"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +  # Shaded region for recession period
  geom_line(aes(y = sticky_cpi_value, color = "Sticky CPI")) +
  geom_line(aes(y = scaled_real_dpi_value, color = "Real DPI (Scaled)")) +
  geom_line(aes(y = fed_funds_rate_value, color = "Fed Funds Rate")) +
  labs(title = "Time Series Data",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Sticky CPI" = "red", "Real DPI (Scaled)" = "green", "Fed Funds Rate" = "blue")) +
  theme_minimal() +
  xlim(as.Date("2006-01-01"), as.Date("2010-01-01")) +
  annotate("text", x = as.Date("2006-01-01"), y = -Inf, label = "Start Date", vjust = -0.5) +
  annotate("text", x = as.Date("2010-01-01"), y = -Inf, label = "End Date", vjust = -0.5)


graph_short_2001 <- ggplot(all_data_df_scaled, aes(x = date)) +
  geom_rect(aes(xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-01"), ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +  # Shaded region for recession period
  geom_line(aes(y = sticky_cpi_value, color = "Sticky CPI")) +
  geom_line(aes(y = scaled_real_dpi_value, color = "Real DPI (Scaled)")) +
  geom_line(aes(y = fed_funds_rate_value, color = "Fed Funds Rate")) +
  labs(title = "Time Series Data",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("Sticky CPI" = "red", "Real DPI (Scaled)" = "green", "Fed Funds Rate" = "blue")) +
  theme_minimal()+
  xlim(as.Date("2000-01-01"), as.Date("2002-04-29")) + #no other reason other than it's my birthdate
  annotate("text", x = as.Date("2000-01-01"), y = -Inf, label = "Start Date", vjust = -0.5) +
  annotate("text", x = as.Date("2002-04-29"), y = -Inf, label = "End Date", vjust = -0.5)

