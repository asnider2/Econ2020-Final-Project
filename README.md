# Econ2020-Final-Project



Introduction:
This project explores time series data related to various economic indicators sourced from the Federal Reserve Economic Data (FRED) website. The indicators include Consumer Price Index (CPI), Disposable Personal Income (DPI), and the Federal Funds Rate. The project involves fetching the data, cleaning it, visualizing it using ggplot2, and analyzing trends over time.

Data Sources-
The data used in this project is sourced from the following FRED series:
https://fred.stlouisfed.org/series/CORESTICKM159SFRBATL

https://fred.stlouisfed.org/series/FLEXCPIM679SFRBATL

https://fred.stlouisfed.org/series/DSPIC96

https://fred.stlouisfed.org/series/FEDFUNDS

https://fred.stlouisfed.org/series/CIVPART


Installation:
To run this project, you need to install the following R packages:

1.Copy code and run in R terminal:
install.packages(c("fredr", "httr", "purrr", "ggplot2", "tidyverse", "dbplyr", "sf", "lubridate", "readr", "zoo"))


Additionally, initialize your renv environment by running:
1:Copy these two lines and run in R terminal
renv::init()
renv::snapshot()

Usage:
Input your own API key in the provided line of code:
api_key <- readLines("YOURCODEHERE")
fredr_set_key(api_key)

Run the script to fetch and process the data.

The project includes several visualizations of the time series data, such as alldata_graph, noflex_graph, alldata_graph_short, graph_short_2020, graph_short_2008, and graph_short_2001. These graphs can be customized and used for further analysis.


TLDR: To replicate this project, follow these steps:
Install the required R packages as described above.
Initialize your renv environment and snapshot the dependencies.
Input your FRED API key in the provided line of code.
Run the script to fetch and process the data.
Customize the visualizations or add additional analyses as needed.
