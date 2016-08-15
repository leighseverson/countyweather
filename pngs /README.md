
<!-- README.md is generated from README.Rmd. Please edit that file -->
Setting up to use the package
-----------------------------

Currently, this package exists in a development version on GitHub. To use the function, you need to install it directly from GitHub using the `install_github` function from `devtools`. Further, the package relies on some functions that are in a development version of `rnoaa`, so you need to install that version of the `rnoaa` package.

You can use the following code for this set-up:

``` r
library(devtools)
install_github("ropenscilabs/rnoaa")
install_github("leighseverson/countyweather")
library(countyweather)
```

You will also need an API key from NOAA to be able to access the weather data. You can get one by ...

This package draws mainly on functions from the `rnoaa` package to identify weather monitors within a county based on its FIPS code and then pull weather data for a specified date range from those monitors. It then does some additional cleaning and aggregating to produce a single, county-level weather dataset. Further, it maps the monitors used for that county and date range and allows you to create and write datasets for many different counties using a single function call.

Basic examples of using the package
-----------------------------------

This package allows you to pull data based on a US county's FIPS code. FIPS codes are 5-digit codes that are unique for every US county. The first two digits specify state and the last three specify county within the state. The health data used for environmental epidemiology studies is often aggregated at the county level for US studies, making it very useful to be able to create weather datasets by county.

Currently, this package can pull daily and hourly weather data for variables like temperature and precipitation. The weather data is collected at monitors, and there are often multiple weather monitors within a county. This package allows you to specify a county and then will pull weather data from all monitors in that county over a specified date range and average the daily or hourly values to generate an average time series for the county.

### Pulling daily data

Here is an example of creating a dataset with daily precipitation for Miami-Dade county (FIPS = 12086) for August 1992, when Hurricane Andrew stuck:

``` r
andrew_precip <- weather_fips(fips = "12086", 
                            date_min = "1992-08-01", 
                            date_max = "1992-08-31",
                            var = c("PRCP"))
```

The output from this function call includes both the weather dataset (`andrew_precip$weather_data`) and a map showing the locations of weather monitors included in the county-averaged dataset (`anderew_precip$station_map`).

Here are the first few rows of the dataset:

``` r
head(andrew_precip$weather_data)
#> Source: local data frame [6 x 3]
#> Groups: date [6]
#> 
#>         date     prcp prcp_reporting
#>       (date)    (dbl)          (int)
#> 1 1992-08-02 8.850000              6
#> 2 1992-08-03 9.366667              6
#> 3 1992-08-04 5.483333              6
#> 4 1992-08-05 2.716667              6
#> 5 1992-08-06 1.633333              6
#> 6 1992-08-07 7.200000              6
```

The dataset includes columns for date (`date`), precipitation (in \[units\], `prcp`), and also the number of stations used to calculated each precipitation observation (`prcp_reporting`).

Here is a plot of this data:

``` r
library(ggplot2)
ggplot(andrew_precip$weather_data, aes(x = date, y = prcp,
                                       color = prcp_reporting)) + 
  geom_line() + 
  theme_minimal()
```

![](README-unnamed-chunk-6-1.png)

From this plot, you can see both the extreme precipitation associated with Hurricane Andrew and that the storm knocked out quite a few of the weather monitors normally available.

To see a map of the monitors used for the county average, run:

``` r
andrew_precip$station_map
```

![](README-unnamed-chunk-7-1.png)

### Pulling hourly data

Here is an example of pulling hourly data for Miami-Dade, again for the period around Hurricane Andrew:

``` r
andrew_hourly <- hourly_fips_df(fips = "12086", year = 1992,
                                var = c("wind_speed", "temperature"))
```

The output from this call includes the date-time of the observation (given in UTC), values for the weather variables selected, and the number of monitors reporting for each observation of each weather variable:

``` r
head(andrew_hourly)
#> Source: local data frame [6 x 5]
#> Groups: date_time [6]
#> 
#>             date_time temperature wind_speed temperature_reporting
#>                (time)       (dbl)      (dbl)                 (int)
#> 1 1992-01-01 00:00:00    20.56667  2.2666667                     3
#> 2 1992-01-01 01:00:00    20.36667  1.0333333                     3
#> 3 1992-01-01 02:00:00    19.40000  2.3500000                     2
#> 4 1992-01-01 03:00:00    19.80000  1.2000000                     3
#> 5 1992-01-01 04:00:00    18.90000  0.8666667                     2
#> 6 1992-01-01 05:00:00    18.86667  2.0666667                     3
#> Variables not shown: wind_speed_reporting (int)
```

Here are hourly values for the month of Hurricane Andrew:

``` r
library(dplyr)
library(lubridate)
to_plot <- andrew_hourly %>%
  filter(months(date_time) == "August")
ggplot(to_plot, aes(x = date_time, y = wind_speed,
                    color = wind_speed_reporting)) + 
  geom_line() + theme_minimal()
```

![](README-unnamed-chunk-10-1.png)

### Pulling data from stream gauges

There are also some functions in this package to pull stream gauge data by FIPS code. (Note: This functionality is still in development.)

``` r
andrew_streams <- stream_data(fips = "12086", date_min = "1992-08-01",
                             date_max = "1992-08-31")
```

The returned data gives the average daily stream flow for all stream gauges in the county over the time period:

``` r
head(andrew_streams)
#> Source: local data frame [6 x 2]
#> 
#>        dates     mean
#>       (date)    (dbl)
#> 1 1992-08-01 362.0750
#> 2 1992-08-02 364.0750
#> 3 1992-08-03 366.6313
#> 4 1992-08-04 346.4438
#> 5 1992-08-05 352.5688
#> 6 1992-08-06 368.1467
```

``` r
ggplot(andrew_streams, aes(x = dates, y = mean)) + 
  geom_line() + theme_minimal()
```

![](README-unnamed-chunk-13-1.png)

This section is still in development, because these measurments need to be compared to typical values for each stream gauge to get a realistic view of when flooding / high stream flow occurred at different gauge locations.

Futher options available in the package
---------------------------------------

The user can choose to filter out any monitors that report variables for less that a certain percent of time (`coverage`).

More on the weather data
------------------------

\[Add more here on the daily and hourly weather data we're pulling.\]

Error and warning messages you may get
--------------------------------------

### Not able to pull data from a monitor

`In rnoaa::meteo_pull_monitors(monitors = stations, keep_flags = FALSE,  :   The following stations could not be pulled from the GHCN ftp:  USR0000FTEN   Any other monitors were successfully pulled from GHCN.`

### `curl` timeout
