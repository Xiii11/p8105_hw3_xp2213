P8105 Homework 3
================
2024-10-12

Name: Xi Peng UNI: xp2213

\#Question 1 Exploration of “NY NOAA” dataset

\##Section 1 Data cleaning

``` r
data("ny_noaa")

weather_dat = ny_noaa |> 
  janitor::clean_names() |> 
  separate(date, c('year', 'month', 'day'), sep ='-', remove = TRUE) |>
  mutate(
    tmax = as.numeric(tmax) / 10,
    tmin = as.numeric(tmin) / 10,
    prcp = prcp / 10,
    snow = snow / 10,
  )

snow_comm = weather_dat |> 
  group_by(snow,id) |> 
  summarise(count = n()) |> 
  arrange(desc(count))
```

In the original “ny_noaa” datasets, there are 2595176 observations and
there are 7 variables included, which are: id, date, prcp, snow, snwd,
tmax, tmin. In this section, the “date” variable was separated into
year, month, and day. Temperature (“tman” and “tmin”), precipitation
(“prcp”), and snowfall(“snow”) were tranfered into reasonable units.

By checking the “snow” variable for snowfall, the result shows that
`0 cm` and `NA` are the most commonly observed values. The likely reason
is that for most of the year, these regions do not experience snowfall,
especially during warmer months. Additionally, some regions included in
the dataset may not experience snow at all throughout the year due to
their warmer climates or geographical location. These places are
unlikely to have a snow season, and as a result, `0 cm` is consistently
recorded for snowfall in these areas. The presence of `NA` values
indicates missing data, which could be due to weather stations not
recording snowfall on certain days or data collection issues.
