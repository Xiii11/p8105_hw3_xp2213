P8105 Homework 3
================

Name: Xi Peng UNI: xp2213 Date: 10.14.2024

# Question 1 Exploration of “NY NOAA” dataset

## Section 1 Data cleaning

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

## Section 2 Comparison of average maximum temperature in January and July across weather stations over the years

``` r
weather_jan_jul = weather_dat |> 
  drop_na() |> 
   mutate(month = as.numeric(month)) |>
  filter(month == 01 | month == 07) |> 
  group_by(id, year, month) |> 
  summarise(avg_tmax = mean(tmax, na.rm = TRUE))
 
weather_jan_jul_ggplot = weather_jan_jul |> 
ggplot(aes(x = year, y = avg_tmax, group = id, color = id)) +
  geom_point(size = 1.5) +
  facet_wrap(~month, ncol = 1, scale = "free", labeller = labeller(month = c("1" = "January", "7" = "July"))) +
  labs(
    title = "Average Max Temperature in January and July by Stations",
       x = "Year", 
       y = "Average Max Temperature (°C)") +
  theme_minimal() + 
  theme(legend.position = "none") +
  viridis::scale_color_viridis(
    name = "id", 
    discrete = TRUE)

knitr::opts_chunk$set(
  fig.width = 15,
  fig.asp = 10,
  out.width = "100%"
)

weather_jan_jul_ggplot
```

![](p8105homework3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

I checked the dataset and recognized there are 747 stations. One issue
that needed attention is the large number of stations in the dataset.
Including all the station labels would result in the scatterplot not
being displayed properly because of the excessive number of stations in
the legend.

According to the scatterplot, there are clear seasonal differences
between January and July temperatures. The temperatures are generally
colder in January, and warmer in July across stations and years. Due to
the large number of stations included in this dataset, without
restricting the analysis to a specific time range or a particular set of
stations, I can only observe general trends. The observable structure
shows that temperatures in January typically range from -10 °C to 10 °C,
where temperatures in July typically ranger from 20 °C to 32.5 °C.
Comparing 1981 to 2010, there appears to be a slight increase in
temperatures for both January and July, suggesting a possible warming
trend over time.There’s considerable variability both between stations
and from year to year, illustrating geographical and annual climate
differences. There are several noticeable outlines. For example, in
January 1982 and 1996, some stations recorded average maximum
temperatures below -10 °C, reflecting extremely cold winters. Also, one
station in July 1988 recorded unusually low average temperature, far
below the general trend. These anomalies could indicate extreme weather
events or errors in data recording.

## Section 3 Analysis of temperature extremes and distribution of snowfall patterns

``` r
tmax_vs_tmin_panel = weather_dat |> 
  ggplot(aes(x = tmin, y = tmax)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c() +
  labs(
    title = "Temperature Comparison: tmax vs tmin",
    x = "Minimum Temperature (°C)",
    y = "Maximum Temperature (°C)"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom") 

snowfall_distri_panel = weather_dat |> 
  filter(snow > 0.0 & snow < 100.0) |> 
  ggplot(aes(x = year, y = snow, color = id)) +
  geom_point(alpha = 0.1, size = 1) +
  labs(
    subtitle = "Distribution of snowfall values by year",
    x = "Year",
    y = "Snowfall (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

ggsave("tmax_vs_tmin_panel.pdf", tmax_vs_tmin_panel, width = 20, height = 18)
ggsave("snowfall_distri_panel.pdf", snowfall_distri_panel, width = 10, height = 20)
```

Due to the large size of the two panels, which made it difficult to
display them properly in the document, I have saved these two panels as
pdf files.
