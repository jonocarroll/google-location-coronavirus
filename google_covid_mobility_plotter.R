# Fork of https://gist.github.com/johnburnmurdoch/1d23978fc8213ff656d9f629608dd1f5/revisions
# modified to work with https://github.com/nacnudus/google-location-coronavirus

# Install and load required packages
# install.packages("needs")
# library(needs)
# needs(tidyverse, magrittr, animation, pdftools, png, scales)
library(tidyverse)
library(magrittr)
library(animation)
library(pdftools)
library(png)
library(scales)
library(RcppRoll)
library(ggrepel)

# final_country <- readr::read_tsv("https://github.com/jonocarroll/google-location-coronavirus/blob/master/2020-04-11-country.tsv?raw=true")
final_country <- readr::read_tsv("2020-04-11-au_state.tsv")

countries_to_plot <- c("AU", "NZ")

# PLot this dataset as small multiples; one chart for each topic, one line on each per country
final_country %>% 
  # Get rid of any missing data
  filter(!is.na(trend)) %>%
  # Make sure dates are read as dates (days) not date-times
  mutate(
    date = as.character(as.Date(date, "%Y-%m-%d")),
    date = as.Date(date)
  ) %>%
  # Make sure we only have one value per country per topic per day
  group_by(category, date, country_code) %>%
  summarise(trend = mean(trend)) %>%
  group_by(category, country_code) %>%
  # Create a smoothed moving average to iron out noise in the daily data (if you want)
  mutate(smoothed = roll_meanr(trend, 7)) %>% 
  # Filter for only a subset of countries of interest for this plot
  filter(country_code %in% countries_to_plot) %>%
  # Plot moving average (or raw data) vs date
  ggplot(aes(date, smoothed)) +
  # Add a dark grid line for zero on the y-asix
  geom_hline(yintercept = 0) +
  # Draw one line per country
  geom_line(aes(col = country_code), size=0.5) +
  # Add a highlight for a country of focus, first adding a thicker white line to create a border behind the main one
  # geom_line(size=1.5, data = . %>% filter(country_code == "AU"), col = "white") +
  # geom_line(size=1, data = . %>% filter(country_code == "AU"), col = "black") +
  # Add country labels to the end of each line
  geom_text_repel(aes(col = country_code, label = country_code), direction = "y", data = . %>% group_by(country_code, category) %>% top_n(1, date), hjust=0) +
  # Clean up the x-axis
  scale_x_date(limits = c(as.Date("2020-02-21"), as.Date("2020-04-11")), breaks = c(as.Date("2020-02-22"), as.Date("2020-04-11")), labels = function(x)format(x,"%d %b")) +
  # Duplicate y-axis for easy reading of values
  # scale_y_continuous(limits=c(-100,40), breaks = seq(-80, 40, 20), expand=c(0,0), sec.axis = dup_axis()) +
  # Small multiples: one chart per topic
  facet_wrap(~category, nrow=1) +
  # Clean up the plot theme
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(),
    axis.title = element_blank(),
    legend.position = "none"
  ) 

## STATES

# PLot this dataset as small multiples; one chart for each topic, one line on each per country
final_country %>% 
  # Get rid of any missing data
  filter(!is.na(trend)) %>%
  # Make sure dates are read as dates (days) not date-times
  mutate(
    date = as.character(as.Date(date, "%Y-%m-%d")),
    date = as.Date(date)
  ) %>%
  # Make sure we only have one value per country per topic per day
  group_by(category, date, sub_region_name) %>%
  summarise(trend = mean(trend)) %>%
  group_by(category, sub_region_name) %>%
  # Create a smoothed moving average to iron out noise in the daily data (if you want)
  mutate(smoothed = roll_meanr(trend, 7)) %>% 
  # Filter for only a subset of countries of interest for this plot
  # filter(sub_region_name %in% countries_to_plot) %>%
  # Plot moving average (or raw data) vs date
  ggplot(aes(date, smoothed)) +
  # Add a dark grid line for zero on the y-asix
  geom_hline(yintercept = 0) +
  # Draw one line per country
  geom_line(aes(col = sub_region_name), size=0.5) +
  # Add a highlight for a country of focus, first adding a thicker white line to create a border behind the main one
  # geom_line(size=1.5, data = . %>% filter(country_code == "AU"), col = "white") +
  # geom_line(size=1, data = . %>% filter(country_code == "AU"), col = "black") +
  # Add country labels to the end of each line
  geom_text_repel(aes(col = sub_region_name, label = sub_region_name), direction = "y", data = . %>% group_by(sub_region_name, category) %>% top_n(1, date), hjust=0) +
  # Clean up the x-axis
  scale_x_date(limits = c(as.Date("2020-02-21"), as.Date("2020-04-11")), breaks = c(as.Date("2020-02-22"), as.Date("2020-04-11")), labels = function(x)format(x,"%d %b")) +
  # Duplicate y-axis for easy reading of values
  # scale_y_continuous(limits=c(-100,40), breaks = seq(-80, 40, 20), expand=c(0,0), sec.axis = dup_axis()) +
  # Small multiples: one chart per topic
  facet_wrap(~category, nrow=1) +
  # Clean up the plot theme
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(),
    axis.title = element_blank(),
    legend.position = "none"
  ) 


