# Set up ------------------------------------------------------------------
library(tidyverse)
library(lintr)
library(styler)
library(tidyverse)
library(reshape2)
library(maps)
library(mapproj)
library(patchwork)

df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
# For this assignment, I want to explore the incarcerated population in the state
# of Oregon. Specifically, I would like to take a closer look of the incarcerated 
# population who are on ICE hold, across counties in Oregon.
# I will also be exploring races represented by the incarcerated population 
# over time in Oregon.

# Introduction & Summary Information --------------------------------------

# How many people are incarcerated in each county in Oregon according to the
# most recent data?
or_county_total <- na.omit(df) %>% 
  filter(year == max(year)) %>% 
  filter(state == "OR") %>% 
  select(state, county_name, total_jail_pop)
or_total_recent <- sum(or_county_total$total_jail_pop)

# Total number of incarcerated population in Oregon that were on ICE hold 
# according to the most recent data
ICE_each_county <- df %>% 
  filter(year == max(year)) %>% 
  filter(state == "OR") %>% 
  select(state, county_name, total_jail_from_ice)
or_ice_total <- sum(ICE_each_county$total_jail_from_ice, na.rm = T)

# Difference between percentage of incarcerated black population vs white population in
# each county in Oregon according to the most recent data?
or_diff_percentage <- na.omit(df) %>% 
  filter(year == max(year)) %>% 
  filter(state == "OR") %>% 
  mutate(diff_percentage = (black_jail_pop_rate - white_jail_pop_rate)*0.01) %>% 
  select(state, county_name, diff_percentage)
avg_diff <- mean(or_diff_percentage$diff_percentage)
median_diff <- median(or_diff_percentage$diff_percentage)

# Total number of incarcerated population of each race in Oregon each year
or_race_over_year <- na.omit(df) %>% 
  filter(state == "OR") %>% 
  group_by(year) %>%
  summarise(black_pop = sum(black_jail_pop), 
            aapi_pop = sum(aapi_jail_pop),
            latinx_pop = sum(latinx_jail_pop),
            white_pop = sum(white_jail_pop),
            native_pop = sum(native_jail_pop),
            other_race_pop = sum(other_race_jail_pop))
or_current_black_pop <- or_race_over_year %>% 
  filter(year == max(year)) %>% 
  pull(black_pop)

# Percentage of incarcerated population that were on ICE hold in Oregon over time?
or_ICE_percentage <- df %>% 
  filter(state == "OR") %>% 
  group_by(year) %>% 
  summarise(percentage_ICE = 100 * sum(total_jail_from_ice, na.rm = TRUE) 
            / sum(total_jail_pop, na.rm = TRUE)) 
or_ice_avg <- mean(or_ICE_percentage$percentage_ICE)

# Trends over time chart --------------------------------------------------
# A line graph that documents the change in number of incarcerated population 
# of each race

mdf <- melt(or_race_over_year, id = "year")
p <- ggplot(data = mdf, aes(x = year, y = value, colour = variable)) +
  geom_line() +
  labs(x = "year", 
       y = "Population in Jail",
       title = "Change in Number of Incarcerated Population by Race") +
  scale_x_continuous("year", labels = mdf$year, breaks = mdf$year) +
  theme(axis.text.x = element_text(angle = 65))

# Variable Comparison Chart -----------------------------------------------

# A scatterplot that reveals the correlation between number of black individual 
# jail admission count and total white population between age of 14 to 64 in Oregon
vdf <- na.omit(df) %>% 
  filter(state == "OR") %>% 
  group_by(year) %>% 
  summarise(black_adm = sum(black_prison_adm), white_pop = sum(white_pop_15to64))
  
v <- ggplot(vdf) +
  geom_point(mapping = aes(x = white_pop, y = black_adm)) +
  labs(x = "White Population between 15 to 64", 
       y = "Black Individual Jail Admission Count",
       title = "Correlation of White Population between 15 to 64 and Number
       of Black Individual Jail Admission in Oregon over the Span of 14 years")

# Map ---------------------------------------------------------------------
library(maps)
library(mapproj)
library(patchwork)

# filter most recent data
most_recent <- df %>% 
  filter(year == max(year))

# create a county df: long | lat | group | order |polyname | fips 
county_shapes <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by = "polyname")

# join map and incarceration df:
or_joined_df <- county_shapes %>% 
  left_join(most_recent, by = "fips") %>% 
  filter(state == "OR")
  
# create blanck theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

m <- ggplot(or_joined_df) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(or_joined_df$black_jail_pop)),
                        na.value = "white", low = "peachpuff", high = "red") +
  blank_theme +
  ggtitle("Incarcerated Black Population in Oregon Counties") 
  
  





  