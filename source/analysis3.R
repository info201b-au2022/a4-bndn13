library(tidyverse)
library(ggplot2)
library(maps)
library(mapdata)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
incarceration_trends <- read.csv("C:/Users/bnngu/Documents/INFO201/Assignment/a4-bndn13/source/incarceration_trends.csv")
view(incarceration_trends)


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
max_jail_pop <- 
  incarceration_trends %>%
  group_by(year) %>%
  summarize(max_total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
View(max_jail_pop)

max <- max_jail_pop %>%
  filter(max_total_jail_pop == max(max_total_jail_pop))
print(max)

get_year_jail_pop <- function() {
  df <- data.frame(Year = incarceration_trends$year,
                   Total_Jail_Population = incarceration_trends$total_jail_pop)
  return(df)
}
View(get_year_jail_pop())

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(){
  jail_pop_trend_chart <-
    ggplot(get_year_jail_pop()) +
    geom_col(
      mapping = aes(x = Year, y = Total_Jail_Population)) +
    labs(
      title = "Jail Population Trend in the U.S. (1970 - 2018)",
      caption = "Visualization of U.S. Jail Population Trend from 1970 to 2018"
    )
  return(jail_pop_trend_chart)
}
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
get_year_jail_pop_by_states <- function(states){
  df <- incarceration_trends %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(Total_Jail_Population = sum(total_jail_pop, na.rm = TRUE), .groups = "drop")
  return(df)
}
View(get_year_jail_pop_by_states(c("WA","OR","CA")))

plot_jail_pop_by_state <- function(states) {
  chart <- 
    ggplot(data = get_year_jail_pop_by_states(states),
           aes(x = year, y = Total_Jail_Population, group = state)) +
    geom_line(
      aes(linetype = state, color = state)) +
    labs(title = "Title")
  return(chart)
}
plot_jail_pop_by_state(c("WA","OR","CA"))

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

# create a dataset
data_frame <- data.frame("Year" = incarceration_trends$year,
                         "Black_Pop" = incarceration_trends$black_pop_15to64,
                         "White_Pop" = incarceration_trends$white_pop_15to64,
                         "Black_Prison" = incarceration_trends$black_prison_pop,
                         "White_Prison" = incarceration_trends$white_prison_pop)
#View(data_frame)

ratio_df <- data_frame %>%
  ##group_by(Year) %>%
  summarise(year = Year, b_ratio = Black_Prison/Black_Pop, w_ratio = White_Prison/White_Pop,)
#View(ratio_df)

prison_pop_by_race_over_year <- function(){
  df <- ratio_df %>%
    group_by(year) %>%
    select(year, b_ratio, w_ratio)
  return(df)
}
#View(df)

plot_prison_pop_by_race_over_year <- function(){
  chart <- 
    ggplot(data = prison_pop_by_race_over_year(), 
          aes(x = b_ratio, y = w_ratio, group = year)) +
    ylim(0, 0.04) +
    xlim(0, 1.25) +
    geom_point(
      aes(color = year)
    ) + 
    labs(
      title = "Prison Population trend by Race",
      caption = "caption"
    )
  return(chart)
}
plot_prison_pop_by_race_over_year()

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 
state_total <- incarceration_trends %>%
  group_by(state) %>%
  summarize(total_prison_pop_per_state = sum(total_prison_pop, na.rm = TRUE))
#View(state_total)

new_state_total <- state_total %>%
  mutate(full_name = tolower(state.name[match(state_total$state, state.abb)])) %>%
  rename(abbr = state) %>%
  rename(state = full_name)
#View(new_state_total)

# Join incarceration data to the U.S. shape file
state_shape <- map_data("state") %>% # load state shape file
  rename(state = region) %>% # rename for joining
  left_join(new_state_total, by="state") # join value data
#View(state_shape)

# Draw the map setting the `fill` of each state using total_prison_pop_per_state
plot_prison_distribution_by_state <- function(){
  ggplot(state_shape) + 
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = total_prison_pop_per_state),
      color = "white", # show state outlines
      size = .1        # thinly stroked
    ) +
    coord_map() + # use a map-based coordinate system
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(fill = "Death") +
    ggtitle("State Prison Population Distribution")
}
plot_prison_distribution_by_state()

