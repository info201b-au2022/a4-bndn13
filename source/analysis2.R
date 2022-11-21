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
#view(incarceration_trends)

S2_df <- data.frame("Year" = incarceration_trends$year,
                    "State" = incarceration_trends$state,
                    "Black_Pop" = incarceration_trends$black_pop_15to64,
                    "White_Pop" = incarceration_trends$white_pop_15to64,
                    "Black_Prison" = incarceration_trends$black_prison_pop,
                    "White_Prison" = incarceration_trends$white_prison_pop)
#View(data_frame)

S2ratio_df <- S2_df %>%
  summarise(year = Year, b_ratio = Black_Prison/Black_Pop, w_ratio = White_Prison/White_Pop,)
View(S2ratio_df )

num_observations <- nrow(incarceration_trends)

mean_ratio <- mean(S2ratio_df$b_ratio, na.rm = TRUE)
print(mean_ratio)

max_ratio <- max(S2ratio_df$b_ratio, na.rm = TRUE)
print(max_ratio)

min_ratio <- min(S2ratio_df$b_ratio, na.rm = TRUE)
print(min_ratio)
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
#View(max_jail_pop)

max <- max_jail_pop %>%
  filter(max_total_jail_pop == max(max_total_jail_pop))
print(max)

get_year_jail_pop <- function() {
  df <- data.frame(Year = incarceration_trends$year,
                   Total_Jail_Population = incarceration_trends$total_jail_pop)
  return(df)
}
#View(get_year_jail_pop())

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(){
  jail_pop_trend_chart <-
    ggplot(get_year_jail_pop()) +
    ylim(0, 900000) +
    geom_col(
      mapping = aes(x = Year, y = Total_Jail_Population)) +
    labs(
      title = "Jail Population Trend in the U.S. (1970 - 2018)",
      y = "Total Jail Population",
      x = "Year",
      caption = "Visualization of U.S. Jail Population Trend from 1970 to 2018"
    ) +
    theme(plot.caption = element_text(hjust = 0.5))
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
#View(get_year_jail_pop_by_states(c("WA","OR","CA")))

plot_jail_pop_by_state <- function(states) {
  chart <- 
    ggplot(data = get_year_jail_pop_by_states(states),
           aes(x = year, y = Total_Jail_Population, group = state)) +
    geom_line(
      aes(linetype = state, color = state)) +
    labs(title = "Growth of Prison Population by State",
         y = "Total Jail Population",
         x = "Year",
         caption = "Figure 4.1 Trend of Prison Growth in Arizona, Massachusetts, New Jersey, Virginia, and Washington State (1970-1980)") +
    theme(plot.caption = element_text(hjust = 0.5))
  return(chart)
}
plot_jail_pop_by_state(c("NJ","VA","WA", "AZ", "MA"))

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
View(ratio_df)

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
    ylim(0, 1) +
    xlim(0, 1) +
    geom_point(
      aes(color = year)
    ) + 
    labs(
      title = "Prison Population trend by Race",
      caption = "Fig. 5.1 Ratio of Prison Population by Year",
      x = "Black Prison Population Ratio",
      y = "White Prison Population Ratio"
    ) +
    theme(plot.caption = element_text(hjust = 0.5))
  return(chart)
}
plot_prison_pop_by_race_over_year()

plot_zoomed_prison_pop_by_race_over_year <- function(){
  chart <- 
    ggplot(data = prison_pop_by_race_over_year(), 
           aes(x = b_ratio, y = w_ratio, group = year)) +
    ylim(0, 0.05) +
    xlim(0, 1) +
    geom_point(
      aes(color = year)
    ) + 
    labs(
      title = "Prison Population trend by Race (Zoomed In)",
      caption = "Fig. 5.2 Ratio of Prison Population by Year (Zoomed In)",
      x = "Black Prison Population Ratio",
      y = "White Prison Population Ratio"
    ) +
    theme(plot.caption = element_text(hjust = 0.5)) 
  return(chart)
}
plot_zoomed_prison_pop_by_race_over_year()

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

### Load Black data frame ----
data_frame2 <- data.frame("Year" = incarceration_trends$year,
                         "State" = incarceration_trends$state,
                         "Black_Pop" = incarceration_trends$black_pop_15to64,
                         "White_Pop" = incarceration_trends$white_pop_15to64,
                         "Black_Prison" = incarceration_trends$black_prison_pop,
                         "White_Prison" = incarceration_trends$white_prison_pop)
View(data_frame2)

ratio_df2 <- data_frame2 %>%
  summarise(state = State, b_ratio = Black_Prison/Black_Pop, w_ratio = White_Prison/White_Pop,)
View(ratio_df2)

black_pop_state_total <- ratio_df2 %>%
  group_by(state) %>%
  summarize(black_prison_pop_per_state = mean(b_ratio, na.rm = TRUE))
black_pop_state_total[is.na(black_pop_state_total)] = 0
black_pop_state_total[sapply(black_pop_state_total, is.infinite)] <- 0
View(black_pop_state_total)

new_black_pop_state_total <- black_pop_state_total %>%
  mutate(full_name = tolower(state.name[match(black_pop_state_total$state, state.abb)])) %>%
  rename(abbr = state) %>%
  rename(state = full_name)
#View(new_black_pop_state_total)

# Join incarceration data to the U.S. shape file
black_state_shape <- map_data("state") %>% # load state shape file
  rename(state = region) %>% # rename for joining
  left_join(new_black_pop_state_total, by="state") # join value data
#View(state_shape)

# Draw the map setting the `fill` of each state using total_prison_pop_per_state
plot_black_pop_prison_distribution_by_state <- function(){
  ggplot(black_state_shape) + 
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_prison_pop_per_state),
      color = "white", # show state outlines
      size = .1        # thinly stroked
    ) +
    coord_map() + # use a map-based coordinate system
    scale_fill_continuous(low = "#D7DBE7", high = "#043E74", limits=c(0, 0.06)) +
    labs(fill = "Ratio",
         caption = "Fig. 6.1 Ratio of Black Prison Population by State (1970-2018)",
         x = "",
         y = "") +
    theme(plot.caption = element_text(hjust = 0.5)) +
    ggtitle("Distribution of Ratio of Black Prison Population by State")
}
plot_black_pop_prison_distribution_by_state()



### Load White data frame ----
white_pop_state_total <- ratio_df2 %>%
  group_by(state) %>%
  summarize(white_prison_pop_per_state = mean(w_ratio, na.rm = TRUE))
white_pop_state_total[is.na(white_pop_state_total)] = 0
white_pop_state_total[sapply(white_pop_state_total, is.infinite)] <- 0
View(white_pop_state_total)

new_white_pop_state_total <- white_pop_state_total %>%
  mutate(full_name = tolower(state.name[match(white_pop_state_total$state, state.abb)])) %>%
  rename(abbr = state) %>%
  rename(state = full_name)
#View(new_black_pop_state_total)

# Join incarceration data to the U.S. shape file
white_state_shape <- map_data("state") %>% # load state shape file
  rename(state = region) %>% # rename for joining
  left_join(new_white_pop_state_total, by="state") # join value data
#View(state_shape)

# Draw the map setting the `fill` of each state using total_prison_pop_per_state
plot_white_pop_prison_distribution_by_state <- function(){
  ggplot(white_state_shape) + 
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = white_prison_pop_per_state),
      color = "white", # show state outlines
      size = .1        # thinly stroked
    ) +
    coord_map() + # use a map-based coordinate system
    scale_fill_continuous(low = "#D7DBE7", high = "#043E74", limits=c(0, 0.06)) +
    labs(fill = "Ratio",
         caption = "Fig. 6.2 Ratio of White Prison Population by State (1970-2018)",
         x = "",
         y = "") +
    theme(plot.caption = element_text(hjust = 0.5)) +
    ggtitle("Distribution of Ratio of White Prison Population by State")
}
plot_white_pop_prison_distribution_by_state()
