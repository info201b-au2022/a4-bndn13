library(tidyverse)
library(ggplot2)
library(maps)
library(mapdata)

incarceration_trends <- read.csv("C:/Users/bnngu/Documents/INFO201/Assignment/a4-bndn13/source/incarceration_trends.csv")
view(incarceration_trends)

# Load incarceration data
state_total <- incarceration_trends %>%
  group_by(state) %>%
  summarize(total_prison_pop_per_state = sum(total_prison_pop, na.rm = TRUE))
View(state_total)

new_state_total <- state_total %>%
  mutate(full_name = tolower(state.name[match(geoDF$state, state.abb)])) %>%
  rename(abbr = state) %>%
  rename(state = full_name)
View(new_state_total)

# Join incarceration data to the U.S. shape file
state_shape <- map_data("state") %>% # load state shape file
  rename(state = region) %>% # rename for joining
  left_join(new_state_total, by="state") # join value data
View(state_shape)

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

