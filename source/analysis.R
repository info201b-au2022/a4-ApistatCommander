library(tidyverse)
library(ggplot2)
library(dplyr)
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
data <- get_data()
## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
avg_black_jail_rate <- data %>%
  filter(year == 2018) %>%
  pull(black_jail_pop_rate) %>%
  mean( na.rm = TRUE)
avg_black_jail_rate <- avg_black_jail_rate/ 100
avg_black_jail_rate

#
highest_black_jail_rate_state <- data %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(black_jail_rate = mean(black_jail_pop_rate, na.rm = TRUE)) %>%
  na.omit %>%
  filter(black_jail_rate == max(black_jail_rate))%>%
  pull(state)

highest_black_jail_rate<- data %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(black_jail_rate = mean(black_jail_pop_rate, na.rm = TRUE)) %>%
  na.omit %>%
  filter(black_jail_rate == max(black_jail_rate))%>%
  pull(black_jail_rate)
highest_black_jail_rate


avg_white_jail_rate <- data %>%
  filter(year == 2018) %>%
  pull(white_jail_pop_rate) %>%
  mean( na.rm = TRUE)
avg_white_jail_rate <- avg_black_jail_rate / 100

utah_white_jail_rate <- data %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(white_jail_rate = mean(white_jail_pop_rate, na.rm = TRUE)) %>%
  na.omit %>%
  filter(state == "UT")%>%
  pull(white_jail_rate)
utah_white_jail_rate

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>

#View(data)
get_year_jail_pop <- function() {
  df <- data %>%
    select(year, total_pop)
return(df)   
}
pop_data <- get_year_jail_pop()
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  vis <- ggplot(data = data) + 
    geom_col(mapping = aes(x = year, y = total_pop))
  return(vis)   
} 
chart <- plot_jail_pop_for_us()
chart

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  df <- data%>%
    filter(state %in% states)%>%
    group_by(state, year)%>%    
#    select(state, year, total_pop)%>%
    summarise(jail_pop = sum(total_pop, na.rm = T))
  return(df)
}
Wa <- get_jail_pop_by_states(c("WA", "OR"))


plot_jail_by_states <- function(states){
  chart <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = jail_pop, color= state))
  return(chart)
}
WaChart <- plot_jail_by_states(c("WA", "OR"))
WaChart
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

race_df <- function(){
  african_american_df <- data %>%
    select(year, black_pop_15to64, black_jail_pop)%>%
    group_by(year)%>%
    summarise(jail_rate = (sum(black_jail_pop, na.rm = TRUE)*100) / (sum(black_pop_15to64, na.rm = TRUE)))%>%
    filter(jail_rate > 0)%>%
    filter(jail_rate != Inf)

  white_df <- data %>%
    select(year, white_pop_15to64,white_jail_pop)%>%
    group_by(year)%>%
    summarise(white_americans_jailed = (sum(white_jail_pop, na.rm = TRUE)*100)/ (sum(white_pop_15to64, na.rm =TRUE))) %>%
    filter(white_americans_jailed > 0) %>%
    filter(white_americans_jailed != Inf)

combined_data <- left_join(african_american_df, white_df, by = "year")

return(combined_data)
}
#View(race_df())

race_plot <- function(){
  african_american_discrimination <-ggplot(data = race_df(), aes(x = year))
  african_american_discrimination<- african_american_discrimination + geom_line(aes(y=jail_rate, color = "African American Population"))
  african_american_discrimination <- african_american_discrimination + geom_line(aes(y = white_americans_jailed, color = "White American Population"))
  african_american_discrimination <- african_american_discrimination + labs(
    title = "Imprisonment Rate between African Americans and White Americans (1990 - 2018)",
    caption = "The graph shows that African Americans are more likely to imprisoned compared to White Americans.",
    x = "Year",
    y = "Imprisonment rate ((jail population * 100) / (total population)"
  )
  return(african_american_discrimination)
}
race_plot()
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
state_names <- read.csv("../source/state_abbr.csv") 
state_names_fix <- state_names %>%
  mutate(
    state = tolower(state),
    abbrev = tolower(abbrev),
    code = tolower(code)
  )
View(state_names_fix)

# 
  state_shape <- map_data("state")
  ggplot(state_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group),
      color = "white",
      size = .1
    ) +
    coord_map()
  return(state_shape)


black_jail_2018 <- function(){
  jail2018 <- data%>%
    select(year, state, black_jail_pop_rate)%>%
    filter(year == 2018)%>%
    group_by(state)%>%
    summarise(total_black_jail_pop = mean(black_jail_pop_rate, na.rm = TRUE))%>%
#    na.omit()%>%
    mutate(state = tolower(state))
  
  colnames(jail2018)[1] = "code"
  jail2018_combined <- right_join(jail2018, state_names_fix, by = "code") %>%
  subset(select = c(1,3,2,4))%>%
  select(state, total_black_jail_pop) %>%
  na.omit()
  
  return(jail2018_combined)
}
View(black_jail_2018())
state_map_data <- function(){
  state_shape <- map_data("state") %>%
    rename(state = region) %>%
    left_join(black_jail_2018(), by = "state")
  map_vis <- ggplot(state_shape) +
    geom_polygon(
      mapping = aes (x = long, y = lat, group = group, fill = total_black_jail_pop),
      color = "white",
      size = .1
    ) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(fill = "Black Jail Rate per .01") 
  return(map_vis)
}

state_map_data()
## Load data frame ---- 


