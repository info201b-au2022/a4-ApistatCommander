library(tidyverse)
library(ggplot2)
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
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
data <- get_data()
View(data)
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
View(Wa)

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
    select(year, black_pop_15to64)%>%
    group_by(year)%>%
    summarise(african_americans_jailed = sum(black_pop_15to64, na.rm = TRUE))%>%
    filter(african_americans_jailed > 0)


  white_df <- data %>%
    select(year, white_pop_15to64)%>%
    group_by(year)%>%
    summarise(white_americans_jailed = sum(white_pop_15to64, na.rm = TRUE)) %>%
    filter(white_americans_jailed > 0)

combined_data <- left_join(african_american_df, white_df, by = "year")

return(combined_data)
}
View(race_df())

race_plot <- function(){
  african_american_discrimination <-ggplot(data = combined_data, aes(x = year))
  african_american_discrimination<- african_american_discrimination + geom_line(aes(y=african_americans_jailed, color = "African American Population"))
  african_american_discrimination <- african_american_discrimination + geom_line(aes(y = white_americans_jailed, color = "White American Population"))
  return(african_american_discrimination)
}
race_plot()
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


