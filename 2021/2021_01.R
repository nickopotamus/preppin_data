# 2021, week 1
# Bike sales
# https://preppindata.blogspot.com/2021/01/2021-week-1.html

library(tidyverse)

raw_data <- googlesheets4::read_sheet("1GYv4573GnJa-C21NYeDj-OhFSTwrK0SnQNF2IQFqa50")

# Function to convert factors (called by fct_relevel())
bike_regex <- function(x = NULL) {
  x[grepl("^Grav", x)] <- "Gravel"
  x[grepl("^Moun", x)] <- "Mountain"
  x[grepl("^Ro", x)] <- "Road"
  return(x)
}

# Create data view
bikes <- raw_data %>%
  # Split Store/Bike
  separate(`Store - Bike`, into = c("Store", "Bike"), sep = " - ") %>% 
  mutate(Bike = fct_relabel(Bike, bike_regex),       # Recode factors using regex
         Quarter = lubridate::quarter(Date),         # Quarter
         `Day of Month` = lubridate::mday(Date)) %>% # Day of the month
  filter(`Order ID` > 10) %>%                        # Drop test rows
  # Reorder variables as in example
  select(c(Quarter, Store, Bike, `Order ID`, `Customer Age`, `Bike Value`, `Existing Customer?`, `Day of Month`))

# Visualisation
# Not sure how they want this to work - each quarter has >31 days
bikes %>% 
  # Sort by day within quarter
  arrange(Quarter, `Day of Month`) %>% 
  # Total sold for each bike type per day
  group_by(Quarter, `Day of Month`, Bike) %>% 
  summarise(daily_earnings = sum(`Bike Value`)) %>% 
  # Cumulative earnings by day
  group_by(Quarter, Bike) %>%
  mutate(cum_earnings = cumsum(daily_earnings)) %>% 
  # Plot  
  ggplot() +
    aes(x = `Day of Month`, y = cum_earnings, color = Bike) +
    geom_line() +
    facet_wrap(~Quarter, strip.position = "left", ncol = 1) +
    # Make it look more like target plot
    theme_light() +
    labs(title = "Typical Running Monthly Sales in each Quarter",
         subtitle = "For <span style='color:#663300'>Mountain</span>, <span style='color:#CC9933'>Gravel</span>, and <span style='color:#666666'>Road</span> bikes") +
    theme(plot.subtitle = ggtext::element_markdown(),
          legend.position = "none") +
    ylab("Running Total of Sales") +
    scale_color_manual(values = c("#CC9933", "#663300", "#666666")) +
    scale_x_continuous(expand = c(0,0), limits = c(1, 31), breaks = seq(2, 30, by = 2))
  