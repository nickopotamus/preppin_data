# 2021, Week 40
# Animal adoptions

library(tidyverse) # Data handling
library(curl)      # Better import

temp <- tempfile()
source <- "https://data.austintexas.gov/resource/9t4d-g238.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

cats_and_dogs <- 
  read_csv(temp)[-4] %>% # Remove duplicated field   
  # Filter to cats and dogs (and remove the NA)
  filter(animal_type %in% c("Cat", "Dog") & !is.na(outcome_type)) %>% 
  mutate(outcome_type = fct_collapse(outcome_type, # Group outcome type
                                   "Adopted, Returned to Owner or Transferred" =
                                     c("Adoption", "Return to Owner", "Transfer"),
                                   other_level = "Other")) %>% 
  group_by(animal_type, outcome_type) %>% # Summarize
  summarise(count = n()) %>% 
  mutate(percentage = round(prop.table(count) * 100, 1)) %>% 
  select(-count) %>% 
  pivot_wider(names_from = outcome_type, # Neaten table
              values_from = percentage) %>% 
  arrange(desc(animal_type)) %>%         # Order as per example
  rename("Animal Type" = animal_type)    # Sort final name



