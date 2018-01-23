
# Load needed packages ################################################
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)


# Read in Data ------------------------------------------------------------

# assumes big_property_data and big_property_info are loaded into workspace

# Temporary Step to avoid repeating Step 1 script
big_property_info <- read_rds("big_property_info.rds")
big_property_data <- read_rds("big_property_data.rds")





# Prop Count --------------------------------------------------------------

# Add a column with a bunch of 1's that can be summed each time to get a prop count
big_property_data <- big_property_data %>%
                     mutate(prop_count = 1)



# Submarket - Sequential Monthly Eff Rent TS -------------------------------------------------

# Calculate Eff Rent using the comparable_monthly_mom filter
test <- big_property_data %>%
        filter(comparable_monthly_mom == TRUE) %>%
        group_by(submarket, date) %>%
        summarize(prop_count = sum(prop_count),
                  eff_rent = sum( total_rent ) / sum( quantity) )


# This will format the data back into a human readable layout
test %>%
  filter(date > ymd("2017-08-01")) %>%
  spread(key = date, value = eff_rent) %>%
  View()


# This 
test %>%
  filter(submarket == "Carmel",
         year(date) >= 2016) %>%
  spread(key = date, value = eff_rent) %>%
  write_csv("test.csv")




# Submarket - Sequential Quarterly Eff Rent TS ----------------------------


# Calculate Eff Rent using the comparable_monthly_mom filter
test <- big_property_data %>%
        filter(comparable_qtr_qoq == TRUE) %>%
        group_by(submarket, current_quarter) %>%
        summarize(prop_count = sum(prop_count) / 3,
                  eff_rent = round( sum( total_rent ) / sum( quantity) , 0 ) )

test %>%
  filter(current_quarter > 2017) %>%
  spread(key = current_quarter, value = eff_rent) %>%
  View()





# Submarket - Eff Rent Quartely YOY ---------------------------------------


# Calculate Eff Rent using the comparable_monthly_mom filter
test <- big_property_data %>%
        filter(comparable_qtr_qoq == TRUE) %>%
        group_by(submarket, current_quarter) %>%
        summarize(prop_count = sum(prop_count) / 3,
                  eff_rent = round( sum( total_rent ) / sum( quantity) , 0 ) )

test %>%
  filter(current_quarter > 2017) %>%
  spread(key = current_quarter, value = eff_rent) %>%
  View()













