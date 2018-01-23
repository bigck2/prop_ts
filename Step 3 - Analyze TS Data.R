
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





# Submarket - Eff Rent TS -------------------------------------------------


test <- big_property_data %>%
        filter(comparable_monthly_MOM == TRUE) %>%
        group_by(Submarket, Date) %>%
        summarize(eff_rent = sum( total_rent ) / sum( Quantity) )


























