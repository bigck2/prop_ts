
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




# Comparable YOY ----------------------------------------------------------

# add a column for prior year month
big_property_data <- big_property_data %>%
                     mutate(prior_year = Date - months(12))

# Create a tmp tbl with mutated columns to join on
tmp <- big_property_data %>%
       select(ProjID, Date, Property_Status) %>%
       rename(prior_year = Date,
              prior_year_status = Property_Status)

# Now join the tmp tbl to big_property_data
big_property_data <- left_join(big_property_data, tmp)

# Determine if the property should be designated comparable_YOY
big_property_data <- big_property_data %>%
                     mutate(comparable_monthly_YOY = (Property_Status == "S") & (prior_year_status == "S"))

rm(tmp)




# Comparable MOM ----------------------------------------------------------

# add a column for prior month
big_property_data <- big_property_data %>%
                     mutate(prior_month = Date - months(1))


# Create a tmp tbl with mutated columns to join on
tmp <- big_property_data %>%
        select(ProjID, Date, Property_Status) %>%
        rename(prior_month = Date,
               prior_month_status = Property_Status)

# Now join the tmp tbl to big_property_data
big_property_data <- left_join(big_property_data, tmp)


# Determine if the property should be designated comparable_MOM
big_property_data <- big_property_data %>%
                      mutate(comparable_monthly_MOM = (Property_Status == "S") & (prior_month_status == "S") )

rm(tmp)





# Determine if every month in Quarter is comparable -----------------------

# add a column for prior_month_2
big_property_data <- big_property_data %>%
                     mutate(prior_month_2 = Date - months(2))

# Create a tmp tbl with mutated columns to join on
tmp <- big_property_data %>%
        select(ProjID, Date, Property_Status) %>%
        rename(prior_month_2 = Date,
               prior_month_2_status = Property_Status)

# Now join the tmp tbl to big_property_data
big_property_data <- left_join(big_property_data, tmp)

rm(tmp)

# To determine if the property is comparable I only need to look 
# at the last month of each quarter (Mar, Jun, Sep, Dec)
# in that month I can check the property was stabilized for each month in the Quarter

# First let's add a current_quarter column
big_property_data <- big_property_data %>%
                     mutate(current_quarter = quarter(Date, with_year = TRUE))

# Add a month_no columns, filter to end of quarter months,
# then check to make sure property_status, prior_month_status, and prior_month_2_status
# are all Stabilized. Lastlty select key columns to use in join
tmp <- big_property_data %>%
       mutate(month_no = month(Date)) %>%
       filter(month_no %in% c(3, 6, 9, 12)) %>%
       mutate(quarterly_stabilized = ((Property_Status == "S") & (prior_month_status == "S") & (prior_month_2_status == "S"))) %>%
       select(ProjID, current_quarter, quarterly_stabilized)

# Now join the tmp tbl to big_property_data
big_property_data <- left_join(big_property_data, tmp)

rm(tmp)




# Quarterly Comparable YOY ------------------------------------------------

# In order to find the prior year quarter or prior sequential quarter I should modify the
# date variable. To shift back to prior quarter subtract months(3), to find prior year
# quarter subtract years(1)
# Then calculate quarter

big_property_data <- big_property_data %>%
                     mutate(prior_year_quarter_date = Date - years(1),
                            prior_year_quarter = quarter(prior_year_quarter_date, 
                                                         with_year = TRUE) )

# Here I think I need to add a date column, so the join will use ProjID, prior_year_quarter,
# and date
# With only ProjID and prior_year_quarter we get repeated rows

tmp <- big_property_data %>%
        select(ProjID, Date, quarterly_stabilized) %>%
        rename(prior_year_quarter_date = Date,
               prior_year_quarterly_stabilized = quarterly_stabilized)

big_property_data <- left_join(big_property_data, tmp)

rm(tmp)


# Now we can check to make sure the property is comparable to the 
# same quarter prior year
big_property_data <- big_property_data %>%
                     mutate(comparable_qtr_YOY = (quarterly_stabilized & prior_year_quarterly_stabilized))




# Quarterly Comparable QoQ ------------------------------------------------

big_property_data <- big_property_data %>%
                      mutate(prior_quarter_date = Date - months(3),
                             prior_quarter = quarter(prior_quarter_date, 
                                                          with_year = TRUE) )


tmp <- big_property_data %>%
        select(ProjID, Date, quarterly_stabilized) %>%
        rename(prior_quarter_date = Date,
               prior_quarter_stabilized = quarterly_stabilized)

big_property_data <- left_join(big_property_data, tmp)

rm(tmp)

# Now we can check to make sure the property is comparable to the 
# prior quarter
big_property_data <- big_property_data %>%
                      mutate(comparable_qtr_QOQ = (quarterly_stabilized & prior_quarter_stabilized))




# Prep for analysis ----------------------------------------------------

# Drop intermediate variables

# Make a character vector of columns I don't need anymore
bad_variables <- c("prior_year", "prior_year_status", "prior_month",
                   "prior_month_status", "prior_month_2", "prior_month_2_status",
                   "prior_year_quarter_date", "prior_year_quarter", 
                   "prior_year_quarterly_stabilized", "prior_quarter_date",
                   "prior_quarter", "prior_quarter_stabilized")


# Drop these intermediate columns
big_property_data <- big_property_data %>%
                      select(-one_of(bad_variables)) # Had to look up this helper function one_of()


rm(bad_variables)



# Create a tmp tbl with variables I plan to join to big_property_data

my_vars <- c("ProjID","Name", "Msa_Name", "Submarket", "Quantity", "AreaPerUnit")

tmp <- big_property_info %>%
       select(one_of(my_vars))

# Join these variables to big_property_data
big_property_data <- left_join(big_property_data, tmp)

rm(tmp, my_vars)


# We need to add a few calculated columns
# first convert to numeric data types
big_property_data <- big_property_data %>%
                     mutate(total_sf = as.numeric(Quantity) * as.numeric(AreaPerUnit),
                            total_rent = as.numeric(Effective_Rent) * as.numeric(Quantity),
                            occupied_units = as.numeric(Occupancy) * as.numeric(Quantity))


# This is also temporary while finishing the script for Step 2
write_rds(big_property_data, "big_property_data.rds")
write_rds(big_property_info, "big_property_info.rds")





# Ideas -------------------------------------------------------------------


# IDEA: If I seperated year and month into two seperate columns and then spread the month
# variables into columns I think I could quickly detemine if the property
# was stabilized for the entire year

# IDEA: This idea above would probably have worked for the quarters, I could test this to see
# if I get the same result, but it would be a little bit trickier and my script currently
# works so there may not be a big benefit























