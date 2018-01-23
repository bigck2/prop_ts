
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
                     mutate(comparable_YOY = (Property_Status == "S") & (prior_year_status == "S"))

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
                      mutate(comparable_MOM = (Property_Status == "S") & (prior_month_status == "S") )

rm(tmp)





# Determine if every month in Quarter is comparable -----------------------

# Note I think I have to do each month seperately to do the joins correctly
# Not sure though I might be able to cleverly do both at once

# Note I don't hink I actually need to determine prior_month_3 or prior_month_3_status

# Note I will probably need a lubridate function to get the Year-Quarter



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

# TODO Delete this section after confirming it isn't needed.
      # # add a column for prior_month_3
      # big_property_data <- big_property_data %>%
      #                       mutate(prior_month_3 = Date - months(3))
      # 
      # 
      # # Create a tmp tbl with mutated columns to join on
      # tmp <- big_property_data %>%
      #         select(ProjID, Date, Property_Status) %>%
      #         rename(prior_month_3 = Date,
      #                prior_month_3_status = Property_Status)
      # 
      # # Now join the tmp tbl to big_property_data
      # big_property_data <- left_join(big_property_data, tmp)
      # 
      # rm(tmp)


# To determine if the property is comparable I only need to look 
# at the last month of each quarter (Mar, Jun, Sep, Dec)
# in that month I can check the property was stabilized for each month in the Quarter

# First let's add a current_quarter column
big_property_data <- big_property_data %>%
                     mutate(current_quarter = quarter(Date, with_year = TRUE))



tmp <- big_property_data %>%
       mutate(month_no = month(Date)) %>%
       filter(month_no %in% c(3, 6, 9, 12)) %>%
       mutate(quarterly_stabilized = ((Property_Status == "S") & (prior_month_status == "S") & (prior_month_2_status == "S"))) %>%
       select(ProjID, current_quarter, quarterly_stabilized)

View(tmp)


big_property_data <- left_join(big_property_data, tmp)

rm(tmp)






