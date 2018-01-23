

# assumes big_property_data and big_property_info are loaded into workspace


# add a column for prior year month
big_property_data <- big_property_data %>%
                     mutate(prior_year = Date - months(12))

# Create a tmp tbl with mutated columns to join on
tmp <- big_property_data %>%
       select(ProjID, Date, Property_Status) %>%
       rename(prior_year = Date,
              prior_year_status = Property_Status)

test <- left_join(big_property_data, tmp)


test <- test %>%
        mutate(comparable_YOY = (Property_Status == prior_year_status))



