

# Load needed packages ################################################

library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)


# Load the Data ########################################################







fname <- choose.files()







for (y in 1:4) {

    # Read in data 
    mydata <- read_excel(fname, sheet = y)
    
    
    #Remove first rows
    mydata <- mydata[2:nrow(mydata), ]
    
    
    #Find last blank column
    headers <- as.character(mydata[1,])
    
    stop_places <- which(!is.na(headers))
    
    
    # Clean character data
    headers <- str_replace_all(string = headers, pattern = "\\.", replacement = "")
    
    headers <- str_replace_all(string = headers, pattern = " ", replacement = "_")
    
    
    # Need to put this in a data frame to use fill
    head_df <- data.frame(Labels = headers, stringsAsFactors = FALSE)
    
    
    #fill down values
    head_df <- fill(head_df, Labels)
    
    headers <- head_df$Labels
    
    rm(head_df)
    
    
    #create a character vector with 2nd row
    first_row <- as.character(mydata[2,])
    
    
    # concactenate first_row with headers after the stop_paces point
    # this will combine together the ctageories and the dates
    first_row[stop_places[1]:length(first_row)] <- paste(headers[stop_places[1]:length(first_row)], 
                                                         first_row[stop_places[1]:length(first_row)],
                                                        sep = " | ")
    
    
    # Remove rows 1 and 2
    mydata <- mydata[3:nrow(mydata),]
    
    # Name the columns with the first_row vector
    names(mydata) <- first_row
    
    #Remove the unneeded objects
    rm(first_row, headers)
    
    
    # create a seperate property_info table/object
    property_info <- mydata[ , 1:(stop_places[1] - 1)]
    
    
    # Which column is the ProjId
    ID <- which(names(mydata) == "ProjID")
    
    
    # Select only the imporant columns
    mydata <- select(mydata, ID, stop_places[1]:ncol(mydata))
    
    rm(ID, stop_places)
    
    
    
    # gather the columns into only 2
    mydata <- gather(mydata, "variable", "value", 2:ncol(mydata))
    
    
    # filter to non-NA value
    mydata <- filter(mydata, !is.na(value))
    
    
    # seperate the "variable column into multiple columns 
    mydata <- separate(mydata, col = variable, sep = "\\|", into = c("Category", "Date_String"))
    
    # Trim spaces
    mydata$Category <- str_trim(mydata$Category)
    
    mydata$Date_String <- str_trim(mydata$Date_String)
    
    
    
    # Clean up dates
    mydata <- separate(mydata, col = Date_String, sep = "-", into = c("Month", "Year"))
    
    mydata <- mutate(mydata, Date_String = paste(Month, " 01, 20", Year, sep = ""))
    
    mydata <- mutate(mydata, Date = mdy(Date_String))
    
    mydata <- select(mydata, -Month, -Year, -Date_String)
    
    
    # Now spread the Category Column
    mydata <- spread(mydata, key = Category, value = value)
    
    
    
        if (y == 1) {
          
          bigdata <- mydata
          
          } else {
          
          bigdata <- left_join(bigdata, mydata)
          
        }


}



# Convert numeric variables to numeric data type (from character)
bigdata <- bigdata %>%
           mutate(Asking_Rent = as.numeric(Asking_Rent),
                  Effective_Rent = as.numeric(Effective_Rent),
                  Effective_Rent_Per_Sq_Ft = as.numeric(Effective_Rent_Per_Sq_Ft),
                  Concession_percent = as.numeric('Concession_%'),
                  Concession_Value = as.numeric(Concession_Value),
                  Occupancy = as.numeric(Occupancy))

# Convert Longitude and Latitude from character to numeric type
property_info <- property_info %>%
                 mutate(Longitude = as.numeric(Longitude),
                        Latitude = as.numeric(Latitude),
                        YearBuilt = as.numeric(YearBuilt),
                        Quantity = as.numeric(Quantity),
                        AreaPerUnit = as.numeric(AreaPerUnit))



# Rename the object
property_data <- bigdata


# Get rid of all objects except big data and property_info
rm(list= ls()[!(ls() %in% c('property_data','property_info'))])




# This step may not be necessary
# write_csv(big_property_data, "big_property_data.csv")
# write_csv(big_property_info, "big_property_info.csv")


# This is also temporary while finishing the script for Step 2
write_rds(property_data, "property_data.rds")
write_rds(property_info, "property_info.rds")
















  