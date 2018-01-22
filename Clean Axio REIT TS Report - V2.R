

# Load needed packages ################################################

library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)


# Load the Data ########################################################







# Get Filenames
fname <- list.files()

counter = 0


for (i in 1:length(fname)){  
  
  # Check for excel files
  
  if (!str_detect(string = fname[i], pattern = "\\.")) {next}
  
  file_extenstion_pos = str_locate(string = fname[i], pattern = "\\.")[1] + 1
  
  if (!(str_sub(string = fname[i], start = file_extenstion_pos) == "xls"  |
        str_sub(string = fname[i], start = file_extenstion_pos) == "xlsx" )) {next}
  
  counter = counter + 1




for (y in 1:4) {

# Read in data 
mydata <- read_excel(fname[i], sheet = y)


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


# Create link column 

# mydata <- mutate(mydata, Link = paste(ProjID, Date))


if (y == 1) {
  
  bigdata <- mydata
  
  } else {
  
  bigdata <- left_join(bigdata, mydata)
  
}


}



if (counter == 1) { 
  
  big_property_info <- property_info
  
  big_property_data <- bigdata
  
  
} else {  
    
  big_property_info <- rbind(big_property_info, property_info)
  
  big_property_data <- rbind(big_property_data, bigdata)
  
  }
  
  
  
}  




# Get rid of all objects except big data and big_property_info

rm(list= ls()[!(ls() %in% c('big_property_data','big_property_info'))])



# I'd like to clean up the Asset Grades to make it simpler



big_property_data <- big_property_data %>%
                     mutate(Simple_Property_MSA_Grade = str_replace_all(string = Property_MSA_Grade,
                                                                       pattern = "\\+", 
                                                                       replacement = ""),
                            Simple_Property_Submarket_Grade = str_replace_all(string = Property_Submarket_Grade,
                                                                       pattern = "\\+", 
                                                                       replacement = ""),
                            Simple_Submarket_Grade = str_replace_all(string = Submarket_Grade,
                                                                       pattern = "\\+", 
                                                                       replacement = ""))


big_property_data <- big_property_data %>%
                    mutate(Simple_Property_MSA_Grade = str_replace_all(string = Simple_Property_MSA_Grade,
                                                                      pattern = "-", 
                                                                      replacement = ""),
                           Simple_Property_Submarket_Grade = str_replace_all(string = Simple_Property_Submarket_Grade,
                                                                            pattern = "-", 
                                                                            replacement = ""),
                           Simple_Submarket_Grade = str_replace_all(string = Simple_Submarket_Grade,
                                                                    pattern = "-", 
                                                                    replacement = ""))





# I want to find a first date stabilized for each property

data_stabilized <- big_property_data %>%
                  filter(Property_Status == "S") %>%
                  group_by(ProjID) %>%
                  summarise(Earliest_Stabilized = min(Date))



# I also want to find the latest asset grades
data_grade <- big_property_data %>%
              filter(Date == max(Date)) %>%
              select(ProjID, 
                     Property_MSA_Grade, 
                     Property_Submarket_Grade, 
                     Submarket_Grade,
                     Simple_Property_MSA_Grade,
                     Simple_Property_Submarket_Grade,
                     Simple_Submarket_Grade)



# Let's join these together (use data_grade as the left table because it might have more records)

attributes <- left_join(data_grade, data_stabilized)


# remove intermediate tables

rm(data_grade, data_stabilized)





# Create simple function to first day in quarter

beg_qtr <- function(mydate){
  
  beg_qtr <- update(object = mydate, 
             month = ceiling(month(mydate)/3)*3 - 2, 
             mday = 1)
  
  beg_qtr
  
  
}



# Need to figure out month it would be Same Store Status

attributes <- attributes %>%
              mutate(Earliest_Same_Store = beg_qtr(Earliest_Stabilized) + months(3) + years(1)) 







# Add attrutes to big_property_info data frame
big_property_info <- left_join(big_property_info, attributes)


rm(attributes)


# Filter out bad rows

big_property_info <- big_property_info %>%
                      filter(!is.na(ProjID))


# Fix Properties with NA Earliest Stabilized / Earliest Same Store

big_property_info[is.na(big_property_info$Earliest_Same_Store), "Earliest_Same_Store"] <- mdy("12/31/2199")



# Now I'd like to add some calculated columns



# First I need to join the units from Property_info to Property_data

units <- select(big_property_info, ProjID, Quantity)


big_property_data <- left_join(big_property_data, units)


rm(units)


big_property_data <- big_property_data %>%
                     mutate(Quantity = as.numeric(Quantity),
                            Occupancy = as.numeric(Occupancy),
                            Effective_Rent = as.numeric(Effective_Rent),
                            Occupied_Units = Quantity * Occupancy,
                            Rental_Income = Occupied_Units * Effective_Rent)






# Check if "Clean Data" directory exists and if not, then create one

if (!dir.exists("Clean Data")) {
  
  dir.create("Clean Data")
  
}

# Go to Clean Data directory

setwd("./Clean Data")


# save down CSV data files

write.csv(big_property_data, "Cleaned Data.csv")

saveRDS(big_property_data, "big_property_data.rds")

write.csv(big_property_info, "Property Info Cleaned Data.csv")

saveRDS(big_property_info, "big_property_info.rds")

# Move back up to parent directory

setwd("..")


















  