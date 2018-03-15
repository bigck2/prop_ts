library(tidyverse)
library(ggmap)




# Data Prep ---------------------------------------------------------------

# Read in pre-processed data
property_data <- read_rds("property_data.rds")
property_info <- read_rds("property_info.rds")
my_map_roadmap_10 <- read_rds("my_map_roadmap_10.rds")


# Create a data frame that will do analysis based on Subject Property
my_property_info <- property_info

subject <- "327 Sunset"

my_property_info$Subject_Property <- subject 


# Add a column with full Status Description
# Create a Named Vector
my_statuses <- c("S" = "Stabilized", 
                 "LU" = "Lease Up", 
                 "R" = "Rehab", 
                 "UC/LU" = "Under Construction/Lease Up")

#Index the Named Vector by the Status Column
my_property_info$Property_Status <- my_statuses[my_property_info$Status]

rm(my_statuses)


# Determine Subject Property Submarket
subject_submarket <- my_property_info$Submarket[my_property_info$Name == subject]






# Add most recent data and calculate distances to subject --------------------------------------------------

# Filter data to last date and join to property info
max_date <- max(property_data$Date)

my_property_data <- property_data %>%
                    filter(Date == max_date)

rm(max_date)

my_property_info <- left_join(my_property_info, 
                              select(my_property_data, -Property_Status), 
                              by = "ProjID")

rm(my_property_data)


# Include Subject Lon/Lat to calculate distances
subject_dat <- my_property_info %>% 
               filter(Name == Subject_Property)


my_property_info$Subject_Lon <- subject_dat$Longitude
my_property_info$Subject_Lat <- subject_dat$Latitude


rm(subject_dat)


my_property_info <- my_property_info %>%
                    mutate(subject_dist = sqrt((Longitude - Subject_Lon)^2 + (Latitude - Subject_Lat)^2)) %>%
                    arrange(subject_dist)

my_property_info$subject_dist_rank <- 1:nrow(my_property_info)













# Download and save a map
# This part is unessecary because I already saved the map
# my_lon <- mean(my_property_info$Longitude)
# my_lat <- mean(my_property_info$Latitude)
# my_map_roadmap_10 <- get_map(location = c(my_lon, my_lat), 
#                             maptype = "roadmap", 
#                             zoom = 10)
# write_rds(my_map_roadmap_10, "my_map_roadmap_10.rds")




# Visualization helper section --------------------------------------------

# Pick a nice color
library(RColorBrewer)

my_blue <- brewer.pal(9, "Blues")[5]

my_orange <- brewer.pal(9, "Oranges")[3]

my_red <- brewer.pal(9, "Reds")[4]

my_green <- brewer.pal(9, "Greens")[4]





# Basic Visualizations ----------------------------------------------------

# Make a Scatterplot with a large black point for the subject property
ggplot(data = my_property_info,
       aes(x = Longitude, y = Latitude, color = Submarket)) +
  geom_point(data = my_property_info %>% 
               filter(Name == Subject_Property), 
             size = 8, color = "black", alpha = 0.4) +
  geom_point() +
  guides(color = FALSE) +
  coord_map() +
  labs(subtitle = paste("Large Black point is the Subject property:", subject))

ggsave(filename = "Plot 1 - Scatter Plot of properties by submarket.png")




# City map of subject property and all properties colored by Axio Submarket
ggmap(my_map_roadmap_10, 
      extent = "device") +
  geom_point(data = my_property_info %>% filter(Name != Subject_Property), 
             aes(x = Longitude, y = Latitude, color = Submarket), 
             alpha = 0.8) +
  geom_point(data = my_property_info %>% filter(Name == Subject_Property),
             aes(x = Longitude, y = Latitude),
             size = 8, color = "black", alpha = 0.4) +
  labs(subtitle = paste("Large Black point is the Subject property:", subject)) +
  guides(color = FALSE)


ggsave(filename = "Plot 2 - City Map of properties by submarket.png")







# Count of Properties / Units in each Submarket and Market Total-----------------------------------------------------

prop_count_by_status <- my_property_info %>%
                        group_by(Property_Status) %>%
                        summarise(Prop_Count = n())

# Option 2 - only changing the color to my_blue
ggplot(data = prop_count_by_status, 
       aes(x = Property_Status, y = Prop_Count)) +
  geom_bar(stat = "identity", fill = my_blue) +
  geom_text(aes(label = Prop_Count), vjust = -0.25) +
  labs(x = NULL, y = "Properties Count", title = "Property Count by Status")

ggsave(filename = "Plot 3 - Property Count by Status.png")



unit_count_by_status <- my_property_info %>%
                        group_by(Property_Status) %>%
                        summarise(Total_Units = sum(Quantity))



# Option 2 - only changing the color to my_blue
ggplot(data = unit_count_by_status, 
       aes(x = Property_Status, y = Total_Units)) +
  geom_bar(stat = "identity", fill = my_blue) +
  geom_text(aes(label = scales::comma(Total_Units)), vjust = -0.25) +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = NULL, y = "Total Units Count", title = "Unit Count by Status")


ggsave(filename = "Plot 4 - Unit Count by Status.png")

rm(unit_count_by_status, prop_count_by_status)









# Property Count / Unit Count by Submarket --------------------------------

prop_count_by_submarket <- my_property_info %>%
                            group_by(Submarket) %>%
                            summarise(Prop_Count = n())

prop_count_by_submarket$Subject_Market <- prop_count_by_submarket$Submarket == subject_submarket


ggplot(data = prop_count_by_submarket, 
       aes(x = reorder(Submarket, Prop_Count), 
           y = Prop_Count,
           fill = Subject_Market)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(my_blue, my_orange)) +
  guides(fill = FALSE) +
  geom_text(aes(label = Prop_Count), hjust = -0.25) +
  labs(x = NULL, y = "Properties Count", 
       title = "Property Count by Submarket",
       subtitle = paste("The subject property", subject, "is located in the", subject_submarket, "submarket.")) +
  coord_flip() 

ggsave(filename = "Plot 5 - Property Count by Submarket.png")


rm(prop_count_by_submarket)




unit_count_by_submarket <- my_property_info %>%
                            group_by(Submarket) %>%
                            summarise(Total_Units = sum(Quantity))

unit_count_by_submarket$Subject_Market <- unit_count_by_submarket$Submarket == subject_submarket


ggplot(data = unit_count_by_submarket, 
       aes(x = reorder(Submarket, Total_Units), 
           y = Total_Units,
           fill = Subject_Market)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(my_blue, my_orange)) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, max(unit_count_by_submarket$Total_Units + 2500))) + 
  guides(fill = FALSE) +
  geom_text(aes(label = scales::comma(Total_Units)), hjust = -0.25) +
  labs(x = NULL, y = "Total Units", 
       title = "Total Units by Submarket",
       subtitle = paste("The subject property", subject, "is located in the", subject_submarket, "submarket.")) +
  coord_flip() 


ggsave(filename = "Plot 6 - Unit Count by Submarket.png")


rm(unit_count_by_submarket)








# Average Occupancy and Rents by Submarket --------------------------------

# We will look at stabilized properties only
dat <- my_property_info %>% filter(Status == "S")

# Group by Submarket and calculate average Occupancy
submarket_occ <- dat %>%
                 group_by(Submarket) %>%
                 summarize(Average_Occ = mean(Occupancy))


# Need to add Total Market and Subject Property

# Create a dataframe for Total market occupancy
total <- data.frame(Submarket = c("Total"), 
                    Average_Occ = mean(dat$Occupancy))

# Create a dataframe for Subject Property Occupancy
subj <- data.frame(Submarket = subject, 
                   Average_Occ = my_property_info$Occupancy[my_property_info$Name == subject])

# Rbind together
submarket_occ <- rbind(submarket_occ, total, subj)

rm(total, subj)


# Now add a categorical column to designate stuff
submarket_occ$Subject_Market <- NA
submarket_occ$Subject_Market[submarket_occ$Submarket != subject_submarket] <- "Other"
submarket_occ$Subject_Market[submarket_occ$Submarket == subject_submarket] <- "Subject Market"
submarket_occ$Subject_Market[submarket_occ$Submarket == subject] <- "Subject"
submarket_occ$Subject_Market[submarket_occ$Submarket == "Total"] <- "Total"


# Graph of Average Occupancy by Submarket
# ggplot(data = submarket_occ,
#        aes(x = reorder(Submarket, Average_Occ),
#            y = Average_Occ,
#            fill = Subject_Market)) +
#   geom_bar(stat="identity") +
#   scale_fill_manual(values = c(my_blue, my_orange, my_red, my_green)) +
#   guides(fill = FALSE) +
#   geom_text(aes(label = scales::percent(round(Average_Occ, 3))), hjust = -0.25) +
#   labs(x = NULL, y = "Occupancy",
#        title = "Average Occupancy by Submarket (Stabilized Only)",
#        subtitle = paste("The subject property", subject, "is located in the", subject_submarket, "submarket.")) +
#   scale_y_continuous(limits = c(0.85, 0.95),
#                        oob = scales::rescale_none,
#                        labels = scales::percent) +
#   coord_flip()




# Graph of Average Occupancy by Submarket
ggplot(data = submarket_occ, 
       aes(x = reorder(Submarket, Average_Occ), 
           y = Average_Occ,
           color = Subject_Market)) +
  geom_point(stat="identity", size = 2) +
  scale_color_manual(values = c(my_blue, my_orange, my_red, my_green)) + 
  geom_segment(aes(x = reorder(Submarket, Average_Occ), 
                   xend = reorder(Submarket, Average_Occ),
                   y = 0, 
                   yend = Average_Occ),
               size = 2) +
  guides(color = FALSE) + 
  labs(x = NULL, y = "Occupancy", 
       title = "Average Occupancy by Submarket (Stabilized Only)",
       subtitle = paste("The subject property", subject, "is located in the", subject_submarket, "submarket.")) +
  scale_y_continuous(limits = c(min(submarket_occ$Average_Occ) - 0.025, 
                                max(submarket_occ$Average_Occ) + 0.025), 
                     oob = scales::rescale_none,
                     labels = scales::percent) + 
  coord_flip() +
  geom_text(aes(label = scales::percent(round(Average_Occ, 3))), 
            hjust = -0.25, 
            color = "black")


ggsave(filename = "Plot 7 - Average Occupancy by Submarket.png")


rm(submarket_occ)





  


# Eff Rent Analysis

# Group by Submarket and calculate average Rent
submarket_rent <- dat %>%
                  group_by(Submarket) %>%
                  summarize(Average_Rent = mean(Effective_Rent))


# Need to add Total Market and Subject Property

# Create a dataframe for Total market Rent
total <- data.frame(Submarket = c("Total"), 
                    Average_Rent = mean(dat$Effective_Rent))

# Create a dataframe for Subject Property Rent
subj <- data.frame(Submarket = subject, 
                   Average_Rent = my_property_info$Effective_Rent[my_property_info$Name == subject])

# Rbind together
submarket_rent <- rbind(submarket_rent, total, subj)

rm(total, subj)


# Now add a categorical column to designate stuff
submarket_rent$Subject_Market <- NA
submarket_rent$Subject_Market[submarket_rent$Submarket != subject_submarket] <- "Other"
submarket_rent$Subject_Market[submarket_rent$Submarket == subject_submarket] <- "Subject Market"
submarket_rent$Subject_Market[submarket_rent$Submarket == subject] <- "Subject"
submarket_rent$Subject_Market[submarket_rent$Submarket == "Total"] <- "Total"



# Graph of Average Occupancy by Submarket
ggplot(data = submarket_rent, 
       aes(x = reorder(Submarket, Average_Rent), 
           y = Average_Rent,
           color = Subject_Market)) +
  geom_point(stat="identity", size = 2) +
  scale_color_manual(values = c(my_blue, my_orange, my_red, my_green)) + 
  geom_segment(aes(x = reorder(Submarket, Average_Rent), 
                   xend = reorder(Submarket, Average_Rent),
                   y = 0, 
                   yend = Average_Rent),
               size = 2) +
  guides(color = FALSE) + 
  labs(x = NULL, y = "Effective Rent", 
       title = "Average Effective Rent by Submarket (Stabilized Only)",
       subtitle = paste("The subject property", subject, "is located in the", subject_submarket, "submarket.")) +
  scale_y_continuous(limits = c(min(submarket_rent$Average_Rent) - 25, 
                                max(submarket_rent$Average_Rent) + 25), 
                     oob = scales::rescale_none,
                     labels = scales::dollar) + 
  coord_flip() +
  geom_text(aes(label = scales::dollar(round(Average_Rent, 0))), 
            hjust = -0.25, 
            color = "black")


ggsave(filename = "Plot 8 - Average Rent by Submarket.png")
  

rm(submarket_rent)





# Eff Rent PSF Analysis

# Group by Submarket and calculate average Rent
submarket_rent_psf <- dat %>%
                      group_by(Submarket) %>%
                      summarize(Average_Rent_PSF = mean(Effective_Rent_Per_Sq_Ft))


# Need to add Total Market and Subject Property

# Create a dataframe for Total market Rent
total <- data.frame(Submarket = c("Total"), 
                    Average_Rent_PSF = mean(dat$Effective_Rent_Per_Sq_Ft))

# Create a dataframe for Subject Property Rent
subj <- data.frame(Submarket = subject, 
                   Average_Rent_PSF = my_property_info$Effective_Rent_Per_Sq_Ft[my_property_info$Name == subject])

# Rbind together
submarket_rent_psf <- rbind(submarket_rent_psf, total, subj)

rm(total, subj)


# Now add a categorical column to designate stuff
submarket_rent_psf$Subject_Market <- NA
submarket_rent_psf$Subject_Market[submarket_rent_psf$Submarket != subject_submarket] <- "Other"
submarket_rent_psf$Subject_Market[submarket_rent_psf$Submarket == subject_submarket] <- "Subject Market"
submarket_rent_psf$Subject_Market[submarket_rent_psf$Submarket == subject] <- "Subject"
submarket_rent_psf$Subject_Market[submarket_rent_psf$Submarket == "Total"] <- "Total"



# Graph of Average Occupancy by Submarket
ggplot(data = submarket_rent_psf, 
       aes(x = reorder(Submarket, Average_Rent_PSF), 
           y = Average_Rent_PSF,
           color = Subject_Market)) +
  geom_point(stat="identity", size = 2) +
  scale_color_manual(values = c(my_blue, my_orange, my_red, my_green)) + 
  geom_segment(aes(x = reorder(Submarket, Average_Rent_PSF), 
                   xend = reorder(Submarket, Average_Rent_PSF),
                   y = 0, 
                   yend = Average_Rent_PSF),
               size = 2) +
  guides(color = FALSE) + 
  labs(x = NULL, y = "Effective Rent", 
       title = "Average Effective Rent PSF by Submarket (Stabilized Only)",
       subtitle = paste("The subject property", subject, "is located in the", subject_submarket, "submarket.")) +
  scale_y_continuous(limits = c(min(submarket_rent_psf$Average_Rent_PSF) - 0.25, 
                                max(submarket_rent_psf$Average_Rent_PSF) + 0.15), 
                     oob = scales::rescale_none,
                     labels = scales::dollar) + 
  coord_flip() +
  geom_text(aes(label = scales::dollar(round(Average_Rent_PSF, 2))), 
            hjust = -0.25, 
            color = "black")



ggsave(filename = "Plot 9 - Average Rent PSF by Submarket.png")

































# Old Analysis ------------------------------------------------------------






ggmap(my_map_roadmap_10) +
  geom_point(data = my_property_info, 
             aes(x = Longitude, y = Latitude, color = Status), 
             alpha = 0.8)
 

ggmap(my_map_roadmap_10) +
  geom_point(data = my_property_info %>% filter(Property_Status != "Stabilized"), 
             aes(x = Longitude, y = Latitude, color = Property_Status), 
             alpha = 0.8)



# Map of Subject Property and Various Lease Ups
ggmap(my_map_roadmap_10) +
  geom_point(data = my_property_info %>% filter(!Property_Status %in% c("Stabilized", "Rehab")), 
             aes(x = Longitude, y = Latitude, color = Property_Status), 
             alpha = 0.8) +
  geom_point(data = my_property_info %>% filter(Name == Subject_Property),
             aes(x = Longitude, y = Latitude),
             size = 8, color = "black", alpha = 0.4) +
  labs(subtitle = paste("Large Black point is the Subject property:", subject))





# Make a scatteplot over the map
ggmap(my_map_roadmap_10, 
      base_layer = ggplot(data = my_property_info, 
                          aes(x = Longitude, 
                              y = Latitude))) +
  geom_point(data = my_property_info %>% 
                    filter(Name == Subject_Property), 
             size = 8, color = "black", alpha = 0.4) +
  geom_point(aes(color = Submarket), alpha = 0.8) +
  guides(color = FALSE)




# Same thing but only show subject property and properties not stabilized/Rehab
ggmap(my_map_roadmap_10, 
      base_layer = ggplot(data = my_property_info %>% 
                                 filter(!Status %in% c("S", "R"), 
                                        Name != Subject_Property), 
                          aes(x = Longitude, 
                              y = Latitude))) +
  geom_point(data = my_property_info %>% 
                    filter(Name == Subject_Property),
             size = 8, color = "black", alpha = 0.4) +
  geom_point(aes(color = Submarket), alpha = 0.6, size = 4)

rm(my_dat)
















# Summary Stats -----------------------------------------------------------


# Some Useful tables / kables
table(all_data$Submarket)

library(knitr)
kable(table(all_data$Submarket))
kable(table(all_data$Submarket, all_data$Status))





# Data Exploration


ggplot(data = all_data,
       aes(x = Longitude, y = Latitude, size = Effective_Rent, color = Submarket)) +
  geom_point(alpha = 0.6)


# Bad
ggplot(data = all_data,
       aes(x = Longitude, y = Latitude, color = Effective_Rent)) +
  geom_point(alpha = 0.6)


# Effective Rent Histograms
ggplot(data = all_data, aes(x = Effective_Rent)) +
  geom_histogram(fill = "lightblue")

ggplot(data = all_data, aes(x = Effective_Rent, fill = Submarket)) +
  geom_histogram()



# Bin the Effective_Rent into buckets
Rent_Buckets <- cut(all_data$Effective_Rent,
                    breaks = c(0, 500, 750, 1000, 1250, 1500, max(all_data$Effective_Rent)),
                    labels = c("$0-500", "$501-750", "$751-1,000", "$1,000-$1,1250", "$1,251-1,500", ">$1,500"))

all_data$Rent_Buckets <- Rent_Buckets

rm(Rent_Buckets)



# Revised Histograms
ggplot(data = all_data, aes(x = Rent_Buckets)) + 
  geom_bar(fill = "lightblue")

# Ugly histogram
ggplot(data = all_data, aes(x = Rent_Buckets, fill = Submarket)) + 
  geom_bar()



ggplot(data = all_data, aes(x = Longitude, y = Latitude, color = Rent_Buckets))+
  geom_point()



table(all_data$Submarket, all_data$Rent_Buckets)

kable(table(all_data$Submarket, all_data$Rent_Buckets))


table(all_data$Submarket, all_data$Status, all_data$Rent_Buckets)


kable(table(all_data$Submarket, all_data$Status, all_data$Rent_Buckets))

kable(table(all_data$Submarket, all_data$Rent_Buckets, all_data$Status))






































