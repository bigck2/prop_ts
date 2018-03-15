
# 2018-03-14
# Note this code below all worked fine with the CRAN version of ggmap 2.6.1
# However using the Github version of the package 2.7 the call to mapdist()
# was returning an error message and a list






library(ggmap)
library(tidyverse)




my_address <- "2690 152nd Ave NE, Redmond, WA 98052"

geocode(my_address)

geocode(data$Address[2])





good_distances <- mapdist(from = "2690 152nd Ave NE, Redmond, WA 98052",
                          to = "8397 158th Ave NE, Redmond, WA 98052")

good_distances2 <- mapdist(from = c("2690 152nd Ave NE, Redmond, WA 98052", 
                                    "16175 Cleveland St, Redmond, WA 98052"),
                           to = c("8397 158th Ave NE, Redmond, WA 98052", 
                                  "17634 NE Union Hill Rd, Redmond, WA 98052"))



bad_distances3 <- mapdist(from = "2690 152nd Ave NE, Redmond, WA 98052",
                          to = c("8397 158th Ave NE, Redmond, WA 98052", 
                                 "17634 NE Union Hill Rd, Redmond, WA 98052"))


bad_distances4 <- mapdist(from = rep("2690 152nd Ave NE, Redmond, WA 98052", 2),
                          to = c("8397 158th Ave NE, Redmond, WA 98052", 
                                 "17634 NE Union Hill Rd, Redmond, WA 98052"))


bad_distances5 <- mapdist(from = c("2690 152nd Ave NE, Redmond, WA 98052", 
                                   "2690 152nd Ave NE, Redmond, WA 98052"),
                          to = c("8397 158th Ave NE, Redmond, WA 98052", 
                                 "17634 NE Union Hill Rd, Redmond, WA 98052"))



addresses <- c("11011 ne 9th st, bellevue, wa 98004, usa", "8397 158th ave ne, redmond, wa 98052, usa", 
               "2690 152nd ave ne, redmond, wa 98052, usa", "10349 ne 10th st, bellevue, wa 98004, usa", 
               "324 central way, kirkland, wa 98033, usa", "16325 cleveland st, redmond, wa 98052, usa", 
               "10715 ne 37th ct, kirkland, wa 98033, usa", "2170 bel-red rd, bellevue, wa 98007, usa", 
               "825 156th ave ne, bellevue, wa 98007, usa", "10505 main st, bellevue, wa 98004, usa", 
               "8300 160th ave ne, redmond, wa 98052, usa", "16175 cleveland st, redmond, wa 98052, usa", 
               "11101 ne 12th st, bellevue, wa 98004, usa", "88 102nd ave ne, bellevue, wa 98004, usa", 
               "17634 ne union hill rd, redmond, wa 98052, usa", "16015 cleveland st, redmond, wa 98052, usa", 
               "7977 170th ave ne, redmond, wa 98052, usa", "10701 main st, bellevue, wa 98004, usa", 
               "1201 121st avenue northeast, bellevue, wa 98005, usa", "11 main st s, kirkland, wa 98033, usa", 
               "7508 159th pl ne, redmond, wa 98052, usa", "8280 164th ave ne, redmond, wa 98052, usa", 
               "10333 main st, bellevue, wa 98004, usa", "500 bellevue way ne, bellevue, wa 98004, usa", 
               "10333 ne 1st st, bellevue, wa 98004, usa", "595 156th ave se, bellevue, wa 98007, usa")


bad_distances_my_goal <- mapdist(from = "2690 152nd Ave NE, Redmond, WA 98052",
                                 to = addresses)







# CHeck your limits
distQueryCheck()
geocodeQueryCheck()









