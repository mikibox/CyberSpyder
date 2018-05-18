##########################################################################
#
# Intalling libraries
#
##########################################################################

# install.packages("tidyverse")

library(ggplot2)
library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyr)



##########################################################################
#
# READING DATASETS
#
##########################################################################
options(stringsAsFactors = FALSE)
if (!dir.exists("data")){
  dir.create("data")
}


# Read breaches json
breaches <- fromJSON("https://query.data.world/s/hlrbfrljlgetudr6zbzv4cdv7446qb")


# Hackmagedon csv
if (!file.exists("data/attacks2015.csv")){
  fileUrl <- "http://www.hackmageddon.com/wp-content/uploads/2015/07/16-30-June-2015-Cyber-Attack-Timeline.csv"
  download.file(url = fileUrl, destfile = "data/attacks2015.csv")
}


# attacksdf <- read.csv(file = "data/2017table.csv", header = TRUE, sep = ",")


##########################################################################
#
# EXPLORING & TRANSFORMING DATASETS
#
##########################################################################
head(breaches)
str(breaches)

breaches$BreachDate <- as.Date(breaches$BreachDate)
class(breaches$BreachDate)
breaches$DATE <- breaches$BreachDate
breaches <- separate(breaches, DATE, c('day', 'month', 'year'))
breaches$julian <- yday(breaches$BreachDate) 

# group by month
breaches.groupedbymonth <- table(breaches$month)




##########################################################################
#
# VISUALIZING DATASETS
#
##########################################################################
# Basic first plot
with(breaches, plot(month, PwnCount))

# plot the data using ggplot2 and pipes
boulder_daily_precip %>%
  na.omit() %>%
  ggplot(aes(x = DATE, y = DAILY_PRECIP)) +
  geom_point(color = "darkorchid4") +
  facet_wrap( ~ YEAR ) +
  labs(title = "Precipitation - Boulder, Colorado",
       subtitle = "Use facets to plot by a variable - year in this case",
       y = "Daily precipitation (inches)",
       x = "Date") + theme_bw(base_size = 15) +
  # adjust the x axis breaks
  scale_x_date(date_breaks = "5 years", date_labels = "%m-%Y")
