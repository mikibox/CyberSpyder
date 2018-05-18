##########################################################################
#
# Intalling libraries
#
##########################################################################

install.packages("tidyverse")

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

breaches$julian <- yday(breaches$BreachDate) 
breaches <- separate(breaches, BreachDate, c('day', 'month', 'year'))

# group by month
breaches.groupedbymonth <- table(breaches$month)




##########################################################################
#
# VISUALIZING DATASETS
#
##########################################################################
# Basic first plot
with(df, plot(month, PwnCount))

# plot the data using ggplot2 and pipes
breaches %>%
  ggplot(aes(x = julian, y = PwnCount)) +
  geom_point(color = "darkorchid4") +
  facet_wrap( ~ YEAR, ncol = 3) +
  labs(title = "Daily Precipitation - Boulder, Colorado",
       subtitle = "Data plotted by year",
       y = "Daily Precipitation (inches)",
       x = "Day of Year") + theme_bw(base_size = 15)
