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

breaches$month <- month(breaches$BreachDate)
breaches$day <- day(breaches$BreachDate)
breaches$julian <- yday(breaches$BreachDate) 

# group by month
breaches.groupedbymonth <- table(breaches$month)




##########################################################################
#
# VISUALIZING DATASETS
#
##########################################################################
# Basic first plot
with(df, plot(month, PwnCount))

