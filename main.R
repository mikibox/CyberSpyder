##########################################################################
#
# Intalling libraries
#
##########################################################################

# install.packages("tidyverse")
# install.packages("net:security")
library(ggplot2)
library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyr)
library(net.security)


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
breaches <- separate(breaches, DATE, c('YEAR', 'MONTH', 'DAY'))
breaches$julianday <- yday(breaches$BreachDate) 

# group by month
breachesbymonth <- breaches %>% group_by(MONTH, YEAR) %>% summarise(COUNT = sum(!is.na(PwnCount)))




class(cves$published.datetime)
head(cves$published.datetime, n = 10)

dates <- as.Date(substr(cves$published.datetime,3,12))
cves$months <- month(dates)
head(cves$months)
summary(cves$months)
cvesCountPerMonthTable <- aggregate(cbind(count = cves$cve) ~ cves$months,data = cves, FUN = NROW)

names(cvesCountPerMonthTable)[1] = 'Month'
with(cvesCountPerMonthTable, plot(cvesCountPerMonthTable$count, cvesCountPerMonthTable$cves$months))

#months vector assuming 1st month is Jan.
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
#add abbreviated month name
cvesCountPerMonthTable$MonthNames <- mymonths[ cvesCountPerMonthTable$Month ]


##fechaDate <- as.POSIXct(cves$published.datetime[[1]],format="%Y-%m-%dT%H:%M:%OS")


##########################################################################
#
# VISUALIZING DATASETS
#
##########################################################################
# Basic first plot
with(breachesbymonth, plot(MONTH, COUNT, xlab="Months", ylab="number of attacks"))

# plot the data using ggplot2 and pipes
breachesbymonth %>%
  na.omit() %>%
  ggplot(aes(x = MONTH, y = COUNT)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ YEAR ) +
  labs(title = "Precipitation - Boulder, Colorado",
       subtitle = "Use facets to plot by a variable - year in this case",
       y = "Daily precipitation (inches)",
       x = "Date") + theme_bw(base_size = 15)

# adjust the x axis breaks
# scale_x_date(date_breaks = "5 years", date_labels = "%m-%Y")

# plot data count of cves per month during all years
with(cvesCountPerMonthTable, plot(cvesCountPerMonthTable$count, cvesCountPerMonthTable$cves$months))  
plot(cvesCountPerMonthTable$count, type="o")
hist(as.Date(substr(cves$published.datetime,3,12)), "month")
hist(as.Date(substr(cves$published.datetime,3,12)), "year")