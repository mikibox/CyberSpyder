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

if (!file.exists("data/hackmaggedon2017.csv")){
  print("Missing data files")
}
if (!file.exists("data/hackmaggedon2018.csv")){
  print("Missing data files")
}

# breaches json
breaches <- fromJSON("https://query.data.world/s/hlrbfrljlgetudr6zbzv4cdv7446qb")


# Hackmagedon csv
attacks2017 <- read.csv(file = "data/hackmaggedon2017.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
attacks2018 <- read.csv(file = "data/hackmaggedon2018.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# CVEs rda
if (!file.exists("data/sysdata.rda")){
  fileUrl <- "https://github.com/r-net-tools/security.datasets/raw/master/net.security/sysdata.rda"
  download.file(url = fileUrl, destfile = "data/sysdata.rda")
}
load("data/sysdata.rda")
cves <- netsec.data$datasets$cves



##########################################################################
#
# EXPLORING & TRANSFORMING DATASETS
#
##########################################################################
process_dates <- function(dataframe, datecolumn, dateformat){
  dataframe[,datecolumn] <- as.Date(dataframe[,datecolumn], dateformat)
  dataframe$DATE <- dataframe[,datecolumn]
  dataframe <- separate(dataframe, DATE, c('YEAR', 'MONTH', 'DAY'))
  dataframe$julianday <- yday(dataframe[,datecolumn]) 
  dataframe
}

head(breaches)
str(breaches)

breaches <- process_dates(breaches, "BreachDate", "%Y-%m-%d")
attacks2018 <- process_dates(attacks2018, "Date", "%d/%m/%Y")

# group by month
breachesbymonth <- breaches %>% group_by(MONTH, YEAR) %>% summarise(COUNT = sum(!is.na(PwnCount)))



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
  filter(YEAR>2010) %>%
  ggplot(aes(x = MONTH, y = COUNT)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ YEAR ) +
  labs(title = "Precipitation - Boulder, Colorado",
       subtitle = "Use facets to plot by a variable - year in this case",
       y = "Daily precipitation (inches)",
       x = "Date") + theme_bw(base_size = 15)

  # adjust the x axis breaks
  # scale_x_date(date_breaks = "5 years", date_labels = "%m-%Y")
