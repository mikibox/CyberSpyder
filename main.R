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
cves <- cves[1:100,]


##########################################################################
#
# EXPLORING & TRANSFORMING DATASETS
#
##########################################################################
process_dates <- function(datafinal, dataframe, datecolumn, dateformat, datatype){
  
  dataframe[,datecolumn] <- as.Date(dataframe[,datecolumn], dateformat)
  dataframe$DATE <- dataframe[,datecolumn]
  dataframe <- separate(dataframe, DATE, c('YEAR', 'MONTH', 'DAY'))
  dataframe$julianday <- yday(dataframe[,datecolumn]) 
  dfresult <- select(dataframe, c('YEAR', 'MONTH', 'DAY', 'julianday'))
  dfresult$TYPE <- datatype
  datafinal <- bind_rows(datafinal,dfresult)
}

head(c(breaches, attacks2018))

spyder <- data.frame(row.names = c('TYPE','YEAR', 'MONTH', 'DAY', 'julianday'))
spyder <- process_dates(spyder, breaches, "BreachDate", "%Y-%m-%d", "breaches")
spyder <- process_dates(spyder, attacks2018, "Date", "%d/%m/%Y", "attacks")
spyder <- process_dates(spyder, attacks2017, "Date", "%d/%m/%Y", "attacks")
spyder <- process_dates(spyder, cves, "published.date", "%d/%m/%Y", "cves")


# group by month and year
breachesbymonth <- breaches %>% group_by(MONTH) %>% summarise(COUNT = sum(!is.na(MONTH)))
attacks2017bymonth <- attacks2017 %>% group_by(MONTH) %>% summarise(COUNT = sum(!is.na(MONTH)))
attacks2018bymonth <- attacks2018 %>% group_by(MONTH) %>% summarise(COUNT = sum(!is.na(MONTH)))

breachesbymonthyear <- breaches %>% group_by(MONTH, YEAR) %>% summarise(COUNT = sum(!is.na(MONTH)))
attacks2017bymonthyear <- attacks2017 %>% group_by(MONTH, YEAR) %>% summarise(COUNT = sum(!is.na(MONTH)))
attacks2018bymonthyear <- attacks2018 %>% group_by(MONTH, YEAR) %>% summarise(COUNT = sum(!is.na(MONTH)))



##########################################################################
#
# VISUALIZING DATASETS
#
##########################################################################
# Basic first plot
with(breachesbymonth, plot(MONTH, COUNT, xlab="Months", ylab="number of attacks"))

# plot the data using ggplot2 and pipes
attacks2017bymonthyear %>%
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
