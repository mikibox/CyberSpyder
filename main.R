##########################################################################
#
# READING DATASETS
#
##########################################################################
# Download & Read Databreaches csv
if (!file.exists("data/data.csv")){
  fileUrl <- "https://query.data.world/s/bropxfkodejrbsh3hkj3iw274pjc4o"
  download.file(url = fileUrl, destfile = "data/data.csv")
} else {
  print(TRUE)
}

# Read Databreaches json
library(jsonlite)
df <- fromJSON("https://query.data.world/s/hlrbfrljlgetudr6zbzv4cdv7446qb")
df$BreachDate <- as.Date(df$BreachDate)
df$AddedDate <- as.Date(df$AddedDate)
summary(df)

# Hackmagedon csv
if (!file.exists("data/attacks2015.csv")){
  fileUrl <- "http://www.hackmageddon.com/wp-content/uploads/2015/07/16-30-June-2015-Cyber-Attack-Timeline.csv"
  download.file(url = fileUrl, destfile = "data/attacks2015.csv")
}


attacksdf <- read.csv(file = "data/2017table.csv",
                      header = TRUE,
                      sep = ",")


##########################################################################
#
# EXPLORING DATASETS
#
##########################################################################

install.packages("net:security")
library("net:security")

class(cves$published.datetime)
cves$
head(cves$published.datetime, n = 10)
install.packages("lubridate")
library("lubridate")



dates <- as.Date(substr(cves$published.datetime,3,12))
cves$months <- month(dates)
head(cves$months)
summary(cves$months)
table <- aggregate(cbind(count = cves$cve) ~ cves$months,data = cves, FUN = NROW)

names(table)[1] = 'Month'
with(table, plot(table$count, table$cves$months))
plot(table$count, type="o")
#months vector assuming 1st month is Jan.
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
#add abbreviated month name
table$MonthNames <- mymonths[ table$Month ]
plot(table$count, type="o")

##fechaDate <- as.POSIXct(cves$published.datetime[[1]],format="%Y-%m-%dT%H:%M:%OS")

hist(as.Date(substr(cves$published.datetime,3,12)), "month")
hist(as.Date(substr(cves$published.datetime,3,12)), "year")

install.packages("ggplot2")
library("ggplot2")
install.packages("scales")
library("scales")



as.Date(cves$published.datetime, "%d%B%Y")




##########################################################################
#
# VISUALIZING DATASETS
#
##########################################################################
# Basic first plot
with(df, plot(BreachDate, PwnCount))

