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
head(df)
unique(unlist(df$DataClasses))
install.packages("lubridate")

df$month <- lubridate::month(df$BreachDate)



##########################################################################
#
# VISUALIZING DATASETS
#
##########################################################################
# Basic first plot
with(df, plot(month, PwnCount))
