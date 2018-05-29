##########################################################################
#
# Intalling libraries
#
##########################################################################

necessaryPackages <- c("bindrcpp", "plotly", "ggplot2", "dplyr", "lubridate", "jsonlite", "tidyr", "tidyverse")

#' Install all necessary packages which are not yet installed
#' @param packages a list of string
#' @return  Print of the result of packages installation
#' installPackages(c("plotly"))
installPackages <- function(packages){
  list.of.packages <- packages
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  library(jsonlite)
  library(tidyr)
}

# installPackages(necessaryPackages)


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
# making a small sample of the cves
# cves <- cves[1:100,]


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

head(c(breaches, attacks2017))

#' Loads project's main dataframe with all datasets configured for the project.
#' This maindataframe normalizes all datasets rows' counts of different types.
#' in a Percentage column named "PER" which represents the percentage of rows 
#' of same type in a particular month of the year.
#' Relevant Variables:
#' -spyderbymonthyear: all datasets percentage by each month and year.
#' -spyderbymonthspred: all datasets percentages by each month of the year normalized
#' with label qualification
#' -spyderTotals: total count of each dataset occurances
loadMainDataFrame <- function(){
  spyder <- data.frame(row.names = c('TYPE','YEAR', 'MONTH', 'DAY', 'julianday'))
  spyder <- process_dates(spyder, breaches, "BreachDate", "%Y-%m-%d", "breaches")
  
  # spyder <- process_dates(spyder, attacks2018, "Date", "%d/%m/%Y", "attacks")
  spyder <- process_dates(spyder, attacks2017, "Date", "%d/%m/%Y", "attacks")
  spyder <- process_dates(spyder, cves, "published.date", "%d/%m/%Y", "cves")
  spyder <- drop_na(spyder)
  
  # group by month and year
  spyderTotals <- spyder %>% group_by(TYPE) %>%
    summarise(TOTAL = sum(!is.na(TYPE))) %>% rename(input_type = TYPE, input_total = TOTAL)
  spyderbymonth <- 
    spyder %>% 
    group_by(TYPE, MONTH) %>% 
    summarise(COUNT = sum(!is.na(TYPE)),
              PER = COUNT / spyderTotals$input_total[spyderTotals$input_type == TYPE][1])
  spyderbymonth$PER <- lapply(spyderbymonth$PER, function(x) x*100)
  spyderbymonthyear <- spyder %>% group_by(TYPE,MONTH, YEAR) %>% summarise(COUNT = sum(!is.na(MONTH)))%>% mutate(PERCENTAGE = COUNT/sum(COUNT))
  spyderbymonthspred <- spread(spyderbymonth[c("TYPE","MONTH","PER")], TYPE, PER)
}

spyder <- drop_na(spyder)


# group by month and year
spyderTotals <- spyder %>% group_by(TYPE) %>% summarise(TOTAL = sum(!is.na(TYPE))) %>% rename(input_type = TYPE, input_total = TOTAL)

spyderbymonth <- 
  spyder %>% 
  group_by(TYPE, MONTH) %>% 
  summarise(COUNT = sum(!is.na(TYPE)),
            PER = COUNT / spyderTotals$input_total[spyderTotals$input_type == TYPE][1])

spyderbymonth$PER <- lapply(spyderbymonth$PER, function(x) x*100)
spyderbymonthyear <- spyder %>% group_by(TYPE,MONTH, YEAR) %>% summarise(COUNT = sum(!is.na(MONTH)))%>% mutate(PERCENTAGE = COUNT/sum(COUNT))

spyderbymonthspred <- spread(spyderbymonth[c("TYPE","MONTH","PER")], TYPE, PER)

##########################################################################
#
# VISUALIZING DATASETS
#
##########################################################################
# Basic first plot
# with(breachesbymonth, plot(MONTH, COUNT, xlab="Months", ylab="number of attacks"))

# plot the data using ggplot2 and pipes
spyderbymonthyear %>%
  na.omit() %>%
  filter(YEAR>2000) %>%
  ggplot(aes(x = MONTH, y = COUNT)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ TYPE ) +
  labs(title = "Precipitation - Boulder, Colorado",
       subtitle = "Use facets to plot by a variable - year in this case",
       y = "Daily precipitation (inches)",
       x = "Date") + theme_bw(base_size = 15)


spyderbymonthspred$month <- month
spyderbymonthspred$month <- factor(spyderbymonthspred$month, levels = spyderbymonthspred[["month"]])
plot_ly(spyderbymonthspred, x = ~month, 
        y = ~attacks, name = 'attacks', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22,255,13)', width = 4)) %>%
  add_trace(y = ~cves, name = 'cves', line = list(color = 'rgb(0,148,255)', width = 4)) %>%
  add_trace(y = ~breaches, name = 'breaches', line = list(color = 'rgb(255,56,0)', width = 4, dash = 'line')) %>%
  layout(title = "Percentage of incidents by month",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Percentage (%)"))
