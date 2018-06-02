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
}

loadPackages <- function(){
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  library(jsonlite)
  library(tidyr)
}

# installPackages(necessaryPackages)
loadPackages()

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

# TODO explore the datasets

get_spyder_df <- function(){
  spyder.main <- data.frame(row.names = c('TYPE','YEAR', 'MONTH', 'DAY', 'julianday'))
  spyder.main <- process_dates(spyder.main, breaches, "BreachDate", "%Y-%m-%d", "breaches")
  spyder.main <- process_dates(spyder.main, attacks2018, "Date", "%d/%m/%Y", "attacks")
  spyder.main <- process_dates(spyder.main, attacks2017, "Date", "%d/%m/%Y", "attacks")
  spyder.main <- process_dates(spyder.main, cves, "published.date", "%d/%m/%Y", "cves")
  spyder.main <- drop_na(spyder.main)
  
  # get totals
  spyder.totals <- spyder.main %>% group_by(TYPE) %>%
    summarise(TOTAL = sum(!is.na(TYPE))) %>% rename(input_type = TYPE, input_total = TOTAL)
  
  # group by month
  spyder.bymonth <- spyder.main %>% group_by(TYPE, MONTH) %>% 
    summarise(COUNT = sum(!is.na(TYPE)),
              PER = (COUNT / spyder.totals$input_total[spyder.totals$input_type == TYPE][1])*100)
  
  # spread month
  spyder.spreadmonth <- spread(spyder.bymonth[c("TYPE","MONTH","PER")], TYPE, PER)
  
  # group by year
  spyder.byyear <- spyder.main %>% group_by(TYPE,MONTH, YEAR) %>% 
    summarise(COUNT = sum(!is.na(MONTH)))
  
  #return
  spyder
}

spyder <- get_spyder_df()


##########################################################################
#
# VISUALIZING DATASETS
#
##########################################################################
# Plot 2017 and 2018 attacks and cves
months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July','August', 'September', 'October', 'November', 'December')

spyder.getTotals <- function(custom_filter){
  df <- spyder.main %>% 
    filter(YEAR %in% custom_filter) %>%
    group_by(TYPE) %>%
    summarise(TOTAL = sum(!is.na(TYPE))) %>% rename(input_type = TYPE, input_total = TOTAL)
}

spyder.plots.AttacksCvesByYear <- function(myYear){
  tmptotals <- spyder.getTotals(myYear)
  tmpdf <- spyder.main %>%
    filter(YEAR %in% myYear) %>%
    group_by(TYPE, MONTH) %>% 
    summarise(COUNT = sum(!is.na(TYPE)),
              PER = (COUNT / tmptotals$input_total[tmptotals$input_type == TYPE][1])*100)
  tmpdf <- spread(tmpdf[c("TYPE","MONTH","PER")],TYPE, PER)
  sum(tmpdf$attacks)
  
  tmpdf$month <- months
  tmpdf$month <- factor(tmpdf$month, levels = tmpdf[["month"]])
  p <- plot_ly(tmpdf,  mode = 'lines') %>%
    layout(title = "Attacks vs. CVEs 2017",
           xaxis = list(title = "Months"),
           yaxis = list (title = "Percentage (%)"))
  p<-add_lines(p, x= ~month, y = ~attacks, name = 'attacks', line = list(color = 'rgb(22,255,13)', width = 4))
  p<-add_lines(p, x= ~month, y = ~cves, name = 'cves', line = list(color = 'rgb(0,148,255)', width = 4))
  p
}

spyder.plots.AttacksCvesByYear(c(2017))


# Basic first plot
# with(breachesbymonth, plot(MONTH, COUNT, xlab="Months", ylab="number of attacks"))

# plot the data using ggplot2 and pipes
spyder.byyear %>%
  na.omit() %>%
  filter(YEAR>2000) %>%
  ggplot(aes(x = MONTH, y = COUNT)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap( ~ TYPE ) +
  labs(title = "Precipitation - Boulder, Colorado",
       subtitle = "Use facets to plot by a variable - year in this case",
       y = "Daily precipitation (inches)",
       x = "Date") + theme_bw(base_size = 15)

spyder.spreadmonth$month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July','August', 'September', 'October', 'November', 'December')
spyder.spreadmonth$month <- factor(spyder.spreadmonth$month, levels = spyder.spreadmonth[["month"]])
plot_ly(spyder.spreadmonth, x = ~month, 
        y = ~attacks, name = 'attacks', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22,255,13)', width = 4)) %>%
  add_trace(y = ~cves, name = 'cves', line = list(color = 'rgb(0,148,255)', width = 4)) %>%
  add_trace(y = ~breaches, name = 'breaches', line = list(color = 'rgb(255,56,0)', width = 4, dash = 'line')) %>%
  layout(title = "Percentage of incidents by month",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Percentage (%)"))

