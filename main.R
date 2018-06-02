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
  
  #return
  spyder.main
}

spyder.main <- get_spyder_df()



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

spyder.plots.ByYear <- function(myYear){
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
  if("attacks" %in% colnames(tmpdf)) p<-add_lines(p, x= ~month, y = ~attacks, name = 'attacks', line = list(color = 'rgb(22,255,13)', width = 4))
  if("cves" %in% colnames(tmpdf)) p<-add_lines(p, x= ~month, y = ~cves, name = 'cves', line = list(color = 'rgb(0,148,255)', width = 4))
  if("breaches" %in% colnames(tmpdf)) p<-add_lines(p, x= ~month, y = ~breaches, name = 'breaches', line = list(color = 'rgb(255,56,0)', width = 4))
  p
}

# Example Plots (Daniel y Caro!)
spyder.plots.ByYear(c(2012, 2013,2014, 2015, 2016, 2017))
spyder.plots.ByYear(c(2017))


# Box plot for yearly analysis of month recurrency
spyder.plots.BoxPlot <- function(myType, myYears){
  # group by year
  tmpdf <- spyder.main %>% 
    filter(TYPE == myType, YEAR %in% myYears)%>% 
    group_by(TYPE,MONTH, YEAR) %>% 
    summarise(COUNT = sum(!is.na(MONTH)))
  
  ggplot(tmpdf, aes(x=as.factor(MONTH), y=COUNT)) + 
    geom_boxplot(
      
      # custom boxes
      color="blue",
      fill="blue",
      alpha=0.2,
      
      # Notch?
      notch=FALSE,
      notchwidth = 0.9,
      
      # custom outliers
      outlier.colour="red",
      outlier.fill="red",
      outlier.size=3
      
    ) +
    ggtitle(paste(myType,"counts through", paste(toString(myYears)))) +
    xlab("Months") + ylab("Count") +
    scale_x_discrete(labels=month.abb)
}

# Example boxplot (Daniel y Caro!)
spyder.plots.BoxPlot("breaches", c(2014,2015,2016)) 
spyder.plots.BoxPlot("cves", c(2014,2015,2016)) 




# # plot the data using ggplot2 and pipes
# spyder.byyear %>%
#   na.omit() %>%
#   filter(YEAR>2000) %>%
#   ggplot(aes(x = MONTH, y = COUNT)) +
#   geom_bar(stat = "identity", fill = "darkorchid4") +
#   facet_wrap( ~ TYPE ) +
#   labs(title = "Precipitation - Boulder, Colorado",
#        subtitle = "Use facets to plot by a variable - year in this case",
#        y = "Daily precipitation (inches)",
#        x = "Date") + theme_bw(base_size = 15)


