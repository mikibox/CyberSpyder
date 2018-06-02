

##########################################################################
#
# READING DATASETS
#
##########################################################################
# PAQUETES

library(rworldmap)


# Hackmagedon csvnetse
geo2017.csv <- read.csv("https://docs.google.com/spreadsheets/d/14-PLnKlHDvL4eZQFuf0qy2fTA-2OuVsRk_t25PrX4eY/export?format=csv",sep = ",")
i<-1
j-1
a<-0

#Valores de las tablas antes de ser modificados 
t<-table(geo2017.csv$Country)
tablecountry<-names(table(geo2017.csv$Country))

#Valor logico de variables que no se encuentran en el mapa
log<-geo2017.csv$Country[1:length(geo2017.csv$Country)] 

for (j in 1:length(geo2017.csv$Country)) {
  if (log[j] == tablecountry[66]) {
    geo2017.csv$Country[j]<-"GB"
  }
}

#Parsear datos eliminando no deseados
for (i in 1:length(geo2017.csv$Country)) {
  if (log[i] == tablecountry[1] | log[i] == tablecountry[41]) {
    geo2017.csv<-geo2017.csv[-(i-a),]
    a<-a+1
    
  }

}

# AGREGAR DATOS AL PAQUETE rworldmap 

laTabla = data.frame(t1)
laTabla$Incidentes<-laTabla$Freq
sPDF <- joinCountryData2Map(dF = laTabla,
                            joinCode = "ISO2",
                            nameJoinColumn = "Var1",
                            verbose = T)
# plot the spatialDataFrame
mapCountryData(sPDF, nameColumnToPlot = "Incidentes")
