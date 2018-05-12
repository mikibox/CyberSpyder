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