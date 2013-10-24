library("RCurl")
library("XML")

# Function to grab data from internet site according to given year
getDairyData <- function(year) {
  
  # Build url string
  urlStr <- paste0("http://www.prodzuivel.nl/pz/noteringen/commissie_historie_",year,".htm")
  
  # Get annual table
  y2013<-getURL(urlStr, ssl.verifypeer = FALSE)
  
  # Parse HTML into data frame and add variable names
  doc <- htmlParse(y2013)
  tableNodes <- getNodeSet(doc, "//table")
  tb <- readHTMLTable(tableNodes[[2]], header = c("Datum", "Boter", "Vol melkpoeder", "Mager melkpoeder mager - food", "Mager melkpoeder - feed", "Weipoeder"), skip.rows = 2)
  tb <- tb[-1,]                        # Delete first row
  tb <- tb[-nrow(tb)+1:-nrow(tb),]     # Delete last two rows
  
    tb[[1]] <- paste0(tb[[1]],"/",year) # Add year to date 
  
  tb
}

# Grab data from internet site and build data frame
period <- c(2009:2013)
data<-c()
for(y in period) {
  ydata <- getDairyData(y)
  data <- rbind(data, ydata)
}

# Change comma's into dots and change class of prices into numeric
for (i in c(2:6)){
  data[[i]] <- as.numeric(sub(",", ".",data[[i]]))
}

# Build file name from last date in file and save as csv
ldate <- data[[nrow(data), 1]]
filestring <- paste0("weekly_dairy_prices_ending_", ldate,".csv")
filestring <- gsub("/","", filestring)
write.csv(data, file = filestring)

tail(data)
