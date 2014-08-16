pollutantmean <- function(directory, pollutant, id = 1:332) {
    all_data = numeric()
    for (i in id) {
        name <- sprintf("./%s/%03d.csv", directory, i)
        #print(name)
        data <- read.csv(name)
        data <- data[[pollutant]]
        all_data <- c(all_data, data)
    }
    mean(all_data, na.rm=TRUE)
}


pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)