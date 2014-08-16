complete <- function(directory, id = 1:332) {
    all_data = numeric()
    ids <- numeric()
    values <- numeric()
    for (i in id) {
        name <- sprintf("./%s/%03d.csv", directory, i)
        data <- read.csv(name)
        m1 <- is.na(data$sulfate)
        m2 <- is.na(data$nitrate)
        m <- m1 | m2
        cnt <- sum(!m)
        ids <- c(ids, i)
        values <- c(values, cnt) 
    }
    data.frame(id = ids, nobs = values)
}


#complete("specdata", 1)
#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 30:25)
#complete("specdata", 3)
