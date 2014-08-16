corr <- function(directory, threshold = 0) {
    names <- list.files(directory, full.names=TRUE)
    cors = numeric()
    for (name in names) {
        data <- read.csv(name)
        m1 <- is.na(data$sulfate)
        m2 <- is.na(data$nitrate)
        m <- m1 | m2
        cnt <- sum(!m)
        if (cnt > threshold) {
            x <- data$sulfate[!m]
            y <- data$nitrate[!m]
            c <- cor(x, y)
            cors = c(cors, c)
        }
    }
    cors
}

cr <- corr("specdata", 150)
head(cr)


summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
cr <- corr("specdata", 400)
head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
length(cr)
## [1] 0
cr <- corr("specdata")
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
length(cr)
## [1] 323







