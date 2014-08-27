best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (!(state %in% data$State)) {
        stop("invalid state")
    }
    ind = -1
    o1 = "heart attack"
    o2 = "heart failure"
    o3 = "pneumonia"
    if (outcome == o1) {
        ind = 11
    } else if (outcome == o2) {
        ind = 17
    } else if (outcome == o3) {
        ind = 23
    } else {
        stop("invalid outcome")
    }
    
    x <- data[data$State == state, c(2,7, ind)]
    x$rate = as.numeric(x[,3])
    x <- x[!is.na(x$rate),c(1,2,4)]
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    rate = min(x$rate, na.rm=TRUE)
    hospitals <- x[x$rate==rate,1]
    
    hospital <- min(hospitals)
    hospital
}

#best("TX", "heart attack")
#best("TX", "heart failure")
#best("MD", "heart attack")
#best("MD", "pneumonia")

#best("BB", "heart attack")
#best("NY", "hert attack")

