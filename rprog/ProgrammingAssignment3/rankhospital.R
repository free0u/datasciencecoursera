rankhospital <- function(state, outcome, num = "best") {
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
    
    x <- x[order(x$rate, x$Hospital.Name),]
    
    n = nrow(x)
    if (num == "best") {
        return(x[1, 1])
    } else if (num == "worst") {
        return(x[n, 1])
    } else {
        return(x[num, 1])
    }
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
