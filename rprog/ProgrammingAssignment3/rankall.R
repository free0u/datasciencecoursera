rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome are valid
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
    
    
    x <- data[,c(2,7, ind)]
    x$rate = as.numeric(x[,3])
    x <- x[!is.na(x$rate),c(1,2,4)]
    
    x <- split(x, x$State)
    
    rank <- function(d) {
        y <- d[order(d$rate, d$Hospital.Name),]

        ind <- -1
        if (num == "best") {
            ind = 1
        } else if (num == "worst") {
            ind = nrow(d)
        } else {
            ind = num
        }
        
        t <- y[ind,c(1,2)]
        state <- d$State[1]
        hospital <- t$Hospital.Name[1]
        ret <- data.frame(hospital, state)
        ret
    }
    
    x <- lapply(x, rank)
    x <- do.call(rbind, x)
    print(x)
}

head(rankall("heart attack", 20), 10)

#tail(rankall("pneumonia", "worst"), 3)

#tail(rankall("heart failure"), 10)
