rankhospital <- function( state, outcome, num = "best" ) {
    data <- read.csv(paste(getwd(), "/data/", "outcome-of-care-measures.csv", sep=""), colClasses="character")
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ( !state %in% unique(data[, 7])) {
        stop("Invalid State")
    } else if ( !outcome %in% outcomes ) {
        stop("Invalid Outcome")
    } else {
        if ( outcome == "heart attack" ) {
            col <- 11
        } else if ( outcome == "heart failure" ) {
            col <- 17
        } else {
            col <- 23
        }
    }
    data[, col] = as.numeric(data[, col])
    data1 = data[data[, 7] == state, c(2, col)]
    data1 = na.omit(data1)
    nhospital = nrow(data1)
    switch(num, best = {
        num = 1
    }, worst = {
        num = nhospital
    })
    if (num > nhospital) {
        return(NA)
    }
    o = order(data1[, 2], data1[, 1])
    data1[o, ][num, 1]
}
