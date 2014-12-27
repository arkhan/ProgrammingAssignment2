best <- function( state, outcome ) {
    data <- read.csv(paste(getwd(), "/data/", "outcome-of-care-measures.csv", sep=""), colClasses="character")
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ( !state %in% unique(data[, 7])) {
        stop("Invalid State")
    } else if ( !outcome %in% outcomes ) {
        stop("Invalid Outcome")
    } else {
        if ( outcome == "heart attack" ) {
           col <- 11
       } else if ( outcome == "heart failure"  ) {
           col <- 17
       } else {
          col <- 23
       }
    }
    data = data[data$State == state, c(2, col)]
    data[which.min(data[, 2]), 1]
}
