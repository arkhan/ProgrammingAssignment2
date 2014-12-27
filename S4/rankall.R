rankall <- function( outcome, num = "best" ) {
    data0 <- read.csv(paste(getwd(), "/data/", "outcome-of-care-measures.csv", sep=""), colClasses="character")
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    states = unique(data0[, 7])
    if ( !outcome %in% outcomes ) {
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
    data0[, col] = as.numeric(data0[, col])
    data0 = data0[, c(2, 7, col)]  # leave only name, state, and death rate
    data0 = na.omit(data0)
    rank <- function(state) {
        data1 = data0[data0[, 2] == state, ]
        nhospital = nrow(data1)
        switch(num, best = {
            num = 1
        }, worst = {
            num = nhospital
        })
        if (num > nhospital) {
            result = NA
        }
        o = order(data1[, 3], data1[, 1])
        result = data1[o, ][num, 1]
        c(result, state)
    }
    out = do.call(rbind, lapply(states, rank))
    out = out[order(out[, 2]), ]
    rownames(out) = out[, 2]
    colnames(out) = c("hospital", "state")
    data.frame(out)
}
