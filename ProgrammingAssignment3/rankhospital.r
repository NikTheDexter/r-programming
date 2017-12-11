###Ranking hospitals by outcome in a state


rankhospital <- function(state, outcome, rank = "best"){
    ## Read outcome data
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomedataframe   <- as.data.frame(cbind(outcomedata[, 2],  # col 2 for hospital
                                outcomedata[, 7],  # col 7 for state
                               outcomedata[, 11],  # col 11 for heart attack
                               outcomedata[, 17],  # col 17 for heart failure
                               outcomedata[, 23]), # col 23 for pneumonia
                               stringsAsFactors = FALSE)
    colnames(outcomedataframe) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (!state %in% outcomedataframe[, "state"]) {
        stop('invalid state')
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else if (is.numeric(rank)) {
        s <- which(outcomedataframe[, "state"] == state)
        t <- outcomedataframe[s, ]                     # extracting dataframe for the called state
        t[, eval(outcome)] <- as.numeric(t[, eval(outcome)])
        t <- t[order(t[, eval(outcome)], t[, "hospital"]), ]
        output <- t[, "hospital"][rank]
    } else if (!is.numeric(rank)){
        if (rank == "best") {
             output <- best(state, outcome)
        } else if (rank == "worst") {
                s <- which(outcomedataframe[, "state"] == state)
                t <- outcomedataframe[s, ]    
                t[, eval(outcome)] <- as.numeric(t[, eval(outcome)])
                t <- t[order(t[, eval(outcome)], t[, "hospital"], decreasing = TRUE), ]
                output <- t[, "hospital"][1]
        } else {
            stop('invalid rank')
        }
    }
return(output)
}
