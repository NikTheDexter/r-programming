### Finding the best hospital in a state

best <- function(state, outcome) {
    ## Read outcome caredata
    caredata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    caredataframe   <- as.data.frame(cbind(caredata[, 2],   # col 2 for hospital
                                caredata[, 7],                  # col 7 for state
                                caredata[, 11],                 # col 11 for heart attack
                                caredata[, 17],                 # col 17 for heart failure
                                caredata[, 23]),                # col 23 for  pneumonia
                       stringsAsFactors = FALSE)
    colnames(caredataframe) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
                                        
    ## Check that state and outcome are valid
    if(!state %in% caredataframe[, "state"]){
        stop('invalid state')
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else {
        si <- which(caredataframe[, "state"] == state)
        ts <- caredataframe[si, ]    # extracting data for the called state
        oi <- as.numeric(ts[, eval(outcome)])
        min_val <- min(oi, na.rm = TRUE)
        result  <- ts[, "hospital"][which(oi == min_val)]
        output  <- result[order(result)]
    }
return(output)
}