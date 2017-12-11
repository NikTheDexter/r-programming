###Ranking hospitals in all states

rankall <- function(outcome, num = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    fd   <- as.data.frame(cbind(data[, 2],  # col 2 for hospital
                                data[, 7],  # col 7 for state
                                data[, 11],  # col 11 for  heart attack
                                data[, 17],  # col 17 for heart failure
                                data[, 23]), # col 23 for pneumonia
                          stringsAsFactors = FALSE)
    colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    fd[, eval(outcome)] <- as.numeric(fd[, eval(outcome)])
    
    ## Check that state and outcome are valid
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else if (is.numeric(num)) {
        bystate <- with(fd, split(fd, state))
        ordered  <- list()
        for (i in seq_along(bystate)){
            bystate[[i]] <- bystate[[i]][order(bystate[[i]][, eval(outcome)], 
                                                 bystate[[i]][, "hospital"]), ]
            ordered[[i]]  <- c(bystate[[i]][num, "hospital"], bystate[[i]][, "state"][1])
        }
        result <- do.call(rbind, ordered)
        output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
        names(output) <- c("hospital", "state")
    } else if (!is.numeric(num)) {
        if (num == "best") {
            bystate <- with(fd, split(fd, state))
            ordered  <- list()
            for (i in seq_along(bystate)){
                bystate[[i]] <- bystate[[i]][order(bystate[[i]][, eval(outcome)], 
                                                     bystate[[i]][, "hospital"]), ]
                ordered[[i]]  <- c(bystate[[i]][1, c("hospital", "state")])
            }
            result <- do.call(rbind, ordered)
            output <- as.data.frame(result, stringsAsFactors = FALSE)
            rownames(output) <- output[, 2]
        } else if (num == "worst") {
            bystate <- with(fd, split(fd, state))
            ordered  <- list()
            for (i in seq_along(bystate)){
                bystate[[i]] <- bystate[[i]][order(bystate[[i]][, eval(outcome)], 
                                                     bystate[[i]][, "hospital"], 
                                                     decreasing = TRUE), ]
                ordered[[i]]  <- c(bystate[[i]][1, c("hospital", "state")])
            }
            result <- do.call(rbind, ordered)
            output <- as.data.frame(result, stringsAsFactors = FALSE)
            rownames(output) <- output[, 2]
        } else {
            stop('invalid num')
        }
    }
return(output)
}