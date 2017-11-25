	### Programming Assignment : Week 2 : Air Pollution
	### Problem Statement : Part 2 - Function to find Completly Observed Cases with respect to - directory & monitor id/ids
	### Function Name : complete
	### Demo ex  :- complete("specdata", 30:25) 
	
	#=====================Start of function=========================================
	
	complete <- function(directory,id =1:332){ 
	
	 reqdirectory <- paste("./",directory,"/")
	 reqdirectory <- str_replace_all(reqdirectory, fixed(" "), "")
		
	 allfiles <- as.character(list.files(reqdirectory))
	 filepaths <- paste(reqdirectory,allfiles,sep = "")
	
	#Get the Ids to be processed
	 idstobeprocessed <- length(id)
	 
	 #Create the Vector to process the data without NA
     datawithoutna <- rep(0, idstobeprocessed)
	
	 counter <- 1
	 for(i in id){
	 curlocation <- read.csv(filepaths[i],header = TRUE,sep = ",")
	 datawithoutna[counter] <- sum(complete.cases(curlocation))
        counter <- counter + 1
	}
	output <- data.frame(id = id, nobs = datawithoutna)
    return(output)
	}
	
	#=====================End of function=========================================