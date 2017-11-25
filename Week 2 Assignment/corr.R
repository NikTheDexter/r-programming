	### Programming Assignment : Week 2 : Air Pollution
	### Problem Statement : Part 3 - Function to find correlation between pollutant considering the complete observations with respect to - directory & threshold 
	### Function Name : corr
	### Demo ex  :- corr("specdata", 400) 
	
	#=====================Start of function=========================================
	
	corr <- function(directory, threshold = 0){
	reqdirectory <- paste("./",directory,"/")
	reqdirectory <- str_replace_all(reqdirectory, fixed(" "), "")
		
	#With the help fo earlier creatd complete function get the complete data
	
	completedata <- complete("specdata",1:332)
	
	#Get the list of complete Nobs from completedata
	
	nobsdetails <- completedata$nobs
	
	# Get all ids which have greater nobs than the Thresholds
	requiredids <- completedata$id[nobsdetails > threshold]
	
	#Get the Ids to be processed
	 idstobeprocessed <- length(requiredids)
	 
	 #Create the Vector to process the Correlation data
	 
	 corrdata <- rep(0,idstobeprocessed)
	 
	 allfiles <- as.character(list.files(reqdirectory))
	 filepaths <- paste(reqdirectory,allfiles,sep = "")
	 
	 counter <- 1
	 for(i in requiredids){
	 curlocation <- read.csv(filepaths[i],header = TRUE,sep = ",")
	 corrdata[counter] <- cor(curlocation$sulfate,curlocation$nitrate,use = "complete.obs")
        counter <- counter + 1
	}
	 
	 output <- corrdata
	 return(output)
	 
	 }
	 #=====================Start of function=========================================