	
	### Programming Assignment : Week 2 : Air Pollution
	### Problem Statement : Part 1 - Function to find Pollutnant Mean with respect to - Directory,Specific Pollutant & ID i.e. monitor
	### Function Name : pollutantmean
	### Demo ex  :- pollutantmean("specdata", "nitrate", 70:72) 
	
	#=====================Start of function=========================================
	pollutantmean <- function(directory,pollutant,id =1:332){ 
	 reqdirectory <- paste("./",directory,"/")
	 reqdirectory <- str_replace_all(reqdirectory, fixed(" "), "")
	 #print(reqdirectory)
	 
	 #Create the Vector to hold the pollutant data
	 pollutantdata <- c()
	 allfiles <- as.character(list.files(reqdirectory))
	 #print(allfiles)
	 filepaths <- paste(reqdirectory,allfiles,sep = "")
	 #print(filepaths)
	 for(i in id){
	 curlocation <- read.csv(filepaths[i],header = TRUE,sep = ",")
	 head(curlocation)
	 removena <- curlocation[!is.na(curlocation[,pollutant]),pollutant]
	 pollutantdata <- c(pollutantdata,removena)
	}
	output <- mean(pollutantdata)
	return (output)
	}
	
	#=====================End of function=========================================