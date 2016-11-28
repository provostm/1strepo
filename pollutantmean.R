pollutantmean <- function(directory, pollutant, id=1:332){
	wdir<-getwd()
	taille<- nchar(paste("000", id, sep=""))
	filename <- substr(paste("000", id, sep=""), taille-2, taille)
	myPattern <-paste(wdir, "\\",directory, "\\", filename ,".csv", sep="")
	#print(myPattern)
	##temp <- list.files(path="C:\\Users\\martin.provost\\Documents\\specdata", pattern=myPattern , all.files = TRUE, full.names = TRUE )
	##print(temp)
	myfiles <- lapply(myPattern , read.csv, header=TRUE,sep = ",", quote = "\"")
	df_pollutant <- do.call(rbind , myfiles )
	
	#data is loaded, now we calculate the mean
	if(pollutant=="sulfate") {
		sulf<-as.numeric(as.character(df_pollutant$sulfate[!is.na(df_pollutant$sulfate)]))
		myMean<-mean(sulf, na.rm=TRUE)
	}
	else if ( pollutant=="nitrate" ) {
		nitr<-as.numeric(as.character(df_pollutant$nitrate[!is.na(df_pollutant$nitrate)]))
		myMean<-mean(nitr, na.rm=TRUE)
	}
	else {
		print("Error, bad pollutant choose")
	}
	myMean
}
