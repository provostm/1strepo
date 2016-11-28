complete <-function(directory, id=1:332){

	wdir<-getwd()
	taille<- nchar(paste("000", id, sep=""))
	filename <- substr(paste("000", id, sep=""), taille-2, taille)
	myPattern <-paste(wdir, "\\",directory, "\\", filename ,".csv", sep="")
	#print(myPattern)
	##temp <- list.files(path="C:\\Users\\martin.provost\\Documents\\specdata", pattern=myPattern , all.files = TRUE, full.names = TRUE )
	##print(temp)
	myfiles <- lapply(myPattern , read.csv, header=TRUE,sep = ",", quote = "\"")
	df_files <- do.call(rbind , myfiles )
	
	complete<- numeric(0)
	for(i in id) {
		df_temp<- df_files[df_files$ID == i,]
		complete <-c(complete, sum(as.numeric(complete.cases(df_temp))))
		#print(df_complete)
	}
	data.frame(id=id, nobs=complete)
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)