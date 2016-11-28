corr <-function(directory, threshold=0){

	
	wdir<-getwd()
	corr<-numeric(0)
	for(i in 1:332) {
		taille<- nchar(paste("000", i, sep=""))
		filename <- substr(paste("000", i, sep=""), taille-2, taille)
		myPattern <-paste(wdir, "\\",directory, "\\", filename ,".csv", sep="")
		
		#on lit le fichier tout en retirant les NA
		myfiles <- na.omit(read.csv(myPattern, header=TRUE,sep = ",", quote = "\""))
		
		if(nrow(myfiles) > threshold) {
			#on calcule la correlation seulement pour les enregistrements complets
			cr<-cor(myfiles$nitrate,myfiles$sulfate,use='complete.obs')
			corr<-append(corr,cr)
		}

	}


	corr
}

cr <- corr("specdata", 400)
head(cr)

