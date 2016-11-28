best<- function(state, outcome) {
	setwd("C:/Users/martin.provost/Downloads/Formations/R Programming/ProgAssignment3-data")
	df_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	outcomes<-c("heart attack", "heart failure","pneumonia")
	indices<-c(11,17,23)
	if (!state %in% df_outcome$State) stop("invalid state")
	if (!outcome %in% outcomes) stop("invalid outcome")
	
	#df_outcome[, 11] <- as.numeric(as.character(df_outcome[, 11]))
	#df_outcome[, 17] <- as.numeric(as.character(df_outcome[, 17]))
	#df_outcome[, 23] <- as.numeric(as.character(df_outcome[, 23]))
	
	i<-indices[match(outcome, outcomes)]
	hospitals<-df_outcome[df_outcome$State==state, c(2,i)]
	names(hospitals)<-c("hospital","ratio")
	hospitals$ratio<-as.numeric(hospitals$ratio)
	hospitals<- na.omit(hospitals)
	min_ratio<-min(hospitals$ratio, na.rm=T)
	hospitals[hospitals$ratio==min_ratio,]$hospital
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")


