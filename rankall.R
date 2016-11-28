getHospital<-function(state, i, num) {
		hospitals<-df_outcome[df_outcome$State==state, c(2,i)]
		names(hospitals)<-c("hospital","ratio")
		hospitals$ratio<-as.numeric(hospitals$ratio)
		hospitals<- na.omit(hospitals)
		if(num=="worst") num=nrow(hospitals)
	
		hospitalsSort<-hospitals[order(hospitals$ratio, hospitals$hospital),]
		hospitalsSort[num,]$hospital
}

rankall <- function(outcome, num="best")  {
	setwd("C:/Users/martin.provost/Downloads/Formations/R Programming/ProgAssignment3-data")
	df_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	outcomes<-c("heart attack", "heart failure","pneumonia")
	indices<-c(11,17,23)
	if(!outcome %in% outcomes) stop("invalid outcome")
	
	if(num=="best") num=1
	i<-indices[match(outcome, outcomes)]
	
	s<-split(df_outcome, df_outcome$State)
	results<-NULL
	for(state in names(s)) {
		s<-getHospital(state=state, i=i, num=num)
		results<-rbind(results, s)
	}
	results[num]
}

rankall("heart failure", "best")
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
