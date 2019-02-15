best <- function(state="TX",outcome="heart attack"){
  data_read <- read.csv(paste(getwd(),"/Coursera/ProgrammingAssignment3/outcome-of-care-measures.csv",sep=""),colClasses="character")
  state_data <- subset(data_read, State==state)
  
  if(nrow(state_data) == 0){
    stop("invalid state")
  }
  
  if(outcome == "heart attack"){
    hospital_df <- data.frame(state_data[2],state_data[7],state_data[11])
  } else if(outcome == "pneumonia"){
    hospital_df <- data.frame(state_data[2],state_data[7],state_data[21])
  } else if(outcome == "heart failure"){
    hospital_df <- data.frame(state_data[2],state_data[7],state_data[15])
  } else {
    stop("invalid outcome")
  }
  relevant_col_name <-paste("30 Day Mortality Rates -",outcome)
  names(hospital_df) <- c("Hospital Name","State",relevant_col_name)
#  print(hospital_df)

  
    cleaned_hosp_df <- data.frame()
  for(i in 1:nrow(hospital_df)){
    hospital_df[i,3] <- as.numeric(hospital_df[i,3])
     }
     good <- complete.cases(hospital_df)
    hospital_df <- hospital_df[good,]
 #   print(hospital_df)
    hospital_df[order(hospital_df$relevant_col_name)]
}