rankhospital <- function(state = "MD",outcome="heart failure",num = 5){
  data_read <- read.csv(paste(getwd(),"/Coursera/ProgrammingAssignment3/outcome-of-care-measures.csv",sep=""),colClasses="character")
  state_data <- subset(data_read, State==state)
  
  if(nrow(state_data) == 0){
    stop("invalid state")
  }
  
  if(outcome == "heart attack"){
    values_df <- data.frame(state_data[11])
    hospital_df <- data.frame(state_data[2],state_data[7],state_data[11])
  } else if(outcome == "pneumonia"){
    values_df <- data.frame(state_data[11])
    hospital_df <- data.frame(state_data[2],state_data[7],state_data[23])
  } else if(outcome == "heart failure"){
    values_df <- data.frame(state_data[11])
    hospital_df <- data.frame(state_data[2],state_data[7],state_data[17])
  } else {
    stop("invalid outcome")
  }
  
  outcome_col <-paste("30D Mortality -",outcome)
  names(hospital_df) <- c("Hospital Name","State",outcome_col)
  # print(hospital_df)
  
  suppressWarnings(hospital_df[,3] <- as.numeric(hospital_df[,3]))
  new_hosp_df <- data.frame()  
  new_hosp_df <- hospital_df[!is.na(hospital_df[,3]),]
  ordered_hosp_df <- data.frame()
  ordered_hosp_df <- new_hosp_df[order(new_hosp_df[,3],new_hosp_df[,1]),]
  
  if(num=="best"){
    num = 1
  }
  if(num=="worst"){
    num = nrow(ordered_hosp_df)
  }
  ordered_hosp_df[num,1]
}