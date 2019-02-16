rankall <- function(outcome = "heart attack", num = "best"){
  data_read <- read.csv(paste(getwd(),"/Coursera/ProgrammingAssignment3/outcome-of-care-measures.csv",sep=""),colClasses="character")
  
  
  
  if(outcome == "heart attack"){
    values_df <- data.frame(data_read[11])
    hospital_df <- data.frame(data_read[2],data_read[7],data_read[11])
  } else if(outcome == "pneumonia"){
    values_df <- data.frame(data_read[11])
    hospital_df <- data.frame(data_read[2],data_read[7],data_read[23])
  } else if(outcome == "heart failure"){
    values_df <- data.frame(data_read[11])
    hospital_df <- data.frame(data_read[2],data_read[7],data_read[17])
  } else {
    stop("invalid outcome")
  }
  

  outcome_col <-paste("30D Mortality -",outcome)
  names(hospital_df) <- c("hospital","State",outcome_col)
#  print(hospital_df)
  
  suppressWarnings(hospital_df[,3] <- as.numeric(hospital_df[,3]))
  new_hosp_df <- data.frame()  
  new_hosp_df <- hospital_df[!is.na(hospital_df[,3]),]
  ordered_hosp_df <- data.frame()
  ordered_hosp_df <- new_hosp_df[order(new_hosp_df[,2],new_hosp_df[,3]),]
  
  #  ordered_hosp_df <- new_hosp_df[order(new_hosp_df[,2],new_hosp_df[,3]),]

  
  states <- unique(hospital_df[,2])
  ordered_states <- states[order(states)]
  rankall_df <- data.frame()
    
  for(i in 1:length(ordered_states)){
#  for(i in 10:53){    ##shorten loop for testing purposes
    by_state_df <- subset(ordered_hosp_df,State==ordered_states[i])
    by_state_df_ordered <- by_state_df[order(by_state_df[,3],by_state_df[,1]),]
 
    #   print(by_state_df_ordered)
      if(num=="best"){
        num_temp = 1
      }
      if(num=="worst"){
        num_temp = nrow(by_state_df_ordered)
      }
      else{
        num_temp = num
        }
#    rankall_df <- cbind(rankall_df,by_state_df[num,1:2])
#    state_output <- c(by_state_df[num,1],ordered_states[i])
    rankall_df <- rbind(rankall_df,by_state_df_ordered[num_temp,1:2])
    if(num>nrow(by_state_df_ordered)){
      rankall_df[i,2]=ordered_states[i]
    }
 #   by_state_df[num,1:2])
#    print(rankall_df)
  }  


rankall_df
#print(rankall_df)
}