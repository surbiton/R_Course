findhospital <- function(cnum,num) {
      ## remove NA values 
      fil <- fil1[is.na(as.numeric(fil1[,cnum]))!=TRUE,]
      ## rank based on mortality rate of the outcome (1st order)
      ## and alphabetic (2nd order)
      fil<-fil[order(fil[,cnum],fil[,2]),]
      ## for num: best->1, worst->last in record
      if (num == "best") {
            num<-1
      } else if (num == "worst") {
            num<-nrow(fil)
      }
      ## check if num is higher than maximum of hospitals
      ## with valid records
      if(nrow(fil)<num){
            return (paste0("input num is larger than valid",
                     " number of records"))
      }
      ## ## choose the hospital name based on num and return it
      return(fil[num,2])
}
  
rankhospital <- function(state,outcome,num){
      ## check if input outcome within condition
      if (is.na(match(outcome,c("Heart.Attack","Heart.Failure", "Pneumonia")))){
            return ("invalid input - needs to be (Heart.Attack, 
                      Heart.Failure or Pneumonia)")
      } 
      ## read outcome-of-care-measures.csv
      ocm = paste0("C:\\Users\\sunc01\\Downloads\\",
               "rprog_data_ProgAssignment3-data\\outcome-of-care-measures.csv")
      df<-read.csv(ocm)
      ## check if the input state is in the dataframe
      if (match(state, unique(df[,7])) == "NA"){
            return ("invalid input of state")
      }
      ## filter based on state
      fil1<<-df[df[,7]==state,]
      ## coerce column 11, 17 and 23 to numeric for ordering
      fil1[,c(11,17,23)]<<-sapply(fil1[,c(11,17,23)],as.numeric)
      ## filter based on outcome
      if (grepl(outcome,names(fil1)[11])==TRUE){
            ## use function to get desire hospital name
            return(findhospital(11,num))
      }
      if (grepl(outcome,names(fil1)[17])==TRUE){
            ## use function to get desire hospital name
            return(findhospital(17,num))
      } 
      if (grepl(outcome,names(fil1)[23])==TRUE){
            ## use function to get desire hospital name
            return(findhospital(23,num))
      }
}