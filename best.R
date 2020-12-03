best <- function(state,outcome){
    ## check if input outcome within condition
        if (is.na(match(outcome,c("Heart.Attack","Heart.Failure", "Pneumonia")))){
              stop ("invalid input - needs to be (Heart.Attack, 
                      Heart.Failure or Pneumonia)")
        } 
    ## read outcome-of-care-measures.csv
        ocm = paste0("C:\\Users\\sunc01\\Downloads\\",
            "rprog_data_ProgAssignment3-data\\outcome-of-care-measures.csv")
        df<-read.csv(ocm)
    ## check if the input state is in the dataframe
        if (match(state, unique(df[,7])) == "NA"){
              stop ("invalid input of state")
        }
    ## return the name of hospital with the lowest 30 day
    ## mortality rate for the specified outcome in that state
    ## generate a dataframe with two columns - hospital
    ## name and 30 data mortality rate with inputs of 
    ## state and outcome
        ## subset outcome by state and create fil1
        fil1<-df[df[,7]==state,]
        ## sort by hospital name
        fil1<-fil1[order(fil1[,2]),]
        ## subset df based on input outcome
        ## column 11 has mortality rate from heart attack
        if (grepl(outcome,names(fil1)[11])==TRUE){
              # filter out na of column 11
              fil1 <- fil1[fil1[,11]!="",]
              # get the minimum rate
              return(fil1[which.min(fil1[,11]),2])
              # create fil2 with just three columns: hospital name, 
              # state and motility
              # fil2 <- fil1[,c(2,7,11)]
        }
        ## column 17 has mortality rate from heart failure
        if (grepl(outcome,names(fil1)[17])==TRUE){
          # filter out na of column 17
          fil1 <- fil1[fil1[,17]!="",]
          # get the minimum rate
          return(fil1[which.min(fil1[,17]),2])
        }        
        ## column 23 has mortality rate from pnumonia
        if (grepl(outcome,names(fil1)[23])==TRUE){
          # filter out na of column 23
          fil1 <- fil1[fil1[,23]!="",]
          # get the minimum rate
          return(fil1[which.min(fil1[,23]),2])
        }
        return("something is wrong")
}
