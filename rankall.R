rankall <- function(outcome, num = 1) {
      ## check if input outcome within condition
      if (is.na(match(outcome,c("Heart.Attack","Heart.Failure", "Pneumonia")))){
        return ("invalid input - needs to be (Heart.Attack, Heart.Failure or Pneumonia)")
      }    
      ## Read outcome data
      ocm = paste0("C:\\Users\\sunc01\\Downloads\\",
               "rprog_data_ProgAssignment3-data\\outcome-of-care-measures.csv")
      df<-read.csv(ocm)   
      if (num == "best") {
            num<-1
      } 
      ## coerce column 11, 17 and 23 to numeric for ordering
      df[,c(11,17,23)]<-sapply(df[,c(11,17,23)],as.numeric)
      ns<-length(unique(df[,7]))
      out<-data.frame("Hospital"=character(),"State"=character(), "Mortality_Rate"=numeric())
      if (grepl(outcome,names(df)[11])==TRUE){
            df <- df[is.na(as.numeric(df[,11]))!=TRUE,]
            ## order by outcome mortality rate
            df<-df[order(df[,11],df[,2]),]
            ## split dataframe by state
            dfls<-split(df,df$State)
            ## Return a data frame with the hospital names and the
            ## (abbreviated) state name
            for (i in 1:ns){
                  if(num == "worst"){
                        num2<-nrow(dfls[[i]])
                        out[nrow(out)+1,] = c (dfls[[i]][num2,2],dfls[[i]][num2,7],dfls[[i]][num2,11])
                  } else if (num > nrow(dfls[[i]])) {
                        out[nrow(out)+1,] = c ("NA",dfls[[i]][num,7],"NA")
                  } else {
                        out[nrow(out)+1,] = c (dfls[[i]][num,2],dfls[[i]][num,7],dfls[[i]][num,11])
                  }
            }
            #return(df[,c(2,7,11,17,23)])
            return(out)
      }
      if (grepl(outcome,names(df)[17])==TRUE){
            df <- df[is.na(as.numeric(df[,17]))!=TRUE,]
            df<-df[order(df[,17],df[,2]),]
            dfls<-split(df,df$State)
            for (i in 1:ns){
                  if(num == "worst"){
                        num2<-nrow(dfls[[i]])
                        out[nrow(out)+1,] = c (dfls[[i]][num2,2],dfls[[i]][num2,7],dfls[[i]][num2,17])
                  } else if (num > nrow(dfls[[i]])) {
                        out[nrow(out)+1,] = c ("NA",dfls[[i]][num,7],"NA")
                  } else {
                        out[nrow(out)+1,] = c (dfls[[i]][num,2],dfls[[i]][num,7],dfls[[i]][num,17])
                  }
            }
            return(out)
      } 
      if (grepl(outcome,names(df)[23])==TRUE){
            df <- df[is.na(as.numeric(df[,23]))!=TRUE,] 
            df<-df[order(df[,23],df[,2]),]
            
            dfls<-split(df,df$State)
            for (i in 1:ns){
                  if(num == "worst"){
                        num2<-nrow(dfls[[i]])
                        out[nrow(out)+1,] = c (dfls[[i]][num2,2],dfls[[i]][num2,7],dfls[[i]][num2,23])
                  } else if (num > nrow(dfls[[i]])) {
                        out[nrow(out)+1,] = c ("NA",dfls[[i]][num,7],"NA")
                  } else {
                        out[nrow(out)+1,] = c (dfls[[i]][num,2],dfls[[i]][num,7],dfls[[i]][num,23])
                  }
            }
            return(out)
      } 
}