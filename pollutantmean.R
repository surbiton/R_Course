pollutantmean <- function (directory, pollutant, id = 1:332) {
## 'directory' is a character vector of length 1 indicating
## the location of the csv files  
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean, either "sulfate" or "nitrate"
## 'id' is an integer vector indicating the monster ID 
## number to be used
## return the mean of the pollutant across all monitor list
## in the 'id' vector (ignoring NA values)
## Note: do not round the results
## part 1: read csv files based on 'directory' and 'id'
      dir <- "C:\\Users\\sunc01\\Downloads\\rprog_data_specdata\\"
      cdir <- paste (dir,directory,"\\",sep="")
      adir <- paste(cdir,aid <- sprintf("%03d.csv", id),sep="")
      adf <- do.call (rbind,lapply(adir,read.csv))   
## part 2: return the mean for the given
## pollutant
      if (pollutant == "sulfate" || pollutant == "nitrate"){
            mean(adf[,pollutant],na.rm= TRUE)
      } else {
            print("error - input pollutant can only be sulfate or nitrate")
      }
}