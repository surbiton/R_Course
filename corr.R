corr <- function (directory,threshold=0) {
## read all the csv files in 'directory' one by one
      dir <- "C:\\Users\\sunc01\\Downloads\\rprog_data_specdata\\"
      cdir <- paste (dir,directory, '\\',sep="")
      cl <- list.files (path=cdir)
      ar <- numeric()
      er <- data.frame(ID = character(),Njobs = numeric())
      for (i in seq_along(cl)) {
            idir=paste(cdir,cl[i],sep="")
            idf <- read.csv(idir)
            njobs=sum(complete.cases(idf) == TRUE)
            if(njobs > threshold){              
                  ar[length(ar)+1]=cor(idf[,"sulfate"],
                  idf[,"nitrate"],use="complete.obs")
            } else {
                  er[nrow(er)+1,] = c (cl[i],njobs)
            }
      }
      print(er)
      ar
}