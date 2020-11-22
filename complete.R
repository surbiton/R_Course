complete <- function (directory, id=1:332) {
## read files based on 'id' in the 'directory'
      dir <- "C:\\Users\\sunc01\\Downloads\\rprog_data_specdata\\"
      cdir <- paste (dir,directory, '\\',sep="")
      df <- data.frame(ID = integer(),Njobs = integer())
      for (i in seq_along(id)) {
            idir <- paste(cdir,aid <- sprintf("%03d.csv", id[i]),sep="")
            idf <- read.csv(idir)
            njobs=sum(complete.cases(idf) == TRUE)
            df[nrow(df)+1,] = c (id[i],njobs)
      }
      df
}