library(tidyverse)


  filename <- "./cdom_test_methods/200708.csv"
  nrows <- 711
  
  folderout<-"./cdom_test_methods/processed/"
  
  dfhead <- read.table(filename,header=TRUE,sep =',',stringsAsFactors=F,nrows=1)
  df <- read.table(filename,skip=1,header=TRUE,sep =',',stringsAsFactors=F,nrows=nrows)
  
  # get the names of the columns from dfhead
  names(df) <- names(dfhead)
  
  # get number of columns
  ncols <- ncol(dfhead)
  
  # get the column names of the first (n-1) columns in dfhead, omitting the name of column n
  colnames <- names(dfhead[,1:ncols-1])

  # set the column names in df1 using "wl" for the first column and then filling in with colnames
  names(df) <- c("wl",colnames)
  
  # select column 1 + columns 2,4,... ncols-1 (dropping the X. columns)
  df <- df[,c(1,seq(2,ncols-1,2))]
  
  # loop through the columns in df from column 2 to the last column
  for(i in 2:ncol(df)){
    # get the name of the column
    currentname <- names(df)[i]
    cat(paste0("Now working on ",currentname,"\n"))
    
    # create a dataframe with only the wl column and the current column
    dftemp <- df[,c(1,i)]
    
    # set the name of output file
    filenameout <- paste0(folderout,currentname,".csv")
    
    # write the output file
    write.table(dftemp,file=filenameout,col.names=T,row.names=F,sep=",",quote=F)
    
  }
  
  