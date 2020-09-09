# ------------- read_csv function ---------------------------------
read_csv <- function( filename){
  # read df as one column
  
  
  ### test of one file ####
  folderout<-"C:/Users/THH/OneDrive - NIVA/CDOM_processing/CDOM_processing/cdom_test_methods/processed"
  
  df <- read.csv("cdom_test_methods/200708.csv", fill = TRUE, header = T, stringsAsFactors = F)
  # switch all colnames by 1
  
  df$X.33<-1 # add 1 to last NA column
  mylist <- colnames(df) # list colnames
  mylist <- c("wl", mylist) # add a new colname "wl"
  mylist<-mylist[!grepl("X.33",unlist(mylist))] # removes last coulmn
  colnames(df) <- mylist
  # remove row 1 and "no data" rows
  
 df<- df %>% slice(-1) %>%  # remove 1st row with names
            select(-X.32) # remove dummy column
  df = df[1:711, ] #removes the rest
  df <- df[!duplicated(as.list(df))] #remove all duplicate wl columns
  #save wl as separate df
  wl_nm<-as.data.frame(df$wl)
  colnames(wl_nm) <- "wl" # change colname to wl
  wl_nm$wl <- as.numeric(as.character( wl_nm$wl)) # make numeric
  df<- df  %>%  
    select(-wl) # remove wl column  
  
  #create a separate dataframe for each colum and merge with wl
  # set working directory to folder to save in 
  setwd("C:/Users/THH/OneDrive - NIVA/CDOM_processing/CDOM_processing/cdom_test_methods/processed")
  
  lapply(names(df), function(colname) write.table(df[,colname],sep=",", file=paste0(colname,'.csv')))
  lapply(names(df), function(colname) write.table(df[,colname],sep=",",prefix=c("x", "absorbance"), file=paste0(colname,'.csv')))
  
  
  # read in and wl and resave as csv
  
  folderin<-"C:/Users/THH/OneDrive - NIVA/CDOM_processing/CDOM_processing/cdom_test_methods/processed"
  folderout<-"C:/Users/THH/OneDrive - NIVA/CDOM_processing/CDOM_processing/cdom_test_methods/processed"
  
  filelist<-list.files(path=folderin,pattern="*.csv")
  
 
  for(filename in filelist)
  {
    cat(paste0(filename,"\n"))
    file<- paste0(folderin,"/",filename)
    dftemp <- read_table(filename, sep ="\t", fill = TRUE, header = F, stringsAsFactors = F)
    colnames(dftemp) <- c("x", "absorbance")
    dftemp<- dftemp %>% select(-x) # remove dummy column
    dftemp$absorbance <- as.numeric(as.character( dftemp$absorbance)) # make numeric
    dftemp <-rbind(dftemp, wl_nm)
    fileout <-file=paste0(colname,'.csv')
    write.table(dftemp, "C:/Users/THH/OneDrive - NIVA/CDOM_processing/CDOM_processing/cdom_test_methods/processed", file = fileout, sep = ",", row.names = F, col.names = T, quote = F)
  }
  
    
    fileout <-file=paste0(colname,'.csv'))
    write.table(dftemp, file = fileout, sep = "\t", row.names = F, col.names = T, quote = F)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  #### VERSION 1 ####
  # Creates a new dataframe from each column, but lose the original column names in the new dataframes
  for(i in 1:ncol(df))
  {assign(colnames(df)[i], data.frame(df[,i]))}
  
  
  for(file in 1:ncol(df))
  {
   dftemp <- assign(colnames(df)[file], data.frame(df[,file()]))
    colnames(dftemp) <- "absorbance"
    dftemp <-cbind(dftemp, wl_nm)
    
    
    fileout<-paste0(folderout,"/",substr(file,1,nchar(file)),".csv")
    write.table(dftemp, file = fileout, sep = "\t", row.names = F, col.names = T, quote = F)
        }
  
  for (file in 1:ncol(df)) {
    dftemp <- assign(colnames(df)[file], data.frame(df[,file()]))
    colnames(dftemp) <- "absorbance"
    dftemp <-cbind(dftemp, wl_nm)
      fileout <- file.path("C:/Users/THH/OneDrive - NIVA/CDOM_processing/CDOM_processing/cdom_test_methods/processed", paste0(dftemp,".csv"))   #  fileout <-file=paste0(colname,'.csv')
    write.table(dftemp, file = fileout, sep = ",", row.names = FALSE, col.names = T,
                quote = FALSE, append = FALSE)
  }
  
for (i in 1:ncol(df)) {
  dftemp <- data.frame(df[, i])
  mytime <- format(Sys.time(), "%b_%d_%H_%M_%S_%Y")
  myfile <- file.path(tempdir(), paste0(mytime, "_", i, ".txt"))
  write.table(a, file = myfile, sep = "", row.names = FALSE, col.names = FALSE,
              quote = FALSE, append = FALSE)
}
  
  
  #### VERSION 2 ####
  # Creates a new dataframe from each column, maintains the original column names in the new dataframes
  for(i in 1:ncol(df))
  {temp <- data.frame(df[,i])
  colnames(temp) <- colnames(df)[i]
  assign(colnames(df)[i], temp)
  rm(temp)
  }
  
  for(file in 1:ncol(df))
  {temp <- data.frame(df[,file])
  colnames(temp) <- colnames(df)[file]
  assign(colnames(df)[file], temp)
  rm(temp)
  }
  
  
  for(file in 1:ncol(df)) {
  temp <- data.frame(df[,file])
  colnames(temp) <- colnames(df)[file]
  assign(colnames(df)[file], temp)
  rm(temp)
    }

  savelist<- colnames(df)
 

  for(i in 1:ncol(df))
  {temp <- data.frame(df[,i])
  colnames(temp) <- colnames(df)[i]
  assign(colnames(df)[i], temp)
  rm(temp)
  write.csv()
  write.table(results,file=paste0(folderout,"/","fit_*.txt",file),sep=",",row.names=F,quote=F)
  
  }
  
  as.double(colnames(df)[i])
  

folderin<-"cdom_test_methods"

  
  # find the row number with the identifier for the end of the actual data
  
  expr[df$Baseline.100.T %in% [, 1:712]
  
  #nend<- which(df[1,]==identifier) + 1
  
  # select the rows after this row
  #df <- df %>%
   # slice(1 : nend())
  
  # split the column into wavelength and absorbance
 # df <- df %>%
    #separate(col = df[1,], sep = ",", into = colnames)
  
  return(df)
}