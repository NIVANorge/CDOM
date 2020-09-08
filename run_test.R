library(tidyr)
library(dplyr)

source("read_sp.R")

# ------------- test  ---------------------------------

folder_in <- "example"
folder_out <- "example_out"

filelist<-list.files(path = folder_in, pattern = "*.sp", ignore.case = T, full.names = F)

# loop through the files and convert them
for(filename in filelist){
  cat(paste0(filename,"\n"))
  file<- paste0(folder_in,"/",filename)
  df <- read_sp(file)
  
  fileout<-paste0(folder_out,"/",substr(filename,1,nchar(filename)-3),".txt")
  write.table(df, file = fileout, sep = "\t", row.names = F, col.names = T, quote = F)
}


