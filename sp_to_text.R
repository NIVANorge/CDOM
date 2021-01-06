library(tidyr)
library(dplyr)

source("read_sp.R")

# ------------- change sp to txt file  ---------------------------------

folder_in <- "example" # change to correct folder
#e.g
#folderin<-"K:/Avdeling/214-Oseanografi/_LAB/CDOM/Data/190510"

folder_out <- "example_out" # create a new folder where the txt files should be placed (e.g. within the raw data folder)

#folder_out<-"K:/Avdeling/214-Oseanografi/_LAB/CDOM/Data/190510/txt"

filelist<-list.files(path = folder_in, pattern = "*.sp", ignore.case = T, full.names = F)

# loop through the files and convert them
for(filename in filelist){
  cat(paste0(filename,"\n"))
  file<- paste0(folder_in,"/",filename)
  df <- read_sp(file)
  
  fileout<-paste0(folder_out,"/",substr(filename,1,nchar(filename)-3),".txt")
  write.table(df, file = fileout, sep = "\t", row.names = F, col.names = T, quote = F)
}


