# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processing script for CDOM files (Carry 300 spectrophotometer)
# Part1- current script= cdom_general_batch_Carry_Part1.R
#        Reads raw data from Carry csv files, saves each observation as a separate file, removes blanks 
#        and adds infro from logfile
# Part2- calculates absorption -  cdom_general_batch_Carry_Part2.R
#        Reads the ind files from Part1 and fits a model for calculationg cdom slope values, plots, 
#        saves processed files and fitted values etc
# Part3- calculates cdom slopes-  cdom_general_batch_Carry_Part3.R

# T.Harvey 042021 Inputs from C. Murray, L. Valestrand and S. Marty
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  set correct folders for "folderin, folderout" in this script

#load data packaged needed, if not installed, download and install them by e.g. install.packages(dplyr)

library(dplyr)
library(tidyverse)
library(readr)
library(cdom)
library(data.table)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dir()
getwd()

folderin<-"varflom2021"
folderout <-"varflom2021/ind/" 
folderout_results<- "varflom2021/results/"


#Add logbook (check for latest version on K)
logbook <- read.delim ("log_cary_2104.txt")
logbook$Sampling_date<- as.character(logbook$Sampling_date)
logbook$Sampling_date <- as.Date(logbook$Sampling_date, "%d/%m/%Y")

#select the relevant observatons (here for filtertest and vårflom)
log1 <- logbook %>%
  rename(Date = Sampling_date) %>%
  filter(Project==c("filtertest","filtertest_vårflom", "vårflom" ))

#add another relevant file
log2 <- logbook %>%
  rename(Date = Sampling_date) %>%
  filter(Folder=="210330")

log1<- rbind(log1, log2)
rm(log2)
rm(logbook)

#files to include fro vårflom- copy to folderin
# print(log1$Folder)
# 200708 200709   200814  200818 201016  201110   210316 210317   210329   210316_1

# read in 1 test file
#df<-read.csv("cdom_test_methods/varflom/200709.csv", header = T, sep=",",stringsAsFactors=F,comment.char ="")


#list files that containg the relevant stations based on the logbook
filelist<-list.files(path=folderin,pattern="*.csv")

#read in files and save as ind files


for(file in filelist){
  df<-read.csv(paste0(folderin, "/", file), header = T, sep=",",stringsAsFactors=F,comment.char ="", fill = TRUE)
  # switch all colnames by 1
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fix df: colnames, remove redundant rows/info, last name, move colnames 1 to the right and add wl to the first colname
  # dropping wl columns:X:s, dropping blank readings
  
  df<-df[!is.na(as.numeric(df[,1])),] #removes redunant info and row 1
  # get the names of the columns from dfhead
  names(df) <- names(df)
    # get number of columns
  ncols <- ncol(df)
    # get the column names of the first (n-1) columns in dfhead, omitting the name of column n
  colnames <- names(df[,1:ncols-1])
    # set the column names in df1 using "wl" for the first column and then filling in with colnames
  names(df) <- c("wl",colnames)
  #remove last column
  df<-df[,!is.na(df[3,])] #removes last column= NA
  
  # select column 1 + columns 2,4,... ncols-1 (dropping the X. columns)
  df <- df[,c(1,seq(2,ncols-1,2))]
  
  #remove potential blank/MQ scans-OBS-this will be changed to be included with ind.names foor MQ samples
  
    options(warn=-1) #supresses general warnings- here of columns not present
  
  df<- df %>% select(-one_of("blank", "Blank","Blank_2", "blank_2", "Blank_3","Blank_4","blank1","Blank2","Blank4",
                             "MQ_water", "mq_water","mq" , "MQ","Mq", "Baseline.100.T", "Baseline.100.T.1", "Baseline.100.T.2",
                             "Baseline.100.T.3","Baseline.100.T.4", "Mq water","Mq.water",  "MQ_water_2","MQ_water_3","Mq1","MQ_1", "MQ_4",
                             "MQ1",  "MQ2",  "MQ3",  "MQ4", "MQvann"))
  
    options(warn=0) #turns  warning on again
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save each scan as ind file  
    
    # make numeric
    df <- as.data.frame(sapply(df, as.numeric)) 
    
    # get number of columns
    ncols <- ncol(df)
    
    # get the column names of the first columns in df
    names <- colnames(df)  
    
    # loop through the columns in df from column 2 to the last column
    
    #save each scan as ind file    
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
}    

