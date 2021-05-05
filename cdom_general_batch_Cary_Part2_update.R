# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processing script for CDOM files (Cary 300 spectrophotometer)
# Part1- current script= cdom_general_batch_Carry_Part1.R
#        Reads raw data from Carry csv files, saves each observation as a separate file, removes blanks 
#        and adds infro from logfile
# Part2- calculates absorption -  cdom_general_batch_Carry_Part2_update.R
#        Reads the ind files from Part1 and process to absorption, plots arsorption spectras, 
#        saves processed files and plots 
# Part3- calculates cdom slopes-  cdom_general_batch_Carry_Part3_update.R
#        Reads the processed files from Part2 and fits several models for calculating cdom slope values, plots results, 
#        saves processed files with fitted values etc, and model parameters etc 
#        and a full summary file of cdom_abs at several wl, slope values and parameters of the models

# T.Harvey 042021 Inputs from C. Murray and L. Valestrand 
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


folderin<-"cdom_test_methods/ind"
folderout <-"cdom_test_methods/processed/" 
folderout_results<- "cdom_test_methods/"
folderout_plots<- "cdom_test_methods/plots/spectras/"

#Add logbook (check for latest version on K)
logbook <- read.delim ("log_cary_2104.txt")
logbook$Sampling_date<- as.character(logbook$Sampling_date)
logbook$Sampling_date <- as.Date(logbook$Sampling_date, "%d/%m/%Y")

#read in full loogbook 
logbook <- logbook %>%
  rename(Date = Sampling_date,Observation=filename) 


# read in 1 test file
#df<-read.csv("varflom2021/ind/Glomma_20200618.csv", header = T, sep=",",stringsAsFactors=F,comment.char ="")


#list files that containg the relevant stations based on the logbook
filelist<-list.files(path=folderin,pattern="*.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#calculate absorption from absorbance with correct cuvette length

#aCDOM(λ)= 2.3020585*absorbance(λ)/0.1

# What about correction of the blank- not needed for 300 Cary data! 

# run script that 1) get station name, 2) add lake names, 3) reads the data file, 4) join the datafile with station and lake names etc
# 5) calculates absorption from absorbance, 6) fit an exponential model between different slopes (can be changed accordingly) with 443 (or another) nm as ref wl
# 7) save the results as a tables, 8) saves the processeed absorption files as ind files
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set constants
l<-0.1 # length of the cuvette- here 10 cm= 0.1 m, make sure it is correct
k<-2.302585 # ln coefficient


#Deletes the summary datafiles if they exists

if(exists("dfabsSummary")){
  rm("dfabsSummary")
}

#read in files and save as ind files with wl, absorbance and absorption


for(file in filelist){
  df<-read.csv(paste0(folderin, "/", file), header = T, sep=",",stringsAsFactors=F,comment.char ="", fill = TRUE)
 
  # get the name of the file
  currentname<-substr(file,1,nchar(file)-4) 
  cat(paste0("Now working on ",currentname,"\n"))
  
   #rename col name
  colnames(df)[2] <- 'absorbance'
  
  # calculate absorption m-1
  df <- df %>% 
      mutate(absorption = (k*absorbance)/l) # 5) calculate absorption m-1
  # make as new processed file and save it 
 
  # set the name of output file
  filenameout <- paste0(folderout,currentname,".csv")
 
  df<- as.data.table(df) 
  
  # write the output file
  write.table(df,file=filenameout,col.names=T,row.names=F,sep=",",quote=F)
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
# plots of absorption
 
  plot(df$wl, df$absorption, pch=20,cex=0.8, xlab= "Wavelength, nm",ylab=expression(paste("Absorption, m"^"-1")))
  title(currentname) # add fine name to plot
  
  dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/",currentname,"_abs", ".png"))
  
}


  
  
  
  