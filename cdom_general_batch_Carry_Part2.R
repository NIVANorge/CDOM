# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processing script for CDOM files (Carry 300 spectrophotometer)
# Part1- current script= cdom_general_batch_Carry_Part1.R
#        Reads raw data from Carry csv files, saves each observation as a separate file, removes blanks 
#        and adds infro from logfile
# Part2- calculates absorption -  cdom_general_batch_Carry_Part2.R
#        Reads the ind files from Part1 and prosecc to absorption, plots arsorption spectras, 
#        saves processed files and plots and summary file of cdom_abs at several wl
# Part3- calculates cdom slopes-  cdom_general_batch_Carry_Part3.R
#        Reads the processed files from Part2 and fits several models for calculating cdom slope values, plots results, 
#        saves processed files with fitted values etc, and model parameters etc

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

folderin<-"varflom2021/ind"
folderout <-"varflom2021/processed/" 
folderout_results<- "varflom2021/results/"
folderout_plots<- "varflom2021/plots/spectras"

#Add logbook (check for latest version on K)
logbook <- read.delim ("log_cary_2104.txt")
logbook$Sampling_date<- as.character(logbook$Sampling_date)
logbook$Sampling_date <- as.Date(logbook$Sampling_date, "%d/%m/%Y")

#read in full loogbook 
logbook <- logbook %>%
  rename(Date = Sampling_date,Observation=filename) 

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
df<-read.csv("varflom2021/ind/Glomma_20200618.csv", header = T, sep=",",stringsAsFactors=F,comment.char ="")


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

######
#START the scrip from here!!!
##### 


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
  
  # extract raw absorbance  and absorption at x nm
  g250<- df[wl == 250] 
  g254<- df[wl == 254] 
  g365<- df[wl == 365] 
  g375<- df[wl == 375]
  g380<- df[wl == 380]
  g410<- df[wl == 410]
  g443<- df[wl == 443] 
  
  #save absorption values in 1 table for each file
  if(exists("dfabsSummary")){
    dfabsSummary <- dfabsSummary %>%
      bind_rows(mutate(g250,Observation=currentname)) %>%
      bind_rows(mutate(g254,Observation=currentname)) %>%
      bind_rows(mutate(g365,Observation=currentname))  %>%
      bind_rows(mutate(g375,Observation=currentname))  %>%
      bind_rows(mutate(g380,Observation=currentname))  %>%
      bind_rows(mutate(g410,Observation=currentname))  %>%
      bind_rows(mutate(g443,Observation=currentname))
    
  }else{
    dfabsSummary <- mutate(g250,Observation=currentname)  %>%
      bind_rows(mutate(g254,Observation=currentname)) %>%
      bind_rows(mutate(g365,Observation=currentname))  %>%
      bind_rows(mutate(g375,Observation=currentname))  %>%
      bind_rows(mutate(g380,Observation=currentname))  %>%
      bind_rows(mutate(g410,Observation=currentname))  %>%
      bind_rows(mutate(g443,Observation=currentname))
  }
  
  #reorder columns
  nm <- c("Observation","wl","absorbance","absorption")
  
  setcolorder(dfabsSummary, c(nm, setdiff(names(dfabsSummary), nm)))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
# plots of absorption
 
  plot(df$wl, df$absorption, pch=20,cex=0.8, xlab= "Wavelength, nm",ylab=expression(paste("Absorption, m"^"-1")))
  title(currentname) # add fine name to plot
  
  dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/",currentname,"_abs", ".png"))
  
}

#add info  from loogbook
dfabsSummary<- left_join(dfabsSummary, logbook, by="Observation")

dfabsSummary_sel <- dfabsSummary %>%
   filter(Project==c("filtertest","filtertest_vårflom", "vårflom" ))
#add to filtertest files
dfabsSummary_test<- dfabsSummary %>%
  filter(Station_name==c("MQ_test1", "MQ_test2"))

dfabsSummary_sel<- bind_rows(dfabsSummary_sel, dfabsSummary_test)


write.table(dfabsSummary,file=paste0(folderout_results,"/","abs_results_all.csv"),sep=",",row.names=F,quote=F)
write.table(dfabsSummary_sel,file=paste0(folderout_results,"/","abs_results_sel.csv"),sep=",",row.names=F,quote=F)


  
  
  
  
  