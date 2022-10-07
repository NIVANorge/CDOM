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

#dir.create(paste0("FB2021_extra/","/ind"))
dir.create(paste0("ØKOKYST_2021/", "/results"))
dir.create(paste0("ØKOKYST_2021/", "/processed"))
dir.create(paste0("ØKOKYST_2021/", "/plots"))
dir.create(paste0("ØKOKYST_2021/", "/plots/", "/spectra"))
dir.create(paste0("ØKOKYST_2021/", "/plots/", "/models"))

# change first part of name to the correct folder

folderin<-"cdom_cary_NOLASIS_WP2/ind"
folderin<-"cdom_cary_NOLASIS_WP2/CDOM_NOLA_07092022/ind/"

folderout <-"cdom_cary_NOLASIS_WP2/CDOM_NOLA_07092022/processed/" 
folderout_results<- "cdom_cary_NOLASIS_WP2/CDOM_NOLA_07092022/"
folderout_plots<- "cdom_cary_NOLASIS_WP2/CDOM_NOLA_07092022/plots/spectra/"

#Add logbook (check for latest version on K)
logbook <- read.delim ("ØKOKYST_2021/log_cary.txt")
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
# correct the absorbance spectra for refraction scattering by substracting the average value betwen 700-800 nm 
# from the spectra according to;
# Green, S. A., & Blough, N. V. (1994). Optical absorption and fluorescence properties of chromophoric dissolved organic matter in natural waters. Limnology and Oceanography, 39(8), 1903–1916. https://doi.org/10.4319/lo.1994.39.8.1903
# Bricaud, A., Morel, A., & Prieur, L. (1981). Absorption by Dissolved Organic Matter of the Sea (Yellow Substance) in the UV and Visible Domains. Limnology and Oceanography, 26(1), 43–53. https://doi.org/10.2307/2835805


# calculate absorption from absorbance with correct cuvette length

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
  
  # Account for absorbance Offset by substract the average absorbance  at 700-800 nm
  avg<- df%>% filter(wl>=700, wl<=800)%>%
   summarise(avg=mean(absorbance))
  avg$avg<- abs(avg$avg)
    # calculate absorption m-1 based on corrected samples

  df<-df %>% mutate(absorbance_corr=absorbance+avg$avg)


 df <- df %>%
    mutate(absorption = (k*absorbance_corr)/l) # 5) calculate absorption m-1

  #without correction
 # df <- df %>% 
 #    mutate(absorption = (k*absorbance)/l) # 5) calculate absorption m-1
 # 
  # make as new processed file and save it 
 
  # set the name of output file
  filenameout <- paste0(folderout,currentname,".csv")
 
  df<- as.data.table(df) 
  
  # write the output file
  write.table(df,file=filenameout,col.names=T,row.names=F,sep=",",quote=F)
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
# plots of absorption
 
  df<- filter (df, wl>=240)
  plot(df$wl, df$absorption,xlim=c(350,700), pch=20,cex=0.8, xlab= "Wavelength, nm",ylab=expression(paste("Absorption, m"^"-1")))
  title(currentname) # add fine name to plot
  
  
  dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/",currentname,"_abs", ".png"))
  
}


  
  
  
  