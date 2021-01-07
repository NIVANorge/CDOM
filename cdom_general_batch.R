###### Processing script for CDOM files (old spectrophotometer, Lambda)

## 1 run script named "read_sp"-> function to chnage sp files to txt
## 2 run script "sp_to_text" transfer the sp files to txt files and saves them in folderin
## 3 set correct folder
## 4 direct to correct blank
## 5 run this script


#load data packaged needed, if not installed, download and install them by e.g. install.packages(dplyr)

library(dplyr)
library(readr)
library(cdom)
library(data.table)


##### If needed change folders accordingly to your path
# set working directory if neseccary
# setwd("C:/Users/THH/NIVA/CDOM/Processing_R/cdom_git/CDOM")

## 1) set folders
folderin <- "example_out" # change to correct folder
folderout <- "example_out_processed" # create a new folder where the txt files should be placed (e.g. within the raw data folder)
folderout_plots<- "example_plots"

# folder to collect data files from e.g. 
folderin<-"K:/Avdeling/214-Oseanografi/_LAB/CDOM/Data/190510/txt"
#folder to save procsessed files  -> create a new folder within the folder with data
folderout<-"K:/Avdeling/214-Oseanografi/_LAB/CDOM/Data/190510/G190510_processed"
#folder for plots -within the folder with data
folderout_plots<-"K:/Avdeling/214-Oseanografi/_LAB/CDOM/Data/190510/G190510/G190510_plots"


# 2) read in the blank file
blank<-read.table("C:/Users/THH/NIVA/CDOM/Processing_R/190510_2/BLANK/BLANK.txt", header=TRUE) # blank reading to be withdrawn from absorbance spectras
blank<-read.table("BLANK.txt", header=TRUE) # blank reading to be withdrawn from absorbance spectras


# read a list of csv files from the input folder
filelist<-list.files(path=folderin,pattern="*.txt")
filelistout<-list.files(path=folderout,pattern="*.txt")

# test read of 1 file
df<-read.table("example_out/1.txt", header=TRUE)

#standard<-read.table("cDOM/G191014/ASCI/standard_2019.txt", header=TRUE)

#calculate absorption from absorbance with correct cuvette length
#aCDOM(λ)= 2.3020585*absorbance(λ)/0.1

#### ---------what about correction of the blank- yes! use "blank" ### --------- 
# set constants

l<-0.1 # length of the cuvette- here 10 cm= 0.1 m
k<-2.302585 # ln coefficient


#list of station names  - can be excluded
  # stations<-c("brottum","brottum_copy","Eikeren1","Eikeren2","Furnest","Kise","Skreia", "TyrP", "Steinsfjorden","Utlop_Storelva" ,
  #             "van1","van2", "stan")
## add station names
  #stations<- c("Djupfest_20190101", "Valset_20190101", "Biologisk_Stasjon_20190101")
  
  station<- c("stn1", "stn2", "stn3", "stn4", "stn5")
  
  #list of area or lake names- can be excluded
  area<-c("Mjosa","Mjosa","Eikeren","Eikeren", "IO")
  #,"Mjosa","Mjosa","Tyrifjorden", "Tyrifjorden", "Tyrifjorden", "Vansjø", "Vansjø", "Standard")
 
  #list year/years if it is the same year it is enought to state it once otherwise list every year
  #year<-c("2020","Mjosa","Eikeren","Eikeren", "Mjosa","Mjosa","Mjosa","Tyrifjorden", "Tyrifjorden", "Tyrifjorden", "Vansjø", "Vansjø", "Standard")
  year<- 2020
  #list month/months if it is the same year it is enought to state it once otherwise list every year
  month<- c("May","June","May","Aug", "Sep")
  #list date/dates if it is the same year it is enought to state it once otherwise list every year
  day<- c(01,02,02,20,23)
 #combine 1 table
  infotable<- data.frame(station, area, year, month, day) 

  
#Deletes the summary datafiles if they exists
  
if(exists("dfSummary")){
  rm("dfSummary")
}  
  if(exists("dfAbsAll")){
    rm("dfAbsAll")
  }   
  
  # if(exists("sSummary")){
  #   rm("sSummary")
  # }   
  
  # run script that 1) get station name, 2) add lake names, 3) reads the data file, 4) join the datafile with station and lake names
  # 5) calculates absorption from absorbance, 6) fit an exponential model between 400 nm and 550 nm (can be changed accordingly) with 443 nm as ref wl
  # 7) save the results as a table, 
  
for(file in filelist){
  
  #station<-substr(file,1,nchar(file)-4) 
  
#  station<-substr(file,1,nchar(file)-13) 
#  lake<-lakes[stations==station]
   
  
  df <-read.table(paste0(folderin,"/",file), header=TRUE) #3) read in data file
  cat(paste0(file," (", nrow(df),")\n"))
  df <- left_join (df, blank, by="wl") # join with correction file (blank)
  
  df <- df %>% 
   mutate(abc_corr = absorbance.x- absorbance.y)%>%  # correct the absorbance
   select( c(wl, abc_corr)) %>% # choose the corredted absorbance and wl columns
   mutate(absorption = (k*abc_corr)/l) # 5) calculate absorption m-1
  
   fit<- cdom_fit_exponential(df$wl,df$absorption, wl0 = 443,startwl=400,endwl=550 ) # 6) fit a model 

  results<-as.data.frame(fit$data) # save results (fitted and residuals data from the model) as a data frame
  results <- as.data.table(results) #change to data table
  results <- dplyr::rename(results, absorption = y)
  results <- dplyr::rename(results, wl = x)
  results <- as.matrix(results) #removes extra attributes in name
  results <- as.data.frame(results) #change to data table
  results <- as.data.table(results) #change to data table
  
  params<- as.data.frame(fit$params) #table of model parameter; i.e S, g443, slope
  
  g443<- results[wl == 443] # extraxt row and absorption at 443 nm
 
  # extract and add R2, slope and a0, value from the model
  r2 <- fit$r2
  S <- params[1,2]

  a0 <- params[3,2]

  #g443<- g443 %>% mutate(S_400_550=S, a0=a0, r2=r2, station=station)
  
  g443<- g443 %>% mutate(S_400_550=S, a0=a0, r2=r2)
  
  # slopetest<- g443 %>% mutate(S_400_550=S, a0=a0, r2=r2, lake=lake, station=station)
  
  #create a summary file of the results

  
  if(exists("dfSummary")){
    dfSummary <- dfSummary %>%
    bind_rows(mutate(g443,Observation=file))# %>%
    # bind_rows(mutate(g410,Observation=file,S_400_550=S, a0=a0, r2=r2, lake=lake, station=station)) %>%
    #  bind_rows(mutate(g380,Observation=file,S_400_550=S, a0=a0,r2=r2, lake=lake, station=station))
    
  }else{
    dfSummary <- mutate(g443,Observation=file) # %>%
   # bind_rows(mutate(g410,Observation=file,S_400_550=S, a0=a0, r2=r2, lake=lake, station=station)) %>%
    # bind_rows(mutate(g380,Observation=file,S_400_550=S, a0=a0, r2=r2, lake=lake, station=station))
  }
  
 # nm <- c("Observation","station","wl","absorption","S_400_550","a0", "r2", "wl0",".fitted",".resid")
  nm <- c("Observation","wl","absorption","S_400_550","a0", "r2", "wl0",".fitted",".resid")
  
    setcolorder(dfSummary, c(nm, setdiff(names(dfSummary), nm)))
  

  #create a file with all spectras
  # if(exists("dfAbsAll")){
  #   dfAbsAll <- dfAbsAll %>%
  #     bind_rows(mutate(results,Observation=file, lake=lake, station=station))
  # }else{
  #   dfAbsAll <- mutate(results,Observation=file, lake=lake, station=station)
  # }  
  
  #create a file with all spectras
  if(exists("dfAbsAll")){
    dfAbsAll <- dfAbsAll %>%
      bind_rows(mutate(results,Observation=file))
  }else{
    dfAbsAll <- mutate(results,Observation=file)
    
  }  
  

  ########## plots- if the plot should be saved activate the script
  # plot(results$wl, results$.resid, pch=20, xlab= "Wavelength, nm",ylab= "Residuals" )
  # abline(h=0, col="black", lwd= 2)
  # dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","res_",file, ".jpeg"))
  # 
  # 
  # plot(results$absorption, results$.fitted, pch=20, xaxs="i", yaxs="i", mgp=c(2.2,1,0),
  #      xlab= expression(paste("Absorption, m"^"-1")),ylab= expression(paste("Fitted, m"^"-1")))
  # abline(0,1, lwd=2) #1:1 line
  # dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","fitted_",file, ".jpeg"))
  # 
  # plot(results$wl, results$absorption, pch=1, cex=0.75, xlab= "Wavelength, nm",
  #      ylab=expression(paste("Absorption, m"^"-1")))
  # #points(results$wl, results$.fitted, pch=4)
  # lines(results$wl, results$.fitted, col = "red")
  # points(g443$wl, g443$absorption, pch=18, col="red", cex=1.5)
  # 
  # dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","spectra_",file,".jpeg"))
  
# save results and tables- for individual files 

  # write.table(results,file=paste0(folderout,"/","fit_*.txt",file),sep=",",row.names=F,quote=F)
  write.table(params,file=paste0(folderout,"/","para_",file),sep=",",row.names=F,quote=F)
  write.table(g443,file=paste0(folderout,"/","g443_",file),sep=",",row.names=F,quote=F)

  
}

  
  stations<- as.data.frame(stations)    
  infotable<- infotable %>% mutate(Observation=c("1.txt", "2.txt", "3.txt", "4.txt", "5.txt"))
  

dfAbsAll<- left_join(dfAbsAll, infotable, by="Observation")
dfSummary<- left_join(dfSummary, infotable, by="Observation")
 
  
 #save summary files
write.table(dfSummary,file=paste0(folderout,"/","results_all.txt"),sep=",",row.names=F,quote=F)
write.table(dfAbsAll,file=paste0(folderout,"/","spectra_all.txt"),sep=",",row.names=F,quote=F)

    
