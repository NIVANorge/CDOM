library(dplyr)
library(ggplot2)
library(gtools)
library(readr)
library(cdom)
library(data.table)
library(openxlsx)
library(tidyverse)
library (lubridate)
library(filesstrings)
library(plyr)

rm(list = ls())
#g_navne is the cdom folder name. Put it here once for the whole script, except where you add the logbook further down in the script
g_navn <- "G200228_1"

## set folders
dir.create(paste0("CDOM/", g_navn, "/ASCI"))
dir.create(paste0("CDOM/", g_navn, "/", g_navn, "_processed"))
dir.create(paste0("CDOM/", g_navn, "/Plots"))
dir.create(paste0("CDOM/", g_navn, "/blank"))
folderin <- paste0("CDOM/", g_navn, "/ASCI") # change to correct folder
folderout <- paste0("CDOM/", g_navn,"/", g_navn, "_processed") # create a new folder where the txt files should be placed (e.g. within the raw data folder)
folderout_plots<- paste0("CDOM/", g_navn, "/Plots")

#add logbook for samplename, depth etc
logbook <- read.delim ("CDOM/Log_2.txt")
logbook$Sampling_date <- as.Date(logbook$Sampling_date, "%d.%m.%Y")


# STEP 1: converte sp files to txt
folder_in <- paste0("CDOM/", g_navn)
folder_out <-  paste0("CDOM/", g_navn, "/ASCI")

read_sp <- function( filename, identifier = "#DATA", colnames=c("wl","absorbance")){
  # read df as one column
  df <- read.csv(filename, fill = TRUE, header = F, stringsAsFactors = F)
  
  # find the row number with the identifier for the start if the actual data
  nstart<- which(df$V1==identifier) + 1
  
  # select the rows after this row
  df <- df %>%
    slice(nstart : n())
  
  # split the column into wavelength and absorbance
  df <- df %>%
    separate(col = "V1", sep = "\t", into = colnames)
  
  return(df)
}


filelist<-list.files(path = folder_in, pattern = "*.sp", ignore.case = T, full.names = F)

# loop through the files and convert them
for(filename in filelist){
  cat(paste0(filename,"\n"))
  file<- paste0(folder_in,"/",filename)
  df <- read_sp(file)
  
  fileout<-paste0(folder_out,"/",substr(filename,1,nchar(filename)-3),".txt")
  write.table(df, file = fileout, sep = "\t", row.names = F, col.names = T, quote = F)
}

#move blank.txt to blank folder
file.move(paste0("CDOM/", g_navn, "/ASCI/BLANK.txt"), paste0("CDOM/", g_navn, "/blank"))

# STEP 2: read in the files

# read blank
blank<-read.table(paste0("CDOM/", g_navn, "/blank/BLANK.txt"), header=TRUE)

#select right foldername in log.xlsx and select relevant metadata. Remenber to add right "mappe" name from logfile
mappe <- "200228_I" # change to correct folder

log1 <- logbook %>%
  filter(Folder == mappe) %>%
  select(Sampling_date, Depth, Station_name, .SP, Folder) %>%
  dplyr::rename(Date = Sampling_date)

view(log1)

# read a list of csv files from the input folder
filelist<-list.files(path=folderin,pattern="*.txt")
#sort the files in chronological order
filelist<- mixedsort(sort(filelist))
filelistout<-list.files(path=folderout,pattern="*.txt")


# Create a INFOTABLE with the infomation needed e.g. corresponing to the scans order staring with 1 and increasing
#so that the corresponing information is listed in the correct order

infotable <- subset(log1, select = c(Station_name, Date, Depth, .SP, Folder))

# Adds a column "Observation" that takes the number from .SP column and adds .txt to the end
infotable$Observation <- paste0(infotable$.SP, ".txt") 

#calculate absorption from absorbance with correct cuvette length
#aCDOM(??)= 2.3020585*absorbance(??)/0.1

#### ---------what about correction of the blank- yes! use "blank" ### --------- 
# set constants

l<-0.1 # length of the cuvette- here 10 cm= 0.1 m, make sure it is correct
k<-2.302585 # ln coefficient

######
#START the scrip from here!!!
##### 

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
  name<-substr(file,1,nchar(file)-4) 
  
  #  station<-substr(file,1,nchar(file)-13) 
  #  lake<-lakes[stations==station]
  
  
  df <-read.table(paste0(folderin,"/",file), header=TRUE) #3) read in data file
  cat(paste0(file," (", nrow(df),")\n"))
  df <- left_join (df, blank, by="wl") # join with correction file (blank)
  
  df <- df %>% 
    mutate(abc_corr = absorbance.x- absorbance.y)%>%  # correct the absorbance
    select( c(wl, abc_corr)) %>% # choose the corredted absorbance and wl columns
    mutate(absorption = (k*abc_corr)/l) # 5) calculate absorption m-1
  
  fit <- try(cdom_fit_exponential(df$wl,df$absorption, wl0 = 443,startwl=400,endwl=550 ), silent = T) # 6) fit a model 
  if(isTRUE(class(fit) == "try-error")) { next }
  
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
  plot(results$wl, results$.resid, pch=20, xlab= "Wavelength, nm",ylab= "Residuals" )
  abline(h=0, col="black", lwd= 2)
  dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","res_",name, ".jpeg"))
  
  # 
  # 
  plot(results$absorption, results$.fitted, pch=20, xaxs="i", yaxs="i", mgp=c(2.2,1,0),
       xlab= expression(paste("Absorption, m"^"-1")),ylab= expression(paste("Fitted, m"^"-1")))
  abline(0,1, lwd=2) #1:1 line
  dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","fitted_",name, ".jpeg"))
  # 
  plot(results$wl, results$absorption, pch=1, cex=0.75, xlab= "Wavelength, nm",
       ylab=expression(paste("Absorption, m"^"-1")))
  points(results$wl, results$.fitted, pch=4)
  lines(results$wl, results$.fitted, col = "red")
  points(g443$wl, g443$absorption, pch=18, col="red", cex=1.5)
  # 
  dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","spectra_",name,".jpeg"))
  
  # save results and tables- for individual files 
  
  # write.table(results,file=paste0(folderout,"/","fit_*.txt",file),sep=",",row.names=F,quote=F)
  write.table(params,file=paste0(folderout,"/","para_",file),sep=",",row.names=F,quote=F)
  write.table(g443,file=paste0(folderout,"/","g443_",file),sep=",",row.names=F,quote=F)
  
  
}



dfAbsAll<- left_join(dfAbsAll, infotable, by="Observation")
dfSummary<- left_join(dfSummary, infotable, by="Observation")


#save summary files
write.table(dfSummary,file=paste0(folderout,"/","results_all.txt"),sep=",",row.names=F,quote=F)
write.table(dfAbsAll,file=paste0(folderout,"/","spectra_all.txt"),sep=",",row.names=F,quote=F)



#select all files within wd that are called "results_all.txt" BUT check that you are in the right dir()!


rm(list=ls())


a <- list.dirs()
b <- a[grep(x = a, pattern = "_processed$")]
b <- b[-3]

one_list_to_rule_them_all <- data.frame()
for(i in b){
  x <- read.csv(paste0(i, "/results_all.txt"), header = T)
  one_list_to_rule_them_all <- rbind(one_list_to_rule_them_all, x)
}
write.table(one_list_to_rule_them_all, file = "lamba_all.txt")