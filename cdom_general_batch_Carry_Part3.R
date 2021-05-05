# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processing script for CDOM files (Cary 300 spectrophotometer)
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

#dir()
#getwd()

folderin<-"lingning_etc/processed/" 
folderout_results<- "lingning_etc/results/"
folderout_plots<- "lingning_etc/plots/models/"


folderin<-"varflom2021/processed"
folderout_results<- "varflom2021/results/"
folderout_plots<- "varflom2021/plots/models/"

#Add logbook (check for latest version on K)
logbook <- read.delim ("log_cary_2104.txt")
logbook$Sampling_date<- as.character(logbook$Sampling_date)
logbook$Sampling_date <- as.Date(logbook$Sampling_date, "%d/%m/%Y")

#read in full loogbook 
logbook <- logbook %>%
  rename(Date = Sampling_date,Observation=filename) 

#add another relevant file
# log2 <- logbook %>%
#   rename(Date = Sampling_date) %>%
#   filter(Folder=="210330")
# 
# log1<- rbind(log1, log2)
# rm(log2)
# rm(logbook)


#files to include fro vårflom- copy to folderin
# print(log1$Folder)
# 200708 200709   200814  200818 201016  201110   210316 210317   210329   210316_1

# read in 1 test file
df<-read.csv("varflom2021/processed/Glomma_20200618.csv", header = T, sep=",",stringsAsFactors=F,comment.char ="")

currentname<- "Glomma_20200618"

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



#Deletes the summary datafiles if they exists

# summary file of results absorbance, absorption, slope values, etc
if(exists("dfSummary")){
  rm("dfSummary")
}  

# summary file of all spectras used in the model
if(exists("dfAbsAll")){
  rm("dfAbsAll")
}   

# summary file of all spectras used in model 
if(exists("dfAbsmod1")){
  rm("dfAbsmod1")
}   

# summary file of all spectras used in model 1
if(exists("dfAbsmod2")){
  rm("dfAbsmod2")
}   

# summary file of all spectras used in model 2
if(exists("dfAbsmod3")){
  rm("dfAbsmod3")
}   

# summary file of absorption results (part 2)
if(exists("dfabsSummary")){
  rm("dfabsSummary")
}

# summary file of all slopes 
if(exists("sSummary")){
  rm("sSummary")
}

#read in files and save as ind files with wl, absorbance and absorption



for(file in filelist){
  df<-read.csv(paste0(folderin, "/", file), header = T, sep=",",stringsAsFactors=F,comment.char ="", fill = TRUE)
  df<- as.data.table(df) 
  # get the name of the file
  currentname<-substr(file,1,nchar(file)-4) 
  cat(paste0("Now working on ",currentname,"\n"))
  
  abs<-df
  #fit model to estimate slope, k and a0
  
  fit<- cdom_fit_exponential(df$wl,df$absorption, wl0 = 443,startwl=400,endwl=550 ) # 6) fit a model marine data
  fit1<- cdom_fit_exponential(df$wl,df$absorption, wl0 = 320,startwl=275,endwl=295 ) # 6) fit a model freshwater data
  fit2<- cdom_fit_exponential(df$wl,df$absorption, wl0 = 320,startwl=350,endwl=400 ) # 6) fit a model freshwater data
  
  
  #resultslist<- list(fit_data, fit1_data, fit2_data)
  #resultslist<- list(mod=fit_data, mod1=fit1_data,  mod2=fit2_data)
  
  
  # extraxt row and absorption at specific nm and make into a df
  g443<- df[wl == 443] 
  g320<- df[wl == 320]
  
  cdom_abs<- g443 %>% select(absorption) %>%
    rename(absorption_443=absorption) %>%
    mutate(absorption_320=g320$absorption) %>%
    mutate(Observation=currentname)
  
 # resultslist<- list(fit, fit1, fit2)
  
  #save the observed and fitted and resid values of the model 1
    results<-as.data.frame(fit$data) # save results (fitted and residuals data from the model) as a data frame
    results <- as.data.table(results) #change to data table
    results <- dplyr::rename(results, absorption = y)
    results <- dplyr::rename(results, wl = x)
    results <- as.matrix(results) #removes extra attributes in name
    results <- as.data.frame(results) #change to data table
    results <- as.data.table(results) #change to data table
    
    params1<- as.data.frame(fit$params) #table of model parameter; i.e S, g443, slope in the model y=a0 +e(−S(x−λ0))+K
    results1<- results
    
    #Trying to loop thru the models- does not work with the list- ask Ciaran for help 
    # for (i in resultslist){
    #   name<- names(resultslist[i])
    #   name<- paste(currentname,name)
    #   cat(paste0("Now working on ",name))
    #   # results <-as.data.frame(i$data) # save results (fitted and residuals data from the model) as a data frame
    #   results <- as.data.table(i) #change to data table
    #   results <- dplyr::rename(results, absorption = y)
    #   results <- dplyr::rename(results, wl = x)
    #   results <- as.matrix(results) #removes extra attributes in name
    #   results <- as.data.frame(results) #change to data table
    #   results <- as.data.table(results) #change to data table
    
        #   for (i in  names(resultslist)){
        #   name<-substr(i,1, nchar(i)) 
        #   name<- paste(currentname,name)
        # }
        # 
     ## resultslist<- list(fit=fit, fit1= fit1, fit2= fit2)
      
     #name<- names(resultslist[i])
    #name<- paste(currentname,name)
      
      
  #   ########## plots- if the plot should be saved activate the script model 1
  #   
    # plot residuals
    plot(results$wl, results$.resid, pch=20, xlab= "Wavelength, nm",ylab= "Residuals" )
    abline(h=0, col="black", lwd= 2)
    dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","res_mod1_",currentname, ".jpeg"))

    # plot obs vs fitted plots
    plot(results$absorption, results$.fitted, pch=20, xaxs="i", yaxs="i", mgp=c(2.2,1,0),
         xlab= expression(paste("Absorption, m"^"-1")),ylab= expression(paste("Fitted, m"^"-1")))
    abline(0,1, lwd=1) #1:1 line
    dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","fitted_mod1_",currentname, ".jpeg"))

    # plot obs vs fitted ~ wl, nm
        plot(results$wl, results$absorption, pch=1, cex=0.75, xlab= "Wavelength, nm",
         ylab=expression(paste("Absorption, m"^"-1")))
    lines(results$wl, results$.fitted, col = "red")
    points(g443$wl, g443$absorption, pch=18, col="red", cex=1.5)
    points(g320$wl, g320$absorption, pch=18, col="black", cex=1.5)
    points(results$wl, results$.fitted, pch=20, cex=0.8)
    dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","spectra_mod1_",currentname,".jpeg"))



  #  save the observed and fitted and resid values of the model 2
    results<-as.data.frame(fit1$data) # save results (fitted and residuals data from the model) as a data frame
    results <- as.data.table(results) #change to data table
    results <- dplyr::rename(results, absorption = y)
    results <- dplyr::rename(results, wl = x)
    results <- as.matrix(results) #removes extra attributes in name
    results <- as.data.frame(results) #change to data table
    results <- as.data.table(results) #change to data table

    params2<- as.data.frame(fit1$params) #table of model parameter; i.e S, g443, slope in the model y=a0 +e(−S(x−λ0))+K
    results2<- results
  #   
  #   ########## plots- if the plot should be saved activate the script model 2
     # plot residuals
    plot(results$wl, results$.resid, pch=20, xlab= "Wavelength, nm",ylab= "Residuals" )
    abline(h=0, col="black", lwd= 2)
    dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","res_mod2_",currentname, ".jpeg"))

    # plot obs vs fitted plots
    plot(results$absorption, results$.fitted, pch=20, xaxs="i", yaxs="i", mgp=c(2.2,1,0),
         xlab= expression(paste("Absorption, m"^"-1")),ylab= expression(paste("Fitted, m"^"-1")))
    abline(0,1, lwd=1) #1:1 line
    dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","fitted_mod2_",currentname, ".jpeg"))

    # plot obs vs fitted ~ wl, nm
      plot(results$wl, results$absorption, pch=1, cex=0.75, xlab= "Wavelength, nm",
         ylab=expression(paste("Absorption, m"^"-1")))
    lines(results$wl, results$.fitted, col = "red")
    points(g443$wl, g443$absorption, pch=18, col="red", cex=1.5)
    points(g320$wl, g320$absorption, pch=18, col="black", cex=1.5)
    points(results$wl, results$.fitted, pch=20, cex=0.8)
      dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","spectra_mod2_",currentname,".jpeg"))

  #     
      #  save the observed and fitted and resid values of the model 3
      results<-as.data.frame(fit2$data) # save results (fitted and residuals data from the model) as a data frame
      results <- as.data.table(results) #change to data table
      results <- dplyr::rename(results, absorption = y)
      results <- dplyr::rename(results, wl = x)
      results <- as.matrix(results) #removes extra attributes in name
      results <- as.data.frame(results) #change to data table
      results <- as.data.table(results) #change to data table

      params3<- as.data.frame(fit2$params) #table of model parameter; i.e S, g443, slope in the model y=a0 +e(−S(x−λ0))+K
      results3<- results
  #
  #     ########## plots- if the plot should be saved activate the script model 3
  #
      # plot residuals
      plot(results$wl, results$.resid, pch=20, xlab= "Wavelength, nm",ylab= "Residuals" )
      abline(h=0, col="black", lwd= 2)
      dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","res_mod3_",currentname, ".jpeg"))

      # plot obs vs fitted plots
    plot(results$absorption, results$.fitted, pch=20, xaxs="i", yaxs="i", mgp=c(2.2,1,0),
         xlab= expression(paste("Absorption, m"^"-1")),ylab= expression(paste("Fitted, m"^"-1")))
    abline(0,1, lwd=1) #1:1 line
    dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","fitted_mod3_",currentname, ".jpeg"))

    # plot obs vs fitted ~ wl, nm
    plot(results$wl, results$absorption, pch=1, cex=0.75, xlab= "Wavelength, nm",
    ylab=expression(paste("Absorption, m"^"-1")))
    lines(results$wl, results$.fitted, col = "red")
    points(g443$wl, g443$absorption, pch=18, col="red", cex=1.5)
    points(g320$wl, g320$absorption, pch=18, col="black", cex=1.5)
    points(results$wl, results$.fitted, pch=20, cex=0.8)
        dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","spectra_mod3_",currentname,".jpeg"))
  #
  #   ############
  #   
    # add R2, K, S and a0
    r2 <- fit$r2
    r2.2 <- fit1$r2
    r2.3 <- fit2$r2
    
    S <- params1[1,2]
    S2 <- params2[1,2]
    S3 <- params3[1,2]
    
    a0 <- params1[3,2]
    a02 <- params2[3,2]
    a03 <- params3[3,2]
    
    K<- params1[2,2]
    K2<- params2[2,2]
    K3<- params3[2,2]
  
    
    
    sum<- cdom_abs %>% mutate(S_400_550=S, S_275_295=S2, S_350_400=S3, a0=a0, a02=a02, a03=a03, r2=r2, r2.2=r2.2, 
                              r2.3=r2.3, K=K, K2=K2, K3=K3)
 

  #create a summary file of the results
    if(exists("dfSummary")){
    dfSummary <- dfSummary %>%
      bind_rows(mutate(sum,Observation=currentname)) # %>%
      #bind_rows(mutate(g410,Observation=file,S_400_550=S, a0=a0, r2=r2, lake=lake, station=station)) %>%
      #bind_rows(mutate(g380,Observation=file,S_400_550=S, a0=a0,r2=r2, lake=lake, station=station))
    
  }else{
    dfSummary <- mutate(sum,Observation=currentname) # %>%
    # bind_rows(mutate(g410,Observation=file,S_400_550=S, a0=a0, r2=r2, lake=lake, station=station)) %>%
    # bind_rows(mutate(g380,Observation=file,S_400_550=S, a0=a0, r2=r2, lake=lake, station=station))
  }
  
  # nm <- c("Observation","station","wl","absorption","S_400_550","a0", "r2", "wl0",".fitted",".resid")
  # list colnames
  nm <- c("Observation","absorption_443", "absorption_320" ,"S_400_550","S_275_295" ,"S_350_400" ,
  "a0","a02","a03","r2","r2.2","r2.3","K","K2","K3")
  
  # order columns
  setcolorder(dfSummary, c(nm, setdiff(names(dfSummary), nm)))
  
  
  #create a file with all spectras
  # if(exists("dfAbsAll")){
  #   dfAbsAll <- dfAbsAll %>%
  #     bind_rows(mutate(results,Observation=file, lake=lake, station=station))
  # }else{
  #   dfAbsAll <- mutate(results,Observation=file, lake=lake, station=station)
  # }  
  
  # #create a file with all absorption spectras full range
  # if(exists("dfAbsAll")){
  #   dfAbsAll <- dfAbsAll %>%
  #     bind_rows(mutate(abs,Observation=file))
  # }else{
  #   dfAbsAll <- mutate(abs,Observation=file)
  #   
  # }  
  
  #create a file with all spectras and fitted values for model 1 with slope 400- 550 nm, wl0=443
  # if(exists("dfAbsmod1")){
  #   dfAbsmod1 <- dfAbsmod1 %>%
  #     bind_rows(mutate(results1,Observation=file))
  # }else{
  #   dfAbsmod1 <- mutate(results1,Observation=file)
  #   
  # }  
  #  nm <- c("Observation","wl","absorption", "wl0",".fitted",".resid")
  #  # order columns
  #  setcolorder(dfAbsmod1, c(nm, setdiff(names(dfAbsmod1), nm)))
  #  
  # #create a file with all spectras and fitted values for model 2 with slope 257-295 nm, wl0=320
  # if(exists("dfAbsmod2")){
  #   dfAbsmod2 <- dfAbsmod2 %>%
  #     bind_rows(mutate(results2,Observation=file))
  # }else{
  #   dfAbsmod2 <- mutate(results2,Observation=file)
  #   
  # }  
  #  nm <- c("Observation","wl","absorption", "wl0",".fitted",".resid")
  #  # order columns
  #  setcolorder(dfAbsmod2, c(nm, setdiff(names(dfAbsmod2), nm)))
  #  
  # #create a file with all spectras and fitted values for model 2 with slope 350-400nm, wl0=320
  # if(exists("dfAbsmod3")){
  #   dfAbsmod3 <- dfAbsmod3 %>%
  #     bind_rows(mutate(results3,Observation=file))
  # }else{
  #   dfAbsmod3 <- mutate(results3,Observation=file)
  #   
  # }  
  #  nm <- c("Observation","wl","absorption", "wl0",".fitted",".resid")
  #  # order columns
  #  setcolorder(dfAbsmod3, c(nm, setdiff(names(dfAbsmod3), nm)))

  
  # save results and tables- for individual files 
  
   # set the name of output file
   filenameout1 <- paste0(folderout_results,"fit1_",currentname,".csv")
   filenameout2 <- paste0(folderout_results,"fit2_",currentname,".csv")
   filenameout3 <- paste0(folderout_results,"fit3_",currentname,".csv")
   filenameout4 <- paste0(folderout_results,"params1_",currentname,".csv")
   filenameout5 <- paste0(folderout_results,"params2_",currentname,".csv")
   filenameout6 <- paste0(folderout_results,"params3_",currentname,".csv")
   filenameout7 <- paste0(folderout_results,"cdom_sum_",currentname,".csv")
   
   
   df<- as.data.table(df) 
   
   # write the output file
   write.table(results1,file=filenameout1,col.names=T,row.names=F,sep=",",quote=F) 
   write.table(results2,file=filenameout2,col.names=T,row.names=F,sep=",",quote=F) 
   write.table(results3,file=filenameout3,col.names=T,row.names=F,sep=",",quote=F) 
   write.table(params1,file=filenameout4,col.names=T,row.names=F,sep=",",quote=F) 
   write.table(params2,file=filenameout5,col.names=T,row.names=F,sep=",",quote=F) 
   write.table(params3,file=filenameout6,col.names=T,row.names=F,sep=",",quote=F) 
   write.table(sum,file=filenameout7,col.names=T,row.names=F,sep=",",quote=F) 
   
}

#merge with logbook data 

#dfAbsAll<- left_join(dfAbsAll, infotable, by="Observation")
dfSummary2<- left_join(dfSummary, logbook, by="Observation")


#save summary files
write.table(dfSummary2,file=paste0(folderout_results,"/","results_all_spectras_210429.csv"),sep=",",row.names=F,quote=F)
#write.table(dfAbsAll,file=paste0(folderout,"/","spectra_all.txt"),sep=",",row.names=F,quote=F)
