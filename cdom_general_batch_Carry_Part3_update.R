# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Processing script for CDOM files (Cary 300 spectrophotometer)
# Part1- current script= cdom_general_batch_Carry_Part1.R
#        Reads raw data from Cary csv files, saves each observation as a separate file, removes blanks 
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

#dir()
#getwd()

folderin<-"lingning_etc/processed/" 
folderout_results<- "lingning_etc/results/"
folderout_plots<- "lingning_etc/plots/models/"

#Add logbook (check for latest version on K)
logbook <- read.delim ("log_cary_2104.txt")
logbook$Sampling_date<- as.character(logbook$Sampling_date)
logbook$Sampling_date <- as.Date(logbook$Sampling_date, "%d/%m/%Y")

#read in full loogbook 
logbook <- logbook %>%
  rename(Date = Sampling_date,Observation=filename) 


# read in 1 test file
#df<-read.csv("varflom2021/processed/Glomma_20200618.csv", header = T, sep=",",stringsAsFactors=F,comment.char ="")

#currentname<- "Glomma_20200618"

#list files that containg the relevant stations based on the logbook
filelist<-list.files(path=folderin,pattern="*.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#calculate absorption from absorbance with correct cuvette length

#aCDOM(λ)= 2.3020585*absorbance(λ)/0.1

# What about correction of the blank- not needed for 300 Cary data! 

# 6) fit an exponential model between different slopes (can be changed accordingly) with 443 (or another) nm as ref wl
# 7) save the results of absorption, slopes and model parameters as a tables, 8) saves the plots of the model and the absorption spectra the model residuals and fitted vs obs values and
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#Deletes the summary datafiles if they exists

list_dfs <- c("dfSummary","dfAbsAll","dfAbsmod1","dfAbsmod2","dfAbsmod3","dfabsSummary","sSummary", "paramSummary")
# summary file of results absorbance, absorption, slope values, etc
for(dfname in list_dfs){
  if(exists(dfname)){
    rm(list=dfname)
  } 
}


#read in files and save as ind files with wl, absorbance and absorption

for(file in filelist){
  df<-read.csv(paste0(folderin, "/", file), header = T, sep=",",stringsAsFactors=F,comment.char ="", fill = TRUE)

  # get the name of the file
  currentname<-substr(file,1,nchar(file)-4) 
  cat(paste0("Now working on ",currentname,"\n"))
  df<- as.data.table(df) 
  
  abs<-df
  #fit model to estimate slope, k and a0
  df<- as.data.table(df) 
  
  mod1<- cdom_fit_exponential(df$wl,df$absorption, wl0 = 443,startwl=400,endwl=550 ) # 6) fit a model marine data
  mod2<- cdom_fit_exponential(df$wl,df$absorption, wl0 = 320,startwl=275,endwl=295 ) # 6) fit a model freshwater data
  mod3<- cdom_fit_exponential(df$wl,df$absorption, wl0 = 320,startwl=350,endwl=400 ) # 6) fit a model freshwater data
  
  
  # extraxt row and absorption at specific nm and make into a df
  g250<- df[wl == 250] 
  g254<- df[wl == 254] 
  g320<- df[wl == 320] 
  g365<- df[wl == 365] 
  g375<- df[wl == 375]
  g380<- df[wl == 380]
  g410<- df[wl == 410]
  g443<- df[wl == 443] 

# merge to 1 df
 cdom_abs<- g250 %>% select(absorption) %>%
    rename(a_250=absorption) %>%
    mutate(a_254=g254$absorption,a_320=g320$absorption, a_365=g365$absorption,
           a_375=g375$absorption,a_380=g380$absorption,a_410=g410$absorption, a_443=g443$absorption) %>%
    mutate(Observation=currentname)

#remove df with abs 
 list_abs <- c("g250","g254","g365","g375","g380","g410")
 # summary file of results absorbance, absorption, slope values, etc
 for(abs in list_abs){
   if(exists(abs)){
     rm(list=abs)
   } 
 }
  
 resultslist<- list(mod1=mod1, mod2=mod2, mod3=mod3)
 
   # resultslist<- list(fit=fit, fit1=fit1, fit2=fit2)
 if(exists("paramSummary")){
   rm("paramSummary")
 }
    # loop thru the models
   # remove parameter summary file
     for (i in 1:length(resultslist)){
       
       namemod<- names(resultslist)[[i]]
       name<- paste0(namemod,currentname, collapse = " ")
       cat(paste0("Now working on ",name))
       
       results <-as.data.frame(resultslist[[i]]$data) # save results (fitted and residuals data from the model) as a data frame
       results <- as.data.table(resultslist[[i]]$data) #change to data table
       results <- dplyr::rename(results, absorption = y)
       results <- dplyr::rename(results, wl = x)
       results <- as.matrix(results) #removes extra attributes in name
       results <- as.data.frame(results) #change to data table
       results <- as.data.table(results) #change to data table

       params<- as.data.frame(resultslist[[i]]$params)
      
       r2_mod1 <- mod1$r2
       r2_mod2 <- mod2$r2
       r2_mod3 <- mod3$r2
       r2_mod1<- as.data.frame(r2_mod1)
       r<- mutate(r2_mod1,  r2_mod2=r2_mod2, r2_mod3=r2_mod3)
       
       S <- params[1,2]
       a0 <- params[3,2]
       K<- params[2,2]

       
       #sumfile of params for each model
       #create a summary file of the results
       if(exists("paramSummary")){
         paramSummary <- paramSummary %>%
           bind_rows(mutate(r, S=S, a0=a0,K=K,
                            Observation=currentname, model=namemod)) # %>%
      }else{
         paramSummary <- mutate(r,S=S, a0=a0,K=K,
                                Observation=currentname, model=namemod) 
       }
       
       #rename col from each row with mod extention and bind to a df with 1 row
       param1 <-paramSummary %>% slice(1) %>%
         plyr::rename(c("S"="S_mod1", "a0"="a0_mod1","K"="K_mod1")) %>%
         select(-model)
       param2 <-paramSummary %>% slice(2) %>%
         plyr::rename(c("S"="S_mod2", "a0"="a0_mod2","K"="K_mod2"))%>%
         select(-model, -r2_mod1, -r2_mod2, -r2_mod3)
       param3<-paramSummary %>% slice(3) %>%
         plyr::rename(c("S"="S_mod3", "a0"="a0_mod3","K"="K_mod3"))%>%
         select(-model, -r2_mod1, -r2_mod2, -r2_mod3)
       param_res<- left_join(param1, param2, by="Observation")
       param_res<- left_join(param_res, param3, by="Observation")
       

      
  #   ########## plots- if the plot should be saved activate the script model 1
  #   
    # plot residuals
    plot(results$wl, results$.resid, pch=20, xlab= "Wavelength, nm",ylab= "Residuals" )
    abline(h=0, col="black", lwd= 2)
    dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","res_",name, ".jpeg"))

    # plot obs vs fitted plots
    plot(results$absorption, results$.fitted, pch=20, xaxs="i", yaxs="i", mgp=c(2.2,1,0),
         xlab= expression(paste("Absorption, m"^"-1")),ylab= expression(paste("Fitted, m"^"-1")))
    abline(0,1, lwd=1) #1:1 line
    dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","fitted_",name, ".jpeg"))

    # plot obs vs fitted ~ wl, nm
        plot(results$wl, results$absorption, pch=1, cex=0.75, xlab= "Wavelength, nm",
         ylab=expression(paste("Absorption, m"^"-1")))
    lines(results$wl, results$.fitted, col = "red")
    points(g443$wl, g443$absorption, pch=18, col="red", cex=1.5)
    points(g320$wl, g320$absorption, pch=18, col="black", cex=1.5)
    points(results$wl, results$.fitted, pch=20, cex=0.8)
    dev.print(jpeg,width=853, height=626, file=paste0(folderout_plots,"/","spectra_",name,".jpeg"))

    # set the name of output file
    filenameout <- paste0(folderout_results, name,".csv")
    filenameout_params <- paste0(folderout_results, "params_ ", name,".csv")
    
 
    # write the output file
    write.table(results,file=filenameout,col.names=T,row.names=F,sep=",",quote=F) 
    write.table(params,file=filenameout_params,col.names=T,row.names=F,sep=",",quote=F) 
    
    
     }

  #   ############
  #   merge absorbption values with slope and model parameters
        sum<-left_join(cdom_abs, param_res, by= "Observation" ) 
 
 # list colnames
  nm <- c("Observation", "a_250","a_254","a_320","a_365","a_375","a_380","a_410", "a_443",
         "S_mod1", "a0_mod1","K_mod1", "S_mod2", "a0_mod2","K_mod2", "S_mod3", "a0_mod3","K_mod3")
 
 # order columns
 setcolorder(sum, c(nm, setdiff(names(sum), nm)))   

    if(exists("dfSummary")){
    dfSummary <- dfSummary %>%
      bind_rows(mutate(sum,Observation=currentname)) # %>%
       }else{
    dfSummary <- mutate(sum,Observation=currentname) # %>%
  }

   filenameout_sum <- paste0(folderout_results,"cdom_sum_",currentname,".csv")

   write.table(sum,file=filenameout_sum,col.names=T,row.names=F,sep=",",quote=F) 
   
}

#merge with logbook data 

dfSummary2<- left_join(dfSummary, logbook, by="Observation")


#save summary files
write.table(dfSummary2,file=paste0(folderout_results,"/","results_all_spectras_210505.csv"),sep=",",row.names=F,quote=F)
