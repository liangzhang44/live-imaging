# calculate z scores for every 1 sec from 5 sec to 55 sec for training and from 5 sec to 45 sec for tests
dF.all.z.tone.1sec.cusum <- function(folder){
    library(tidyverse)
    source('dF.training.R')
    source('dF.test.R')
    source("z.dF.tone.1sec.cusum2.R")
    
    # load one experiment folder, list all csv folders
    folders <- list.files(folder)
    folders <- folders[!is.na(as.numeric(folders))]
    
    # list csv files in the training folder, set up data frames
    files <- list.files(paste0(folder,"/", folders[1]), full.names = TRUE)
    dat <- data.frame(time=round(seq(0, 64.94, length.out = 763),2),cycle1=NA,cycle2=NA,
                      cycle3=NA,cycle4=NA,cycle5=NA,cycle6=NA,cycle7=NA)
    dat_resid <- data.frame(time=round(seq(0, 64.94, length.out = 763),2),
                            cell1=NA,cell2=NA,cell3=NA,cell4=NA,cell5=NA,cell6=NA,
                            cell7=NA,cell8=NA,cell9=NA,cell10=NA,cell11=NA,cell12=NA)
    
    # calculate Z scores for training data
    temp <- dF.training(files, dat, dat_resid)
    temp <- z.dF.tone.1sec.cusum2(temp)
    
    # save file
    write_csv(temp, file.path(folder, "z tone 1sec cusum training.csv"))
    
    # list csv files in the test folders, set up data frames
    for(i in 2:length(folders)){
        files <- list.files(paste0(folder, "/", folders[i]), full.names = TRUE)
        dat1 <- data.frame(time=round(seq(0, 46.96, length.out = 552),2),cycle1=NA,cycle2=NA,
                           cycle3=NA,cycle4=NA,cycle5=NA)
        dat_resid1 <- data.frame(time=round(seq(0, 46.96, length.out = 552),2),
                                 cell1=NA,cell2=NA,cell3=NA,cell4=NA,cell5=NA,cell6=NA,
                                 cell7=NA,cell8=NA,cell9=NA,cell10=NA,cell11=NA,cell12=NA)
        # calcualte Z scores for test data
        temp <- dF.test(files, dat1, dat_resid1)
        temp <- z.dF.tone.1sec.cusum2(temp)
        
        # save file
        write_csv(temp, file.path(folder, paste0("z tone 1sec cusum test", i-1, ".csv")))
    }
    
}