# calculate z scores for entire datasets
dF.z <- function(folder){
    library(tidyverse)
    source('dF.training.R')
    source('dF.test.R')
    source("zscore.R")
    
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
    
    # calculate Z scores for training data, all sessions
    temp <- dF.training(files, dat, dat_resid)
    all <- zscore(temp$all)
    write_csv(all, file.path(folder, "z training all.csv"))
    
    # calculate Z scores for training data, first 2 sessions
    early2 <- zscore(temp$early2)
    write_csv(early2, file.path(folder, "z training early2.csv"))
    
    # calculate Z scores for training data, last 2 sessions
    late2 <- zscore(temp$late2)
    write_csv(late2, file.path(folder, "z training late2.csv"))
    
    # list csv files in the test folders, set up data frames
    for(i in 2:length(folders)){
        files <- list.files(paste0(folder, "/", folders[i]), full.names = TRUE)
        dat1 <- data.frame(time=round(seq(0, 46.96, length.out = 552),2),cycle1=NA,cycle2=NA,
                           cycle3=NA,cycle4=NA,cycle5=NA)
        dat_resid1 <- data.frame(time=round(seq(0, 46.96, length.out = 552),2),
                                 cell1=NA,cell2=NA,cell3=NA,cell4=NA,cell5=NA,cell6=NA,
                                 cell7=NA,cell8=NA,cell9=NA,cell10=NA,cell11=NA,cell12=NA)
        
        # calcualte Z scores for test data, all trials
        temp <- dF.test(files, dat1, dat_resid1)
        all <- zscore(temp$all)
        write_csv(all, file.path(folder, paste0("z test", i-1, " all.csv")))
        
        # calculate Z scores for test data, trial 1
        early2 <- zscore(temp$trial1)
        write_csv(early2, file.path(folder, paste0("z test", i-1, " early2.csv")))
        
        # calculate Z scores for test data, last 2 cycles
        late2 <- zscore(temp$late2)
        write_csv(late2, file.path(folder, paste0("z test", i-1, " late2.csv")))
        
        # calculate Z scores for test data, first 3 cycles
        early3 <- zscore(temp$early3)
        write_csv(early3, file.path(folder, paste0("z test", i-1, " early3.csv")))
        
        # calculate Z scores for test data, last 2 cycles
        late3 <- zscore(temp$late3)
        write_csv(late3, file.path(folder, paste0("z test", i-1, " late3.csv")))
    }
    
}