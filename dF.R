# calculate dF/F and z scores for entire datasets
dF <- function(folder){
    library(tidyverse)
    source('dF.training.R')
    source('dF.test.R')
    source("zscore.R")
    
    # load one experiment folder, list all csv folders
    folders <- list.files(folder)
    folders <- folders[!is.na(as.numeric(folders))]
    
    # list  csv files in the training folder, set up data frames
    files <- list.files(paste0(folder,"/", folders[1]), full.names = TRUE)
    d <- data.frame(time = round(seq(0, 64.94, length.out = 763),2),
                    session1 = NA, session2 = NA, session3 = NA, session4 = NA,
                    session5 = NA, session6 = NA, session7 = NA, session8 = NA)
    d_resid <- data.frame(time = round(seq(0, 64.94, length.out = 763), 2),
                            cell1 = NA, cell2 = NA, cell3 = NA, cell4 = NA, 
                          cell5 = NA, cell6 = NA, cell7 = NA, cell8 = NA, 
                          cell9 = NA, cell10 = NA, cell11 = NA, cell12 = NA)
    
    # calculate dF/F for training data
    temp <- dF.training(files, d, d_resid)
    
    # calculate z scores for all sessions
    for (i in 1:length(temp)) {
        write_csv(temp[[i]], 
                  file.path(folder, paste0("dF training ", names(temp[i]), ".csv")))
        print(paste0(folder, "/dF training ", names(temp[i]), ".csv SAVED!"))
        write_csv(zscore(temp[[i]]), 
                  file.path(folder, paste0("z training ", names(temp[i]), ".csv")))
        print(paste0(folder, "/z training ", names(temp[i]), ".csv SAVED!"))
    }
    
    # list csv files in the test folders, set up data frames
    for(i in 2:length(folders)){
        files <- list.files(paste0(folder, "/", folders[i]), full.names = TRUE)
        d1 <- data.frame(time = round(seq(0, 46.96, length.out = 552), 2),
                         trial1 = NA, trial2 = NA, trial3 = NA, 
                         trial4 = NA, trial5 = NA, trial6 = NA)
        d_resid1 <- data.frame(time=round(seq(0, 46.96, length.out = 552), 2),
                                 cell1 = NA, cell2 = NA, cell3 = NA, cell4 = NA,
                               cell5 = NA, cell6 = NA, cell7 = NA, cell8 = NA,
                               cell9 = NA, cell10 = NA, cell11 = NA, cell12 = NA)
        # calcualte dF/F for test data
        temp <- dF.test(files, d1, d_resid1)
        
        # calculate z scores
        for (j in 1:length(temp)) {
            write_csv(temp[[j]], 
                      file.path(folder, paste0("dF test", i-1, " ", names(temp[j]),".csv")))
            print(paste0(folder, "/dF test", i-1, " ", names(temp[j]), ".csv SAVED!"))
            write_csv(zscore(temp[[j]]), 
                      file.path(folder, paste0("z test", i-1, " ", names(temp[j]),".csv")))
            print(paste0(folder, "/z test", i-1, " ", names(temp[j]), ".csv SAVED!"))
        }
    }
}