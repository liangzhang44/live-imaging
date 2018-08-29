# calculate cusum of entire cycle for each cycle, each cell
cusum.all.phase <- function(folder){
    library(tidyverse)
    source('cusum.all.R')
    
    # load one experiment folder, list all csv folders
    folders <- list.files(folder)
    folders <- folders[!is.na(as.numeric(folders))]
    
    # list all csv files in the training folder, set up data frames
    files <- list.files(paste0(folder,"/", folders[1]), full.names = TRUE)
    dat <- data.frame(time=round(seq(0, 64.94, length.out = 763),2),cycle1=NA,cycle2=NA,
                      cycle3=NA,cycle4=NA,cycle5=NA,cycle6=NA,cycle7=NA)
    
    # calculate cusum for training data
    temp <- cusum.all(files, dat)
    
    # save file
    write_csv(temp, file.path(folder, "cusum training.csv"))
    
    # list csv files in testing folders, set up data frames
    for(i in 2:length(folders)){
        files <- list.files(paste0(folder, "/", folders[i]), full.names = TRUE)
        dat1 <- data.frame(time=round(seq(0, 46.96, length.out = 552),2),cycle1=NA,cycle2=NA,
                           cycle3=NA,cycle4=NA,cycle5=NA)
        # calcualte cusum for testing data
        temp <- cusum.all(files, dat1)
        
        # save file
        write_csv(temp, file.path(folder, paste0("cusum test", i-1, ".csv")))
    }
    
}