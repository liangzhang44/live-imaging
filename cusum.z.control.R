cusum.z.control <- function(folder){
    library(tidyverse)
    source('E:/Live imaging/cusum.control.R')
    
    # load one experiment folder, list all csv folders
    folders <- list.files(folder)
    folders <- folders[!is.na(as.numeric(folders))]
    
    # list all csv files in the training folder, set up data frames
    files <- list.files(paste0(folder,"/", folders[1]), full.names = TRUE)
    fileone <- read.csv(files[1])
    dat <- data.frame(time=fileone[,2],cycle1=NA,cycle2=NA,cycle3=NA,cycle4=NA,
                      cycle5=NA,cycle6=NA,cycle7=NA, cycle8=NA)
    z_training <- data.frame(cell=1:(ncol(fileone)-2), cycle1=NA, cycle2=NA, 
                             cycle3=NA, cycle4=NA, cycle5=NA, 
                             cycle6=NA, cycle7=NA, cycle8=NA)
    
    # calculate z score of each cycle for training data
    temp <- cusum.control(files, dat, z_training)
    write_csv(temp, file.path(folder, "z training cycles.csv"))
    
    # list csv files in testing folders, set up data frames
    for(i in 2:length(folders)){
        files <- list.files(paste0(folder, "/", folders[i]), full.names = TRUE)
        fileone <- read.csv(files[1])
        dat1 <- data.frame(time=fileone[,2],cycle1=NA,cycle2=NA,
                           cycle3=NA,cycle4=NA,cycle5=NA,cycle6=NA)
        z_test <- data.frame(cell=1:(ncol(fileone)-2), cycle1=NA, cycle2=NA, 
                                 cycle3=NA, cycle4=NA, cycle5=NA, cycle6=NA)
        # calcualte z score of each cycle for testing data
        temp <- cusum.control(files, dat1, z_test)
        write_csv(temp, file.path(folder, paste0("z test", i-1, " cycles.csv")))
    }
}