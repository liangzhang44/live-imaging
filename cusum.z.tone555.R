# calculate z scores from 5 sec to 55 sec for training and from 5 sec to 45 sec for tests
cusum.z.tone555 <- function(folder){
    library(tidyverse)
    source('resid.all.cycle.R')
    source('z.tone555.R')
    
    # load one experiment folder, list all csv folders
    folders <- list.files(folder)
    folders <- folders[!is.na(as.numeric(folders))]
    
    # list all csv files in the training folder, set up data frames
    files <- list.files(paste0(folder,"/", folders[1]), full.names = TRUE)
    fileone <- read.csv(files[1])
    dat <- data.frame(time=fileone[,2],cycle1=NA,cycle2=NA,
                      cycle3=NA,cycle4=NA,cycle5=NA,cycle6=NA,cycle7=NA)
    dat_resid <- data.frame(time=fileone[, 2],
                            cell1=NA,cell2=NA,cell3=NA,cell4=NA,cell5=NA,cell6=NA,
                            cell7=NA,cell8=NA,cell9=NA,cell10=NA,cell11=NA,cell12=NA)
    
    # calculate cusum for training data
    temp <- z.tone555(resid.all.cycle(files, dat, dat_resid))
    
    # save file
    write_csv(temp, file.path(folder, "cusum555 training.csv"))
    
    # list csv files in testing folders, set up data frames
    
    for(i in 2:length(folders)){
        files <- list.files(paste0(folder, "/", folders[i]), full.names = TRUE)
        fileone <- read.csv(files[1])
        dat1 <- data.frame(time=fileone[,2],cycle1=NA,cycle2=NA,
                           cycle3=NA,cycle4=NA,cycle5=NA)
        dat_resid1 <- data.frame(time=fileone[, 2],
                                 cell1=NA,cell2=NA,cell3=NA,cell4=NA,cell5=NA,cell6=NA,
                                 cell7=NA,cell8=NA,cell9=NA,cell10=NA,cell11=NA,cell12=NA)
        # calcualte cusum for testing data
        temp <- z.tone555(resid.all.cycle(files, dat1, dat_resid1))
        
        # save file
        write_csv(temp, file.path(folder, paste0("cusum545 test", i-1, ".csv")))
    }
    
}