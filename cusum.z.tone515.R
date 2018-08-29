cusum.z.tone515 <- function(folder){
    library(tidyverse)
    source('cusum515.R')
    source('z.score.tone.R')
    source("cusumplot.tone.R")
    
    # load one experiment folder, list all csv folders
    folders <- list.files(folder)
    folders <- folders[!is.na(as.numeric(folders))]
    
    # list all csv files in the training folder, set up data frames
    files <- list.files(paste0(folder,"/", folders[1]), full.names = TRUE)
    fileone <- read.csv(files[1])
    dat <- data.frame(time=fileone[,2],cycle1=NA,cycle2=NA,
                      cycle3=NA,cycle4=NA,cycle5=NA,cycle6=NA,cycle7=NA)
    dat_cusum <- data.frame(time=fileone[fileone[,2]>=5 & fileone[,2]<=15, 2],
                            cell1=NA,cell2=NA,cell3=NA,cell4=NA,cell5=NA,cell6=NA,
                            cell7=NA,cell8=NA,cell9=NA,cell10=NA,cell11=NA,cell12=NA)
    
    # calculate cusum for training data
    temp <- cusum515(files, dat, dat_cusum)
    dat_cusum <- temp$cusum
    mn <- temp$mn
    avg <- temp$avg
    
    # calculate z score for each cell
    all_z <- data.frame(cell=1:(ncol(dat_cusum)-1), training=NA, test1=NA, test2=NA, test3=NA, test4=NA)
    all_z[,2] <- z.score.tone(dat_cusum, mn, avg)
    
    # plot cusum
    cusumplot.tone(dat_cusum, mn, avg)
    ggsave(filename = paste0("cusum515-", folders[1], ".png"), path = folder)
    
    # list csv files in testing folders, set up data frames
    
    for(i in 2:length(folders)){
        files <- list.files(paste0(folder, "/", folders[i]), full.names = TRUE)
        fileone <- read.csv(files[1])
        dat1 <- data.frame(time=fileone[,2],cycle1=NA,cycle2=NA,
                       cycle3=NA,cycle4=NA,cycle5=NA)
        dat_cusum1 <- data.frame(time=fileone[fileone[,2]>=5 & fileone[,2]<=15, 2],
                                 cell1=NA,cell2=NA,cell3=NA,cell4=NA,cell5=NA,cell6=NA,
                             cell7=NA,cell8=NA,cell9=NA,cell10=NA,cell11=NA,cell12=NA)
        # calcualte cusum for testing data
        temp <- cusum515(files, dat1, dat_cusum1)
        dat_cusum1 <- temp$cusum
        mn <- temp$mn
        avg <- temp$avg
        
        # calculate z score for each cell
        all_z[,i+1] <- z.score.tone(dat_cusum1, mn, avg)
        
        # plot cusum
        cusumplot.tone(dat_cusum1, mn, avg)
        ggsave(filename = paste0("cusum515-", folders[i], ".png"), path = folder)
    }
    
    write_csv(all_z, file.path(folder, "z score 515.csv"))
}