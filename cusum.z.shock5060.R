cusum.z.shock5060 <- function(folder){
    library(tidyverse)
    source('E:/Live imaging/cusum5060.R')
    source('E:/Live imaging/z.score.shock.R')
    source("E:/Live imaging/cusumplot.shock.R")
    
    # load one experiment folder, list all csv folders
    folders <- list.files(folder)
    folders <- folders[!is.na(as.numeric(folders))]
    
    # list all csv files in the training folder, set up data frames
    files <- list.files(paste0(folder,"/", folders[1]), full.names = TRUE)
    fileone <- read.csv(files[1])
    dat <- data.frame(time=fileone[,2],cycle1=NA,cycle2=NA,
                      cycle3=NA,cycle4=NA,cycle5=NA,cycle6=NA,cycle7=NA)
    dat_cusum <- data.frame(time=fileone[fileone[,2]>=50 & fileone[,2]<=60, 2],
                            cell1=NA,cell2=NA,cell3=NA,cell4=NA,cell5=NA,cell6=NA,
                            cell7=NA,cell8=NA,cell9=NA,cell10=NA,cell11=NA,cell12=NA)
    
    # calculate cusum for training data
    temp <- cusum5060(files, dat, dat_cusum)
    dat_cusum <- temp$cusum
    mn <- temp$mn
    avg <- temp$avg
    
    # calculate z score for each cell
    all_z <- data.frame(cell=1:(ncol(dat_cusum)-1), shock=NA)
    all_z[,2] <- z.score.shock(dat_cusum, mn, avg)
    
    # plot cusum
    cusumplot.shock(dat_cusum, mn, avg)
    ggsave(filename = paste0("cusum5060-", folders[1], ".png"), path = folder)
    write_csv(all_z, file.path(folder, "z score 5060.csv"))
}