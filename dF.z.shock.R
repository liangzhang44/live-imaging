# calculate z scores for 5 sec before and after shock for training
dF.z.shock <- function(folder){
    library(tidyverse)
    source('dF.shock.R')
    source("z.dF.shock.R")
    
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
    temp <- dF.shock(files, dat, dat_resid)
    temp <- z.dF.shock(temp)
    
    # save file
    write_csv(temp, file.path(folder, "z shock.csv"))
}