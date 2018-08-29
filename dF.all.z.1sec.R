# calculate z scores for every 1 sec from 0 to 60 sec for training and from 0 to 45 sec for tests
dF.all.z.1sec <- function(folder){
    library(tidyverse)
    source('dF.training.cycles.R')
    source('dF.test.cycles.R')
    source("z.dF.1sec.R")
    
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
    
    # calculate Z scores for training data, all cycles
    temp <- dF.training.cycles(files, dat, dat_resid)
    all <- z.dF.1sec(temp$all)
    write_csv(all, file.path(folder, "z 1sec training all.csv"))
    
    # calculate Z scores for training data, first 2 cycles
    early2 <- z.dF.1sec(temp$early2)
    write_csv(early2, file.path(folder, "z 1sec training early2.csv"))
    
    # calculate Z scores for training data, last 2 cycles
    late2 <- z.dF.1sec(temp$late2)
    write_csv(late2, file.path(folder, "z 1sec training late2.csv"))
    
    # calculate Z scores for training data, first 3 cycles
    early3 <- z.dF.1sec(temp$early3)
    write_csv(early3, file.path(folder, "z 1sec training early3.csv"))
    
    # calculate Z scores for training data, last 3 cycles
    late3 <- z.dF.1sec(temp$late3)
    write_csv(late3, file.path(folder, "z 1sec training late3.csv"))
    
    # list csv files in the test folders, set up data frames
    for(i in 2:length(folders)){
        files <- list.files(paste0(folder, "/", folders[i]), full.names = TRUE)
        dat1 <- data.frame(time=round(seq(0, 46.96, length.out = 552),2),cycle1=NA,cycle2=NA,
                           cycle3=NA,cycle4=NA,cycle5=NA)
        dat_resid1 <- data.frame(time=round(seq(0, 46.96, length.out = 552),2),
                                 cell1=NA,cell2=NA,cell3=NA,cell4=NA,cell5=NA,cell6=NA,
                                 cell7=NA,cell8=NA,cell9=NA,cell10=NA,cell11=NA,cell12=NA)
        # calcualte Z scores for test data, all cycles
        temp <- dF.test.cycles(files, dat1, dat_resid1)
        all <- z.dF.1sec(temp$all)
        write_csv(all, file.path(folder, paste0("z 1sec test", i-1, " all.csv")))
        
        # calculate Z scores for test data, first 2 cycles
        early2 <- z.dF.1sec(temp$early2)
        write_csv(early2, file.path(folder, paste0("z 1sec test", i-1, " early2.csv")))
        
        # calculate Z scores for test data, last 2 cycles
        late2 <- z.dF.1sec(temp$late2)
        write_csv(late2, file.path(folder, paste0("z 1sec test", i-1, " late2.csv")))
        
        # calculate Z scores for test data, first 3 cycles
        early3 <- z.dF.1sec(temp$early3)
        write_csv(early3, file.path(folder, paste0("z 1sec test", i-1, " early3.csv")))
        
        # calculate Z scores for test data, last 2 cycles
        late3 <- z.dF.1sec(temp$late3)
        write_csv(late3, file.path(folder, paste0("z 1sec test", i-1, " late3.csv")))
    }
    
}