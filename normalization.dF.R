library(zoo)

normalization.dF <- function(filename) {
    allfolders <- list.files()
    allfolders <- allfolders[grep("extinction", allfolders)]
    
    # read z score data of dat1
    dat <- data.frame()
    for(i in 1:length(allfolders)){
        temp <- read.csv(paste0("./", allfolders[i], "/", filename))
        temp <- mutate(temp, cell = cell + 100*i)
        dat <- rbind(dat, temp)
    }
    
    # calculate baseline mean and std
    dat.mean <- dat %>%
        group_by(time) %>%
        summarise(dF = mean(dF))
    mean1 <- mean(dat.mean$dF[dat.mean$time < 10])
    sd1 <- sd(dat.mean$dF[dat.mean$time < 10])
    dat <- mutate(dat, z = (dF - mean1)/sd1, dF = NULL)
    
    #output
    dat
}