cusum515 <- function(files, d, d_cusum){
    # read each file, fit curve, take residuals, put into d_cusum
    fileone <- read.csv(files[1])
    bl <- rep(0, ncol(fileone)-2)
    for(i in 1:(ncol(fileone)-2)){
        for(j in 1:length(files)){
            f <- read.csv(files[j])
            
            # subset one column of data for model fitting
            thesub <- f[,c(2,i+2)]
            names(thesub) <- c('time','signal')
            
            # fit one phase decay curve with self-start
            d_model <- try(nls(signal ~ SSasymp(time, Asym, r0, lrc), data = thesub), silent = TRUE)
            
            # discard data if not fitted
            if(class(d_model) == 'nls'){
                d[,j+1] <- resid(d_model)
            } else {
                d[,j+1] <- NA
            }
        }
        
        # subset from 5 to 15 sec, calculate row means
        dat_temp <- filter(d, time>=5, time<=15)
        dat_temp <- mutate(dat_temp, mean=rowMeans(dat_temp[,-1], na.rm=TRUE))
        
        # calculate baseline and variance of baseline
        baseline <- mean(dat_temp$mean[dat_temp$time >= 5 & dat_temp$time <= 10])
        bl[i] <- baseline
        
        # substract baseline from each datapoints
        dat_temp$a_b <- dat_temp$mean-baseline
        
        # calculate CuSum
        dat_temp$cusum <- cumsum(dat_temp$a_b)
        
        # put CuSum into the cusum data frame
        d_cusum[,i+1] <- dat_temp$cusum
    }
    # output
    list(cusum = d_cusum, mn = bl)
}