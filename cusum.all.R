cusum.all <- function(files, d){
    # read each file, 
    fileone <- read.csv(files[1])
    d.all <- data.frame()
    
    # fit curve, take residuals, calculate cumsum
    for(i in 1:(ncol(fileone)-2)){ # number of cells
        for(j in 1:(length(files)-1)){ # number of cycles
            f <- read.csv(files[j])
            
            # subset one column of data for model fitting
            thesub <- f[,c(2,i+2)]
            names(thesub) <- c('time','signal')
            
            # fit one phase decay curve with self-start
            d_model <- try(nls(signal ~ SSasymp(time, Asym, r0, lrc), data = thesub), silent = TRUE)
            
            # if not fitted, fit linear model
            if(class(d_model) == 'nls'){
                d[,j+1] <- resid(d_model)
            } else {
                d_model <- lm(signal ~ time, data = thesub)
                d[,j+1] <- resid(d_model)
            }
            
            # substract baseline from all data points
            avg <- mean(d[d$time < 10, j+1])
            d[,j+1] <- (d[,j+1] - avg)
            
            # calculate cumsum and z scores
            d[,j+1] <- cumsum(d[,j+1])
            #std <- sd(d[d$time <= 10, j+1])
            #avg <- mean(d[d$time <= 10, j+1])
            #d[,j+1] <- (d[,j+1] - avg)/std
            d.temp <- mutate(d, cell = i)
        }
        d.all <- rbind(d.all, d.temp)
    }
 
    # output
    d.all
}