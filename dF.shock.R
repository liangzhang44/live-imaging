# 1. fit curve based on first 10 sec and last 5 sec
# 2. calculate residuals (dF) and plateaus (F)
# 3. calculate mean of all cycles (row means)
# 4. calculate dF/F
dF.shock <- function(files, d, d_resid){
    library(zoo) # to calculate moving averages
    
    # read the first file
    fileone <- read.csv(files[1])
    d_resid <- d_resid[,1:(ncol(fileone)-1)]
    
    # fit curve, take residuals
    for(i in 1:(ncol(fileone)-2)){ # number of cells
        plateau <- 0
        for(j in 1:(length(files)-1)){ # number of cycles
            f <- read.csv(files[j])
            
            # subset one column of data for model fitting
            thesub <- f[,c(2,i+2)]
            names(thesub) <- c('time','signal')
            
            # calculate moving averages of 3 points
            thesub$signal[2:(dim(thesub)[1]-1)] <- rollmean(thesub$signal, 3)
            temp <- filter(thesub, time < 10 | time > 60)
            
            # fit one phase decay curve with self-start
            d_model <- try(nls(signal ~ SSasymp(time, Asym, r0, lrc), data = temp), silent = TRUE)
            
            # if not fitted to decay curve, fit linear model
            if(class(d_model) == 'nls'){
                fit <- predict(d_model, list(time=thesub$time))
                d[,j+1] <- thesub$signal - fit
                plateau <- plateau + d_model$m$getPars()["Asym"]
            } else {
                d_model <- lm(signal ~ time, data = temp)
                fit <- predict(d_model, list(time=thesub$time))
                d[,j+1] <- thesub$signal - fit
                plateau <- plateau + tail(d_model$fitted.values, 1)
            }
        }
        
        # calculate row means (dF)
        d_temp <- mutate(d, mean=rowMeans(d[,-1], na.rm=TRUE))
        
        # calculate baseline and average of plateau (F)
        baseline <- mean(d_temp$mean[d_temp$time >= 50 & d_temp$time <= 55])
        plateau <- plateau/(length(files)-1)
        
        # calculate dF/F
        d_resid[,i+1] <- (d_temp$mean-baseline)/plateau
    }
    # output
    d_resid
}