# calculate mean residuals of entire cycle for each cell
resid.all.mean <- function(files, d, d_resid){
    # read each file
    fileone <- read.csv(files[1])
    d_resid <- d_resid[,1:(ncol(fileone)-1)]
    
    # fit curve, take residuals
    for(i in 1:(ncol(fileone)-2)){ # number of cells
        for(j in 1:(length(files)-1)){ # number of cycles
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
        
        # if not fitted to decay curve, fit linear model
        if(sum(is.na(d))/nrow(d) > ncol(d)/2){
            for(k in 1:(length(files)-1)){
                f <- read.csv(files[k])
                
                # subset one column of data for model fitting
                thesub <- f[,c(2,i+2)]
                names(thesub) <- c('time','signal')
                
                # fit linear model
                d_model <- lm(signal ~ time, data = thesub)
                d[,k+1] <- resid(d_model)
            }
        }
        
        # calculate row means
        d_temp <- mutate(d, mean=rowMeans(d[,-1], na.rm=TRUE))
        
        # calculate baseline of baseline
        baseline <- mean(d_temp$mean[d_temp$time >= 5 & d_temp$time <= 10])
        
        # substract baseline from each datapoints
        d_resid[,i+1] <- d_temp$mean-baseline
    }
    # output
    d_resid
}