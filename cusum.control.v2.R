cusum.control <- function(files, d, z_df){
    # read each file, fit curve, take residuals, put into d_cusum
    fileone <- read.csv(files[1])
    
    for(i in 1:(ncol(fileone)-2)){ # number of cells
        for(j in 1:length(files)){ # number of cycles
            f <- read.csv(files[j])
            
            # subset one column of data for model fitting
            thesub <- f[,c(2,i+2)]
            names(thesub) <- c('time','signal')
            
            # fit one phase decay curve with self-start
            d_model1 <- try(nls(signal ~ SSasymp(time, Asym, r0, lrc), data = thesub), silent = TRUE)
            d_model2 <- lm(signal ~ time, data = thesub)
            # discard data if not fitted
            if(class(d_model1) == 'nls'){
                d[,j+1] <- resid(d_model1)
            } else {
                d[,j+1] <- resid(d_model2)
            }
            
            # calculate baseline for each cycle
            baseline <- mean(d[d$time >= 5 & d$time <= 10, j+1])
            # substract baseline from each datapoints
            d[,j+1] <- d[,j+1]-baseline
            # calculate CuSum
            d[,j+1] <- cumsum(d[,j+1])
            # calculate std and mean for cusum
            std <- sd(d[d$time >= 5 & d$time <= 10, j+1])
            avg <- mean(d[d$time >= 5 & d$time <= 10, j+1])
            # calculate z score for each cycle
            d[,j+1] <- (d[,j+1]-avg)/std
        }
        
        # subset from 10 to 15 sec
        d_temp <- d %>%
            filter(time>=10, time<=15) %>%
            gather(key = "cycle", value = "z", -time) %>%
            mutate(cycle = parse_number(cycle)) %>%
            group_by(cycle) %>%
            summarise(max = max(z), min = min(z))
        
        d_temp <- mutate(d_temp, z = ifelse(abs(d_temp$max)>abs(d_temp$min), 
                                            d_temp$max, d_temp$min))
        
        # put CuSum into the cusum data frame
        z_df[i,-1] <- d_temp$z
    }
    # output
    z_df
}