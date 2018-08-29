# 1. fit curve based on first 10 sec and last 2 sec
# 2. calculate residuals (dF) and plateaus (F)
# 3. calculate mean of the first and last 2 sessions
# 4. calculate dF/F
dF.training <- function(files, d, d_resid){
    library(zoo) # to calculate moving averages
    
    # read the first file
    fileone <- read.csv(files[1])
    d_resid <- d_resid[,1:(ncol(fileone)-1)]
    dF.list <- list(all = d_resid, early2 = d_resid, late2 = d_resid, session8 = d_resid)
    
    # fit curve, take residuals
    for(i in 1:(ncol(fileone)-2)){ # number of cells
        plateau <- 0
        for(j in 1:length(files)){ # number of sessions
            f <- read.csv(files[j])
            
            # subset one column of data for model fitting
            thesub <- f[,c(2,i+2)]
            names(thesub) <- c('time','signal')
            
            # calculate moving averages of 5 points
            thesub$signal[3:(dim(thesub)[1]-2)] <- rollmean(thesub$signal, 5)
            temp <- filter(thesub, time < 10 | time > 63)
            
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
        
        # calculate row means (dF) of sessions
        d <- mutate(d, all = rowMeans(d[,2:8]), early2 = rowMeans(d[,2:3]), 
                         late2 = rowMeans(d[,7:8]))
        
        # calculate average of plateau (F)
        plateau <- plateau/length(files)
        
        # calculate dF/F of all sessions
        sessions <- c("all", "early2", "late2", "session8")
        for (j in sessions) {
            baseline <- mean(d[d$time < 10, j])
            dF.list[[j]][,i+1] <- (d[,j] - baseline)/plateau
        }
    }
    
    # regroup data by cell
    for (i in sessions) {
        dF.list[[i]] <- dF.list[[i]] %>%
            gather(key = "cell", value = "dF", -time) %>%
            mutate(cell = parse_number(cell))
    }
    # output
    dF.list
}