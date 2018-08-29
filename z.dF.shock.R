z.dF.shock<-function(d_resid){
    # subset time frame for z score calculation
    d_resid <- filter(d_resid, time >= 50, time <= 60)
    
    # assign time blocks
    d_resid$block <- NA
    d_resid$block[d_resid$time > 50 & d_resid$time <= 55] <- "before"
    d_resid$block[d_resid$time > 55 & d_resid$time <= 60] <- "after"
    
    # calculate z scores for data points after 10 sec
    for(j in 1:(ncol(d_resid)-2)){
        for(i in 1:length(unique(d_resid$block))){
            d_resid[d_resid$block==unique(d_resid$block)[i], j+1] <- 
                cumsum(d_resid[d_resid$block==unique(d_resid$block)[i], j+1])
        }
        std <- sd(d_resid[d_resid$block=="before", j+1])
        avg <- mean(d_resid[d_resid$block=="before", j+1])
        d_resid[,j+1] <- (d_resid[,j+1]-avg)/std
    }
    
    # regroup data by cell and time block
    d_resid <- d_resid %>%
        gather(key = "cell", value = "z", -time, -block) %>%
        mutate(cell = parse_number(cell))
    
    # output
    d_resid
}
