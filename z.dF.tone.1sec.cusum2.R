z.dF.tone.1sec.cusum2<-function(d_resid){
    # subset time frame for z score calculation, assign time blocks
    if(tail(d_resid$time, 1) > 50){
        d_resid <- d_resid %>%
            filter(time >= 5, time <= 55) %>%
            mutate(block = cut(time, breaks = 5:55, labels = 5:54))
    } else {
        d_resid <- d_resid %>%
            filter(time >= 5, time <= 45) %>%
            mutate(block = cut(time, breaks = 5:45, labels = 5:44))
    }
    
    # calculate cumsum for each second
    for(j in 1:(ncol(d_resid)-2)){
        for(i in 1:length(unique(d_resid$block))){
            d_resid[d_resid$block==unique(d_resid$block)[i], j+1] <- 
                cumsum(d_resid[d_resid$block==unique(d_resid$block)[i], j+1])
        }
    }
    
    # subset the first 5 seconds and calcualte base cumsum, mean, std
    base <- filter(d_resid, block %in% c("5", "6", "7", "8", "9")) %>%
        gather(key = "cell", value = "cusum", -time, -block) %>%
        mutate(cell = parse_number(cell)) %>%
        group_by(cell) %>%
        summarise(avg = mean(cusum), std = sd(cusum))
    # pick max or min for each second
    d_resid <- d_resid %>%
        gather(key = "cell", value = "cusum", -time, -block) %>%
        mutate(cell = parse_number(cell)) %>%
        group_by(cell, block) %>%
        summarise(max=max(cusum), min=min(cusum))
    d_resid$cusum <- ifelse(abs(d_resid$max)>abs(d_resid$min), d_resid$max, d_resid$min)
    d_resid <- d_resid %>%
        mutate(max = NULL, min = NULL) %>%
        spread(key = "cell", value = "cusum")
    
    # calculate z scores
    for(i in 1:(ncol(d_resid)-1)){
        d_resid[,i+1] <- (d_resid[,i+1]-base$avg[i])/base$std[i]
    }
    
    # regroup data by cell
    d_resid <- gather(d_resid, key = "cell", value = "z", -block)
    
    # output
    d_resid
}
