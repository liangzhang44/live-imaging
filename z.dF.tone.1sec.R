z.dF.tone.1sec<-function(d_resid){
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
    
    # calculate sum for each second
    d_resid <- d_resid %>%
        gather(key = "cell", value = "dF", -time, -block) %>%
        mutate(cell = parse_number(cell)) %>%
        group_by(cell, block) %>%
        summarise(sum = sum(dF)) %>%
        spread(key = "cell", value = "sum")
    
    # calculate z scores
    d_resid <- as.data.frame(d_resid)
    for(i in 1:(ncol(d_resid)-1)){
        avg <- mean(d_resid[1:5, i+1])
        std <- sd(d_resid[1:5, i+1])
        d_resid[,i+1] <- (d_resid[,i+1]-avg)/std
    }
    
    # regroup data by cell
    d_resid <- gather(d_resid, key = "cell", value = "z", -block)
        
    # output
    d_resid
}
