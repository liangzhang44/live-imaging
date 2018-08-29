# calculate Z scores based on dF/F
# tone/trace baseline: 0-10sec, shock baseline: 50-55sec
z.dF.1sec<-function(d_resid){
    # subset time frame for z score calculation, assign time blocks
    if(tail(d_resid$time, 1) > 50){
        d_resid <- d_resid %>%
            filter(time < 60) %>%
            mutate(block = cut(time, breaks = 0:60, labels = 0:59, right = FALSE))
    } else {
        d_resid <- d_resid %>%
            filter(time < 45) %>%
            mutate(block = cut(time, breaks = 0:45, labels = 0:44, right = FALSE))
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
    if(nrow(d_resid) > 50){
        for(i in 1:(ncol(d_resid)-1)){
            avg1 <- mean(d_resid[1:10, i+1])
            std1 <- sd(d_resid[1:10, i+1])
            avg2 <- mean(d_resid[51:55, i+1])
            std2 <- sd(d_resid[51:55, i+1])
            d_resid[1:55,i+1] <- (d_resid[1:55,i+1]-avg1)/std1
            d_resid[56:60,i+1] <- (d_resid[56:60,i+1]-avg2)/std2
        }
    } else {
        for(i in 1:(ncol(d_resid)-1)){
            avg1 <- mean(d_resid[1:10, i+1])
            std1 <- sd(d_resid[1:10, i+1])
            d_resid[,i+1] <- (d_resid[,i+1]-avg1)/std1
        }
    }
    
    # regroup data by cell
    d_resid <- gather(d_resid, key = "cell", value = "z", -block)
    
    # output
    d_resid
}
