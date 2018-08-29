# calculate Z scores based on dF/F
# tone/trace baseline: 0-10sec, shock baseline: 50-55sec
z.dF<-function(d_resid){
    # calculate z scores
    if(nrow(d_resid) > 700){
        for(i in 1:(ncol(d_resid)-1)){
            avg1 <- mean(d_resid[d_resid$time < 10, i+1])
            std1 <- sd(d_resid[d_resid$time < 10, i+1])
            avg2 <- mean(d_resid[d_resid$time > 50 & d_resid$time < 55, i+1])
            std2 <- sd(d_resid[d_resid$time > 50 & d_resid$time < 55, i+1])
            d_resid[d_resid$time < 55,i+1] <- (d_resid[d_resid$time < 55,i+1] - avg1)/std1
            d_resid[d_resid$time >= 55,i+1] <- (d_resid[d_resid$time >= 55,i+1] - avg2)/std2
        }
    } else {
        for(i in 1:(ncol(d_resid)-1)){
            avg1 <- mean(d_resid[d_resid$time < 10, i+1])
            std1 <- sd(d_resid[d_resid$time < 10, i+1])
            d_resid[,i+1] <- (d_resid[,i+1] - avg1)/std1
        }
    }
    
    # regroup data by cell
    d_resid <- d_resid %>%
        gather(key = "cell", value = "z", -time) %>%
        mutate(cell = parse_number(cell))
    
    # output
    d_resid
}
