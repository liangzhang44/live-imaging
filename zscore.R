# calculate Z scores based on dF/F
# baseline: 0-10sec
zscore<-function(d_resid){
    # spread dF/F data by cell
    d_resid <- spread(d_resid, key = "cell", value = "dF")
    
    # calculate z scores
    for(i in 1:(ncol(d_resid)-1)){
        avg <- mean(d_resid[d_resid$time < 10, i+1])
        std <- sd(d_resid[d_resid$time < 10, i+1])
        d_resid[,i+1] <- (d_resid[,i+1] - avg)/std
    }
    
    # regroup data by cell
    d_resid <- gather(d_resid, key = "cell", value = "z", -time)
    
    # output
    d_resid
}
