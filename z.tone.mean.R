z.score.tone<-function(d_cusum, m, f1){
    # calculate 3sd
    i <- 1:nrow(d_cusum)
    n <- sum(d_cusum$time <= 10)
    
    # calculate z scores for data points after 10 sec
    d_zscore <- d_cusum
    for(j in 1:(ncol(f1)-2)){
        d_zscore[,j+1] <- sqrt(i*abs((i-n)*m[j])/n)
        d_cusum[,j+1] <- d_cusum[,j+1]/d_zscore[,j+1]
    }
    d_cusum <- d_cusum %>%
        gather(key = "cell", value = "z", -time) %>%
        mutate(cell = parse_number(cell)) %>%
        filter(time > 11) %>%
        group_by(cell)
    
    # calculate max and min z scores
    d_z <- summarise(d_cusum, max = max(z), min = min(z))
    d_z <- mutate(d_z, z = ifelse(abs(d_z$max)>abs(d_z$min), d_z$max, d_z$min))
    d_z$z
}
