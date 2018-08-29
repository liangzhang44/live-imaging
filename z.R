z.score<-function(d_cusum, v, f1){
    # calculate 3sd
    d_cusum$i <- 1:nrow(d_cusum)
    n <- sum(d_cusum$time <= 10)
    m <- sqrt(v/(ncol(f1)-2))
    
    # calculate z scores for data points after 10 sec
    d_cusum <- d_cusum %>%
        mutate(sd = sqrt(i*abs(i-n)*m/n)) %>%
        gather(key = "cell", value = "cusum", -time, -i, -sd) %>%
        mutate(cell = parse_number(cell)) %>%
        filter(time > 10) %>%
        mutate(z = cusum/sd) %>%
        group_by(cell)
    
    # calculate max and min z scores
    d_z <- summarise(d_cusum, max = max(z), min = min(z))
    d_z <- mutate(d_z, z = ifelse(abs(d_z$max)>abs(d_z$min), d_z$max, d_z$min))
    d_z$z
}