z.tone<-function(d_cusum, m, av){
    # calculate z scores for data points after 10 sec
    for(j in 1:(ncol(d_cusum)-1)){
        d_cusum[,j+1] <- (d_cusum[,j+1]-av[j])/m[j]
    }
    d_cusum <- d_cusum %>%
        gather(key = "cell", value = "z", -time) %>%
        mutate(cell = parse_number(cell)) %>%
        filter(time > 10) %>%
        group_by(cell)
    
    # calculate max and min z scores
    d_z <- summarise(d_cusum, max = max(z), min = min(z))
    d_z <- mutate(d_z, z = ifelse(abs(d_z$max)>abs(d_z$min), d_z$max, d_z$min))
    d_z$z
}
