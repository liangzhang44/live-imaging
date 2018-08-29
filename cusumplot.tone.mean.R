cusumplot.tone <- function(d_cusum, m, f1) {
    # calculate sd
    i <- 1:nrow(d_cusum)
    n <- sum(d_cusum$time<=10)
    
    # calculate z scores for data points after 10 sec
    d_sd <- d_cusum
    for(j in 1:(ncol(f1)-2)){
        d_sd[,j+1] <- sqrt(i*abs((i-n)*m[j])/n)
    }
    
    d_cusum <- d_cusum %>%
        gather(key = "cell", value = "cusum", -time) %>%
        mutate(cell = factor(parse_number(cell)))
    
    d_sd <- d_sd %>%
        gather(key = "cell", value = "sd", -time) %>%
        mutate(cell = factor(parse_number(cell)))
    
    # plot all cells and +-3sd
    ggplot(data = d_cusum, mapping = aes(time, cusum)) +
        geom_line()+
        geom_line(data = d_sd, mapping = aes(time, 3*sd), color = "black", lty=2) +
        geom_line(data = d_sd, mapping = aes(time, -3*sd), color = "black", lty=2) +
        scale_y_continuous(limits = c(-150, 150))+
        facet_wrap(~cell, nrow = 3)
    
}
