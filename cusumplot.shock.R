cusumplot.shock <- function(d_cusum, m, av) {
    # calculate z scores for data points after 55 sec
    d_sd <- d_cusum
    for(j in 1:(ncol(d_cusum)-1)){
        d_sd[,j+1] <- m[j]
    }
    
    for(j in 1:(ncol(d_cusum)-1)){
        d_cusum[,j+1] <- d_cusum[,j+1]-av[j]
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
        geom_vline(xintercept = 55, lty=2)+
        scale_y_continuous(limits = c(-150, 150))+
        facet_wrap(~cell, nrow = 3)
}