cusumplot <- function(d_cusum, m, f1) {
    library(directlabels)
    # calculate sd
    d_cusum$i <- 1:nrow(d_cusum)
    n <- sum(d_cusum$time<=10)
    m <- sqrt(m/(ncol(f1)-2))
    
    d_cusum <- d_cusum %>%
        mutate(sd=sqrt(i*abs(i-n)*m/n)) %>%
        gather(key = "cell", value = "cusum", -time, -i, -sd) %>%
        mutate(cell = factor(parse_number(cell)))
    
    # plot all cells and +-3sd
    ggplot(data = d_cusum, mapping = aes(time, cusum, col=cell)) +
        geom_line()+
        geom_line(mapping = aes(time, 3*sd), color = "black", size = 1.5) +
        geom_line(mapping = aes(time, -3*sd), color = "black", size = 1.5) +
        scale_y_continuous(limits = c(-100, 100))+
        geom_dl(aes(label = cell), 
                method = list(dl.trans(x = x + 0.2), "last.points"))
    
}
