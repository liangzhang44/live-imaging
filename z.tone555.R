z.tone555<-function(d_resid){
    # subset time frame for z score calculation
    if(tail(d_resid$time, 1) > 50){
        d_resid <- filter(d_resid, time >= 5, time <= 55)
    } else {
        d_resid <- filter(d_resid, time >= 5, time <= 45)
    }
    
    # assign time blocks
    d_resid$block <- NA
    d_resid$block[d_resid$time > 5 & d_resid$time <= 10] <- "block 0"
    d_resid$block[d_resid$time > 10 & d_resid$time <= 15] <- "block 1"
    d_resid$block[d_resid$time > 15 & d_resid$time <= 20] <- "block 2"
    d_resid$block[d_resid$time > 20 & d_resid$time <= 25] <- "block 3"
    d_resid$block[d_resid$time > 25 & d_resid$time <= 30] <- "block 4"
    d_resid$block[d_resid$time > 30 & d_resid$time <= 35] <- "block 5"
    d_resid$block[d_resid$time > 35 & d_resid$time <= 40] <- "block 6"
    d_resid$block[d_resid$time > 40 & d_resid$time <= 45] <- "block 7"
    if(tail(d_resid$time, 1) > 45){
        d_resid$block[d_resid$time > 45 & d_resid$time <= 50] <- "block 8"
        d_resid$block[d_resid$time > 50 & d_resid$time <= 55] <- "block 9"
    }
    
    # calculate z scores for data points after 10 sec
    for(j in 1:(ncol(d_resid)-2)){
        d_resid[, j+1] <- cumsum(d_resid[, j+1])
        std <- sd(d_resid[d_resid$time > 5 & d_resid$time <= 10, j+1])
        avg <- mean(d_resid[d_resid$time > 5 & d_resid$time <= 10, j+1])
        d_resid[,j+1] <- (d_resid[,j+1]-avg)/std
    }
    
    # regroup data by cell and time block
    d_temp <- d_resid %>%
        gather(key = "cell", value = "z", -time, -block) %>%
        mutate(cell = parse_number(cell)) %>%
        group_by(cell, block)
    
    # calculate max and min z scores
    d_z <- summarise(d_temp, max = max(z), min = min(z))
    d_z$z <- ifelse(abs(d_z$max)>abs(d_z$min), d_z$max, d_z$min)
    d_z$max <- NULL
    d_z$min <- NULL
    d_z
}
