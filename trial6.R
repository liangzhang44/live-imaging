library(tidyverse)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(zoo)
library(magick)
source("normalization.R")

# normalize test data, calculate baseline firing rate
firing.all <- data.frame()
for (i in 1:4) {
    assign(paste0("test", i), normalization(paste0("z test", i, " all.csv")))
    firing <- filter(get(paste0("test", i)), time < 10)
    firing.all <- rbind(firing.all, firing)
}
cutoff <- 3 * sd(firing.all$z)
p1 <- sum(firing.all$z >= cutoff)/nrow(firing.all)
p2 <- sum(firing.all$z <= -1*cutoff)/nrow(firing.all)

# calculate firing rate of the trace period, rank by trace activity, determine activation
tests <- data.frame()
for(i in 1:4){
    assign("temp", get(paste0("test", i)))
    firing <- temp %>%
        filter(time > 15, time < 45) %>%
        group_by(cell) %>%
        summarise(change = (sum(z >= cutoff) - sum(z <= -1*cutoff))/n())
    # rearrange by late2 order
    temp <- temp %>%
        filter(time > 10, time < 45) %>%
        spread(key = "time", value = "z")
    temp$change <- firing$change
    # determine activity
    temp$activity <- "Unchanged"
    temp$activity[temp$change < -10*p2] <- "Inhibited"
    temp$activity[temp$change > 10*p1] <- "Activated"
    # select out trace cells
    temp <- mutate(temp, test = paste0("test", i), trial = "Averaged Five Trials")
    tests <- rbind(tests, temp)
}

tests.control <- data.frame()
for (i in 1:4) {
    assign("temp", normalization(paste0("z test", i, " trial6.csv")))
    firing <- filter(temp, time < 10)
    cutoff <- 3 * sd(firing$z)
    p1 <- sum(firing$z >= cutoff)/nrow(firing)
    p2 <- sum(firing$z <= -1*cutoff)/nrow(firing)
    firing <- temp %>%
        filter(time > 15, time < 45) %>%
        group_by(cell) %>%
        summarise(change = (sum(z >= cutoff) - sum(z <= -1*cutoff))/n())
    # rearrange by late2 order
    temp <- temp %>%
        filter(time > 10, time < 45) %>%
        spread(key = "time", value = "z")
    temp$change <- firing$change
    # determine activity
    temp$activity <- "Unchanged"
    temp$activity[temp$change < -10*p2] <- "Inhibited"
    temp$activity[temp$change > 10*p1] <- "Activated"
    # select out trace cells
    temp <- mutate(temp, test = paste0("test", i), trial = "Trial 6")
    tests.control <- rbind(tests.control, temp)
}


alltests <- rbind(tests, tests.control)
bars <- alltests %>%
    group_by(test, activity, trial) %>%
    summarise(number = n())
bars[is.na(bars)] <- 0

ggbarplot(bars, x = "test", y = "number", fill = "activity", facet.by = "trial")
