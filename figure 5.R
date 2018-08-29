library(tidyverse)
library(ggpubr)

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# read Z score of tone files of 5 mice
training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z training all.csv"))
    temp$cell <- temp$cell+100*i
    training <- rbind(training, temp)
}

training <- training %>%
    spread(key = "time", value = "z")

temp <- training[, c(-1)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
training[, c(-1)] <- temp

# read Z score files of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test1 all.csv"))
    temp$cell <- temp$cell+100*i
    test1 <- rbind(test1, temp)
}

test1 <- test1 %>%
    spread(key = "time", value = "z")

temp <- test1[, c(-1)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test1[, c(-1)] <- temp

# read Z score files of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test2 all.csv"))
    temp$cell <- temp$cell+100*i
    test2 <- rbind(test2, temp)
}

test2 <- test2 %>%
    spread(key = "time", value = "z")

temp <- test2[, c(-1)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test2[, c(-1)] <- temp

# load z score files of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test3 all.csv"))
    temp$cell <- temp$cell+100*i
    test3 <- rbind(test3, temp)
}

test3 <- test3 %>%
    spread(key = "time", value = "z")

temp <- test3[, c(-1)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test3[, c(-1)] <- temp

# load z score files of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test4 all.csv"))
    temp$cell <- temp$cell+100*i
    test4 <- rbind(test4, temp)
}

test4 <- test4 %>%
    spread(key = "time", value = "z")

temp <- test4[, c(-1)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test4[, c(-1)] <- temp

training.tone <- training %>%
    dplyr::select(cell, `10.06`:`24.97`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Training", period = "Tone", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)
training.trace <- training %>%
    dplyr::select(cell, `25.06`:`54.97`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Training", period = "Trace", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)

test1.tone <- test1 %>%
    dplyr::select(cell, `10.06`:`14.91`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Test1", period = "Tone", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)
test1.trace <- test1 %>%
    dplyr::select(cell, `15`:`44.91`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Test1", period = "Trace", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)

test2.tone <- test2 %>%
    dplyr::select(cell, `10.06`:`14.91`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Test2", period = "Tone", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)
test2.trace <- test2 %>%
    dplyr::select(cell, `15`:`44.91`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Test2", period = "Trace", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)

test3.tone <- test3 %>%
    dplyr::select(cell, `10.06`:`14.91`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Test3", period = "Tone", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)
test3.trace <- test3 %>%
    dplyr::select(cell, `15`:`44.91`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Test3", period = "Trace", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)

test4.tone <- test4 %>%
    dplyr::select(cell, `10.06`:`14.91`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Test4", period = "Tone", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)
test4.trace <- test4 %>%
    dplyr::select(cell, `15`:`44.91`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "Test4", period = "Trace", pos = sum(z == 1)/n()*100, neg = sum(z == -1)/n()*100)

perc <- rbind(training.tone, training.trace, test1.tone, test1.trace, test2.tone, test2.trace,
              test3.tone, test3.trace, test4.tone, test4.trace)
perc$group <- NA
group1 <- as.character(training.tone$cell[training.tone$pos > 0 & test1.tone$pos > 0])
group2 <- as.character(training.tone$cell[training.tone$pos == 0 & training.trace$pos > 0])
group3 <- as.character(training.tone$cell[test1.tone$pos == 0 & test3.tone$pos > 0])


perc$group[perc$cell %in% group1] <- "group1"
perc$group[perc$cell %in% group2] <- "group2"
perc$group[perc$cell %in% group3] <- "group3"
perc$group[is.na(perc$group)] <- "ungrouped"

# figure 5a
f5a <- ggline(perc[perc$group == "group1",], x = "phase", y = "pos", color = "period", 
       group = "cell", facet.by = "cell", scale = "free_y")+
    labs(x = "Phase", y = "Active Time (%)")+
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_blank())

# figure 5b
f5b <- ggline(perc[perc$group == "group2",], x = "phase", y = "pos", color = "period", 
       group = "cell", facet.by = "cell", scale = "free_y")+
    labs(x = "Phase", y = "Active Time (%)")+
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_blank())

# figure 5c
f5c <- ggline(perc[perc$group == "group3",], x = "phase", y = "pos", color = "period", 
       group = "cell", facet.by = "cell", scale = "free_y")+
    labs(x = "Phase", y = "Active Time (%)")+
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_blank())

ggline(perc[perc$group == "ungrouped",], x = "phase", y = "pos", color = "period", 
       group = "cell", facet.by = "cell", scale = "free_y")+
    labs(x = "Phase", y = "Active Time (%)")+
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

figure5 <- ggarrange(f5a, f5b, f5c, labels = c("A", "B", "C"), nrow = 3, ncol = 1)
figure5 <- annotate_figure(figure5, fig.lab = "Figure 5", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure5, filename = "new figure 5.png", height = 12, width = 6)
