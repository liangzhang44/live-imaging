library(tidyverse)
library(readxl)
library(ggpubr)
source("E:/Live imaging/cusum.z.control.R")

# process all trace fear extinction folders for 5 mice
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]
for(i in 1:length(allfolders)){
    cusum.z.control(allfolders[i])
}

# load z score training files of 5 mice
all.training <- data.frame()
for(i in 1:length(allfolders)){
    tone <- read.csv(paste0("./", allfolders[i], "/z training cycles.csv"))
    tone$cell <- tone$cell+100*i
    tone[is.na(tone)] <- 0
    all.training <- rbind(all.training, tone)
}

all.training$mouse <- NA
all.training[all.training$cell<200,"mouse"] <- "mouse1"
all.training[all.training$cell<400&all.training$cell>200,"mouse"] <- "mouse2"
all.training[all.training$cell<500&all.training$cell>400,"mouse"] <- "mouse3"
all.training[all.training$cell<700&all.training$cell>500,"mouse"] <- "mouse4"
all.training[all.training$cell>700,"mouse"] <- "mouse5"

all.training[,2:9] <- all.training[,2:9]>3
training <- all.training %>%
    gather(key = "cycle", value = "activated", -cell, -mouse) %>%
    group_by(mouse, cycle) %>%
    summarise(rate=mean(activated))
ggbarplot(training, x = "cycle", y = "rate", add = "mean_se")

# load z score test1 files of 5 mice
all.test <- data.frame()
for(i in 1:length(allfolders)){
    tone <- read.csv(paste0("./", allfolders[i], "/z test1 cycles.csv"))
    tone$cell <- tone$cell+100*i
    tone[is.na(tone)] <- 0
    all.test <- rbind(all.test, tone)
}

all.test$mouse <- NA
all.test[all.test$cell<200,"mouse"] <- "mouse1"
all.test[all.test$cell<400&all.test$cell>200,"mouse"] <- "mouse2"
all.test[all.test$cell<500&all.test$cell>400,"mouse"] <- "mouse3"
all.test[all.test$cell<700&all.test$cell>500,"mouse"] <- "mouse4"
all.test[all.test$cell>700,"mouse"] <- "mouse5"

all.test[,2:7] <- all.test[,2:7]>3
test <- all.test %>%
    gather(key = "cycle", value = "activated", -cell, -mouse, na.rm=TRUE) %>%
    group_by(mouse, cycle) %>%
    summarise(rate=mean(activated))
ggbarplot(test, x = "cycle", y = "rate", add = "mean_se")
