library(tidyverse)
library(readxl)
library(RColorBrewer)
library(circlize)
library(ggpubr)
library(ComplexHeatmap)
source("dF.all.R")

# calculate dF/F for 5 mice
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]
for(i in 1:length(allfolders)){
    dF.all(allfolders[i])
}

# read dF/F data of training
training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF training.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    training <- rbind(training, temp)
}

# plot dF/F data of training
training$cell <- factor(training$cell)
ggplot(training, aes(x=time, y=dF, color=cell))+
    geom_smooth(span=0.1, se=FALSE, size=1.5)+
    geom_point(size=0.1)+
    geom_vline(xintercept = 10, color="blue")+
    geom_vline(xintercept = 25, color="blue")+
    geom_vline(xintercept = 55, color="red")+
    facet_wrap(~cell, nrow = 8, scales = "free_y")+
    theme_pubr()+
    labs(x="Time (sec)", y=expression(Delta*"F/F (%)"), title=expression(Delta*"F/F for Training"))+
    theme(legend.position = "none", strip.background = element_blank(),
          strip.text.x = element_blank())
ggsave("dF training.png", width = 12, height = 8)

# read dF/F data of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF test1.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    test1 <- rbind(test1, temp)
}

# plot dF/F data of test1
test1$cell <- factor(test1$cell)
ggplot(test1, aes(x=time, y=dF, color=cell))+
    geom_smooth(span=0.1, se=FALSE, size=1.5)+
    geom_point(size=0.1)+
    geom_vline(xintercept = 10, color="blue")+
    geom_vline(xintercept = 15, color="blue")+
    facet_wrap(~cell, nrow = 8, scales = "free_y")+
    theme_pubr()+
    labs(x="Time (sec)", y=expression(Delta*"F/F (%)"), title=expression(Delta*"F/F for Test 1"))+
    theme(legend.position = "none", strip.background = element_blank(),
          strip.text.x = element_blank())
ggsave("dF test1.png", width = 12, height = 8)

# read dF/F data of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF test2.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    test2 <- rbind(test2, temp)
}

# plot dF/F data of test2
test2$cell <- factor(test2$cell)
ggplot(test2, aes(x=time, y=dF, color=cell))+
    geom_smooth(span=0.1, se=FALSE, size=1.5)+
    geom_point(size=0.1)+
    geom_vline(xintercept = 10, color="blue")+
    geom_vline(xintercept = 15, color="blue")+
    facet_wrap(~cell, nrow = 8, scales = "free_y")+
    theme_pubr()+
    labs(x="Time (sec)", y=expression(Delta*"F/F (%)"), title=expression(Delta*"F/F for Test 2"))+
    theme(legend.position = "none", strip.background = element_blank(),
          strip.text.x = element_blank())
ggsave("dF test2.png", width = 12, height = 8)

# read dF/F data of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF test3.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    test3 <- rbind(test3, temp)
}

# plot dF/F data of test3
test3$cell <- factor(test3$cell)
ggplot(test3, aes(x=time, y=dF, color=cell))+
    geom_smooth(span=0.1, se=FALSE, size=1.5)+
    geom_point(size=0.1)+
    geom_vline(xintercept = 10, color="blue")+
    geom_vline(xintercept = 15, color="blue")+
    facet_wrap(~cell, nrow = 8, scales = "free_y")+
    theme_pubr()+
    labs(x="Time (sec)", y=expression(Delta*"F/F (%)"), title=expression(Delta*"F/F for Test 3"))+
    theme(legend.position = "none", strip.background = element_blank(),
          strip.text.x = element_blank())
ggsave("dF test3.png", width = 12, height = 8)

# read dF/F data of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF test4.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    test4 <- rbind(test4, temp)
}

# plot dF/F data of test4
test4$cell <- factor(test4$cell)
ggplot(test4, aes(x=time, y=dF, color=cell))+
    geom_smooth(span=0.1, se=FALSE, size=1.5)+
    geom_point(size=0.1)+
    geom_vline(xintercept = 10, color="blue")+
    geom_vline(xintercept = 15, color="blue")+
    facet_wrap(~cell, nrow = 8, scales = "free_y")+
    theme_pubr()+
    labs(x="Time (sec)", y=expression(Delta*"F/F (%)"), title=expression(Delta*"F/F for Test 4"))+
    theme(legend.position = "none", strip.background = element_blank(),
          strip.text.x = element_blank())
ggsave("dF test4.png", width = 12, height = 8)



# process dF/F data into Z scores
source("dF.all.z.tone.R")
source("dF.z.shock.R")

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# calculate Z scores of tone for 5 mice
for(i in 1:length(allfolders)){
    dF.all.z.tone(allfolders[i])
}

# calculate Z scores of shock for 5 mice
for(i in 1:length(allfolders)){
    dF.z.shock(allfolders[i])
}

# read Z score of tone files of 5 mice
tone.training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone training.csv"))
    temp$cell <- temp$cell+100*i
    tone.training <- rbind(tone.training, temp)
}

# read Z score of shock files of 5 mice
shock.training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z shock.csv"))
    temp$cell <- temp$cell+100*i
    shock.training <- rbind(shock.training, temp)
}

# summarise maximum of z score of tone for each cell and each block
tone.training.max<- tone.training %>%
    group_by(cell, block) %>%
    summarise(max=max(z)) %>%
    spread(key = "block", value = "max")
tone.training.max$cell <- as.character(tone.training.max$cell)

# summarise maximum of z score of shock for each cell and each block
shock.training.max<- shock.training %>%
    group_by(cell, block) %>%
    summarise(max=max(z)) %>%
    spread(key = "block", value = "max")
shock.training.max$cell <- as.character(shock.training.max$cell)
shock.training.max$before <- NULL
colnames(shock.training.max) <- c("cell", "Training-Shock")

# merge shock and tone data of training
all <- merge(shock.training.max, tone.training.max, by="cell")

# asign types based on Z scores during shock, block1, block2, block3
type1a <- as.character(all$cell[(all$`block 1` > 3 & all$`block 2` > 3 & all$`block 3` > 3)
                                & all$`Training-Shock` > 3])
type1b <- as.character(all$cell[((all$`block 1` > 3 & all$`block 2` > 3 & all$`block 3` < 3) |
                                     (all$`block 1` > 3 & all$`block 2` < 3 & all$`block 3` > 3) |
                                     (all$`block 1` < 3 & all$`block 2` > 3 & all$`block 3` > 3))
                                & all$`Training-Shock` > 3])
type1c <- as.character(all$cell[((all$`block 1` > 3 & all$`block 2` < 3 & all$`block 3` < 3) |
                                     (all$`block 1` < 3 & all$`block 2` > 3 & all$`block 3` < 3) |
                                     (all$`block 1` < 3 & all$`block 2` < 3 & all$`block 3` > 3))
                                & all$`Training-Shock` > 3])
type1d <- as.character(all$cell[all$`block 1` < 3 & all$`block 2` < 3 & all$`block 3` < 3
                               & all$`Training-Shock` > 3])

type2a <- as.character(all$cell[(all$`block 1` > 3 & all$`block 2` > 3 & all$`block 3` > 3)
                                & all$`Training-Shock` < 3])
type2b <- as.character(all$cell[((all$`block 1` > 3 & all$`block 2` > 3 & all$`block 3` < 3) |
                                     (all$`block 1` > 3 & all$`block 2` < 3 & all$`block 3` > 3) |
                                     (all$`block 1` < 3 & all$`block 2` > 3 & all$`block 3` > 3))
                                & all$`Training-Shock` < 3])
type2c <- as.character(all$cell[((all$`block 1` > 3 & all$`block 2` < 3 & all$`block 3` < 3) |
                                     (all$`block 1` < 3 & all$`block 2` > 3 & all$`block 3` < 3) |
                                     (all$`block 1` < 3 & all$`block 2` < 3 & all$`block 3` > 3))
                                & all$`Training-Shock` < 3])
type2d <- as.character(all$cell[all$`block 1` < 3 & all$`block 2` < 3 & all$`block 3` < 3
                               & all$`Training-Shock` < 3])


order1 <- which(all.tone$`block 1`>3 & all.tone$`block 2`>3 & all.tone$`block 3`>3)
order2 <- which(all.tone$`block 1`>3 & all.tone$`block 2`>3 & all.tone$`block 3`<3)
order3 <- which(all.tone$`block 1`>3 & all.tone$`block 2`<3 & all.tone$`block 3`>3)
order4 <- which(all.tone$`block 1`<3 & all.tone$`block 2`>3 & all.tone$`block 3`>3)
order5 <- which(all.tone$`block 1`>3 & all.tone$`block 2`<3 & all.tone$`block 3`<3)
order6 <- which(all.tone$`block 1`<3 & all.tone$`block 2`>3 & all.tone$`block 3`<3)
order7 <- which(all.tone$`block 1`<3 & all.tone$`block 2`<3 & all.tone$`block 3`>3)
order8 <- which(all.tone$`block 1`<3 & all.tone$`block 2`<3 & all.tone$`block 3`<3)
order <- c(order1, order2, order3, order4, order5, order6, order7,order8)


training <- tone.training %>%
    mutate(block = NULL) %>%
    spread(key = "time", value = "z") %>%
    mutate(type = NA)
training$cell <- as.character(training$cell)
training$type[training$cell %in% type1a] <- "Type Ia"
training$type[training$cell %in% type1b] <- "Type Ib"
training$type[training$cell %in% type1c] <- "Type Ic"
training$type[training$cell %in% type1d] <- "Type Id"
training$type[training$cell %in% type2a] <- "Type IIa"
training$type[training$cell %in% type2b] <- "Type IIb"
training$type[training$cell %in% type2c] <- "Type IIc"
training$type[training$cell %in% type2d] <- "Type IId"
training <- training[order,]

temp <- training[, c(-1, -589)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
training[, c(-1, -589)] <- temp

col <- colorRamp2(c(1, 0, -1), brewer.pal(3, "RdBu"))
timeticks <- c(1, 59, 118, 177, 236, 295, 354, 413, 472, 531, 587)
timelabels <- c("5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55")
anno = anno_histogram(as.matrix(training[,61:588]), 
                      gp = gpar(fill = "orange"), which = "row")
anno = anno_density(as.matrix(training[,61:588]), which = "row", type = "line")

ha_right = HeatmapAnnotation(violin=anno, which = "row", width = unit(8, "cm"))
ha_bottom <- columnAnnotation(link=column_anno_link(at=timeticks, labels=timelabels, side="bottom"),
                              gp=gpar(fontsize=14))
png("dF z training.png", height = 1024, width = 1024)
Heatmap(training[,c(-1, -589)],col = col, name = "Training",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time (sec)", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = training$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# read Z score files of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone test1.csv"))
    temp$cell <- temp$cell+100*i
    test1 <- rbind(test1, temp)
}

test1 <- test1 %>%
    mutate(block = NULL) %>%
    spread(key = "time", value = "z") %>%
    mutate(type = NA)

test1$type[test1$cell %in% type1a] <- "Type Ia"
test1$type[test1$cell %in% type1b] <- "Type Ib"
test1$type[test1$cell %in% type1c] <- "Type Ic"
test1$type[test1$cell %in% type1d] <- "Type Id"
test1$type[test1$cell %in% type2a] <- "Type IIa"
test1$type[test1$cell %in% type2b] <- "Type IIb"
test1$type[test1$cell %in% type2c] <- "Type IIc"
test1$type[test1$cell %in% type2d] <- "Type IId"
test1 <- test1[order,]

temp <- test1[, c(-1, -472)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test1[, c(-1, -472)] <- temp

timeticks <- c(1, 59, 118, 177, 236, 295, 354, 413, 470)
timelabels <- c("5", "10", "15", "20", "25", "30", "35", "40", "45")
ha_bottom <- columnAnnotation(link=column_anno_link(at=timeticks, labels=timelabels, side="bottom"),
                              gp=gpar(fontsize=14))

png("dF z test1.png", height = 1024, width = 1024)
Heatmap(test1[,c(-1, -472)],col = col, name = "Test 1",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test1$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# read Z score files of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone test2.csv"))
    temp$cell <- temp$cell+100*i
    test2 <- rbind(test2, temp)
}

test2 <- test2 %>%
    mutate(block = NULL) %>%
    spread(key = "time", value = "z") %>%
    mutate(type = NA)

test2$type[test2$cell %in% type1a] <- "Type Ia"
test2$type[test2$cell %in% type1b] <- "Type Ib"
test2$type[test2$cell %in% type1c] <- "Type Ic"
test2$type[test2$cell %in% type1d] <- "Type Id"
test2$type[test2$cell %in% type2a] <- "Type IIa"
test2$type[test2$cell %in% type2b] <- "Type IIb"
test2$type[test2$cell %in% type2c] <- "Type IIc"
test2$type[test2$cell %in% type2d] <- "Type IId"
test2 <- test2[order,]

temp <- test2[, c(-1, -472)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test2[, c(-1, -472)] <- temp

png("dF z test2.png", height = 1024, width = 1024)
Heatmap(test2[,c(-1, -472)],col = col, name = "Test 2",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test2$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# load z score files of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone test3.csv"))
    temp$cell <- temp$cell+100*i
    test3 <- rbind(test3, temp)
}

test3 <- test3 %>%
    mutate(block = NULL) %>%
    spread(key = "time", value = "z") %>%
    mutate(type = NA)

test3$type[test3$cell %in% type1a] <- "Type Ia"
test3$type[test3$cell %in% type1b] <- "Type Ib"
test3$type[test3$cell %in% type1c] <- "Type Ic"
test3$type[test3$cell %in% type1d] <- "Type Id"
test3$type[test3$cell %in% type2a] <- "Type IIa"
test3$type[test3$cell %in% type2b] <- "Type IIb"
test3$type[test3$cell %in% type2c] <- "Type IIc"
test3$type[test3$cell %in% type2d] <- "Type IId"
test3 <- test3[order,]

temp <- test3[, c(-1, -472)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test3[, c(-1, -472)] <- temp

png("dF z test3.png", height = 1024, width = 1024)
Heatmap(test3[,c(-1, -472)],col = col, name = "Test 3",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test3$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# load z score files of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone test4.csv"))
    temp$cell <- temp$cell+100*i
    test4 <- rbind(test4, temp)
}

test4 <- test4 %>%
    mutate(block = NULL) %>%
    spread(key = "time", value = "z") %>%
    mutate(type = NA)

test4$type[test4$cell %in% type1a] <- "Type Ia"
test4$type[test4$cell %in% type1b] <- "Type Ib"
test4$type[test4$cell %in% type1c] <- "Type Ic"
test4$type[test4$cell %in% type1d] <- "Type Id"
test4$type[test4$cell %in% type2a] <- "Type IIa"
test4$type[test4$cell %in% type2b] <- "Type IIb"
test4$type[test4$cell %in% type2c] <- "Type IIc"
test4$type[test4$cell %in% type2d] <- "Type IId"
test4 <- test4[order,]

temp <- test4[, c(-1, -472)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test4[, c(-1, -472)] <- temp

png("dF z test4.png", height = 1024, width = 1024)
Heatmap(test4[,c(-1, -472)],col = col, name = "Test 4",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test4$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()



# process dF/F data into Z scores by one sec
source("dF.all.z.tone.1sec.R")
source("dF.z.shock.1sec.R")

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# calculate Z scores of tone for 5 mice
for(i in 1:length(allfolders)){
    dF.all.z.tone.1sec(allfolders[i])
}

# calculate Z scores of shock for 5 mice
for(i in 1:length(allfolders)){
    dF.z.shock.1sec(allfolders[i])
}

# read Z score of tone files of 5 mice
tone.training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec training.csv"))
    temp$cell <- temp$cell+100*i
    tone.training <- rbind(tone.training, temp)
}

# read Z score of shock files of 5 mice
shock.training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z shock 1sec.csv"))
    temp$cell <- temp$cell+100*i
    shock.training <- rbind(shock.training, temp)
}

# summarise maximum of z score of shock for each cell
shock.training.max<- shock.training %>%
    filter(block > 54) %>%
    group_by(cell) %>%
    summarise(max=max(z))
shock.training.max$cell <- as.character(shock.training.max$cell)
colnames(shock.training.max) <- c("cell", "Training-Shock")

type1 <- as.character(shock.training.max$cell[shock.training.max$max >= 3])
type2 <- as.character(shock.training.max$cell[shock.training.max$max < 3 &
                                                  shock.training.max$max > -3])
type3 <- as.character(shock.training.max$cell[shock.training.max$max <= -3])

training <- tone.training %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)
training <- training %>%
    mutate(mean = rowMeans(training[, 7:21])) %>%
    arrange(desc(mean))
training$type[training$cell %in% type1] <- "Type I"
training$type[training$cell %in% type2] <- "Type II"
training$type[training$cell %in% type3] <- "Type III"
training$cell <- as.character(training$cell)
order <- training$cell

temp <- training[, c(-1, -52, -53)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
training[, c(-1, -52, -53)] <- temp

col <- colorRamp2(c(1, 0, -1), brewer.pal(3, "RdBu"))
timeticks <- c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 50)
timelabels <- c("5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55")
ha_bottom <- columnAnnotation(link=column_anno_link(at=timeticks, labels=timelabels, side="bottom"),
                              gp=gpar(fontsize=14))
png("dF z 1sec training.png", height = 1024, width = 1024)
Heatmap(training[,c(-1, -52, -53)],col = col, name = "Training",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time (sec)", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = training$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# read Z score files of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec test1.csv"))
    temp$cell <- temp$cell+100*i
    test1 <- rbind(test1, temp)
}

test1 <- test1 %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)
test1$type[test1$cell %in% type1] <- "Type I"
test1$type[test1$cell %in% type2] <- "Type II"
test1$type[test1$cell %in% type3] <- "Type III"
test1$cell <- as.character(test1$cell)
test1 <- test1[match(order, test1$cell),]

temp <- test1[, c(-1, -42)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test1[, c(-1, -42)] <- temp

timeticks <- c(1, 6, 11, 16, 21, 26, 31, 36, 40)
timelabels <- c("5", "10", "15", "20", "25", "30", "35", "40", "45")
ha_bottom <- columnAnnotation(link=column_anno_link(at=timeticks, labels=timelabels, side="bottom"),
                              gp=gpar(fontsize=14))

png("dF z 1sec test1.png", height = 1024, width = 1024)
Heatmap(test1[,c(-1, -42)],col = col, name = "Test 1",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test1$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# read Z score files of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec test2.csv"))
    temp$cell <- temp$cell+100*i
    test2 <- rbind(test2, temp)
}

test2 <- test2 %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)
test2$type[test2$cell %in% type1] <- "Type I"
test2$type[test2$cell %in% type2] <- "Type II"
test2$type[test2$cell %in% type3] <- "Type III"
test2$cell <- as.character(test2$cell)
test2 <- test2[match(order, test2$cell),]

temp <- test2[, c(-1, -42)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test2[, c(-1, -42)] <- temp

png("dF z 1sec test2.png", height = 1024, width = 1024)
Heatmap(test2[,c(-1, -42)],col = col, name = "Test 2",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test2$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# load z score files of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec test3.csv"))
    temp$cell <- temp$cell+100*i
    test3 <- rbind(test3, temp)
}

test3 <- test3 %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)

test3$type[test3$cell %in% type1] <- "Type I"
test3$type[test3$cell %in% type2] <- "Type II"
test3$type[test3$cell %in% type3] <- "Type III"
test3$cell <- as.character(test3$cell)
test3 <- test3[match(order, test3$cell),]

temp <- test3[, c(-1, -42)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test3[, c(-1, -42)] <- temp

png("dF z 1sec test3.png", height = 1024, width = 1024)
Heatmap(test3[,c(-1, -42)],col = col, name = "Test 3",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test3$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# load z score files of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec test4.csv"))
    temp$cell <- temp$cell+100*i
    test4 <- rbind(test4, temp)
}

test4 <- test4 %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)

test4$type[test4$cell %in% type1] <- "Type I"
test4$type[test4$cell %in% type2] <- "Type II"
test4$type[test4$cell %in% type3] <- "Type III"
test4$cell <- as.character(test4$cell)
test4 <- test4[match(order, test4$cell),]

temp <- test4[, c(-1, -42)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test4[, c(-1, -42)] <- temp

png("dF z 1sec test4.png", height = 1024, width = 1024)
Heatmap(test4[,c(-1, -42)],col = col, name = "Test 4",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test4$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# process dF/F data into Z scores by one sec
source("dF.all.z.tone.1sec.cusum.R")
#source("dF.z.shock.1sec.R")

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# calculate Z scores of tone for 5 mice
for(i in 1:length(allfolders)){
    dF.all.z.tone.1sec.cusum(allfolders[i])
}

# read Z score of tone files of 5 mice
tone.training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec cusum training.csv"))
    temp$cell <- temp$cell+100*i
    tone.training <- rbind(tone.training, temp)
}

# read Z score of shock files of 5 mice
shock.training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z shock.csv"))
    temp$cell <- temp$cell+100*i
    shock.training <- rbind(shock.training, temp)
}

# summarise maximum of z score of shock for each cell and each block
shock.training.max<- shock.training %>%
    group_by(cell, block) %>%
    summarise(max=max(z)) %>%
    spread(key = "block", value = "max")
shock.training.max$cell <- as.character(shock.training.max$cell)
shock.training.max$before <- NULL
colnames(shock.training.max) <- c("cell", "max")

type1 <- as.character(shock.training.max$cell[shock.training.max$max >= 3])
type2 <- as.character(shock.training.max$cell[shock.training.max$max < 3 &
                                                  shock.training.max$max > -3])
type3 <- as.character(shock.training.max$cell[shock.training.max$max <= -3])

training <- tone.training %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)
training <- training %>%
    mutate(mean = rowMeans(training[, 7:21])) %>%
    arrange(desc(mean))
training$type[training$cell %in% type1] <- "Type I"
training$type[training$cell %in% type2] <- "Type II"
training$type[training$cell %in% type3] <- "Type III"
training$cell <- as.character(training$cell)
order <- training$cell

temp <- training[, c(-1, -52, -53)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
training[, c(-1, -52, -53)] <- temp

col <- colorRamp2(c(1, 0, -1), brewer.pal(3, "RdBu"))
timeticks <- c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 50)
timelabels <- c("5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55")
ha_bottom <- columnAnnotation(link=column_anno_link(at=timeticks, labels=timelabels, side="bottom"),
                              gp=gpar(fontsize=14))
png("dF z 1sec cusum training.png", height = 1024, width = 1024)
Heatmap(training[,c(-1, -52, -53)],col = col, name = "Training",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time (sec)", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = training$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# read Z score files of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec cusum test1.csv"))
    temp$cell <- temp$cell+100*i
    test1 <- rbind(test1, temp)
}

test1 <- test1 %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)
test1$type[test1$cell %in% type1] <- "Type I"
test1$type[test1$cell %in% type2] <- "Type II"
test1$type[test1$cell %in% type3] <- "Type III"
test1$cell <- as.character(test1$cell)
test1 <- test1[match(order, test1$cell),]

temp <- test1[, c(-1, -42)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test1[, c(-1, -42)] <- temp

timeticks <- c(1, 6, 11, 16, 21, 26, 31, 36, 40)
timelabels <- c("5", "10", "15", "20", "25", "30", "35", "40", "45")
ha_bottom <- columnAnnotation(link=column_anno_link(at=timeticks, labels=timelabels, side="bottom"),
                              gp=gpar(fontsize=14))

png("dF z 1sec cusum test1.png", height = 1024, width = 1024)
Heatmap(test1[,c(-1, -42)],col = col, name = "Test 1",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test1$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# read Z score files of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec cusum test2.csv"))
    temp$cell <- temp$cell+100*i
    test2 <- rbind(test2, temp)
}

test2 <- test2 %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)
test2$type[test2$cell %in% type1] <- "Type I"
test2$type[test2$cell %in% type2] <- "Type II"
test2$type[test2$cell %in% type3] <- "Type III"
test2$cell <- as.character(test2$cell)
test2 <- test2[match(order, test2$cell),]

temp <- test2[, c(-1, -42)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test2[, c(-1, -42)] <- temp

png("dF z 1sec cusum test2.png", height = 1024, width = 1024)
Heatmap(test2[,c(-1, -42)],col = col, name = "Test 2",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test2$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# load z score files of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec cusum test3.csv"))
    temp$cell <- temp$cell+100*i
    test3 <- rbind(test3, temp)
}

test3 <- test3 %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)

test3$type[test3$cell %in% type1] <- "Type I"
test3$type[test3$cell %in% type2] <- "Type II"
test3$type[test3$cell %in% type3] <- "Type III"
test3$cell <- as.character(test3$cell)
test3 <- test3[match(order, test3$cell),]

temp <- test3[, c(-1, -42)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test3[, c(-1, -42)] <- temp

png("dF z 1sec cusum test3.png", height = 1024, width = 1024)
Heatmap(test3[,c(-1, -42)],col = col, name = "Test 3",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test3$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()

# load z score files of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z tone 1sec cusum test4.csv"))
    temp$cell <- temp$cell+100*i
    test4 <- rbind(test4, temp)
}

test4 <- test4 %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)

test4$type[test4$cell %in% type1] <- "Type I"
test4$type[test4$cell %in% type2] <- "Type II"
test4$type[test4$cell %in% type3] <- "Type III"
test4$cell <- as.character(test4$cell)
test4 <- test4[match(order, test4$cell),]

temp <- test4[, c(-1, -42)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test4[, c(-1, -42)] <- temp

png("dF z 1sec cusum test4.png", height = 1024, width = 1024)
Heatmap(test4[,c(-1, -42)],col = col, name = "Test 4",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 12),
        split = test4$type, gap = unit(3, "mm"),
        bottom_annotation = ha_bottom,
        heatmap_legend_param = list(title_gp=gpar(fontsize=12),
                                    labels_gp=gpar(fontsize=12), at=c(-1,0,1)))
dev.off()