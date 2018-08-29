library(tidyverse)
library(readxl)
library(RColorBrewer)
library(circlize)
library(ggpubr)
library(ComplexHeatmap)

source("dF.R")

# calculate dF/F for 5 mice
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]
for(i in 1:length(allfolders)){
    dF(allfolders[i])
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



# process dF/F data into Z scores by one sec
source("dF.all.z.1sec.R")

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# calculate Z scores of tone for 5 mice
for(i in 1:length(allfolders)){
    dF.all.z.1sec(allfolders[i])
}

# read Z score of tone files of 5 mice
z.training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec training all.csv"))
    temp$cell <- temp$cell+100*i
    z.training <- rbind(z.training, temp)
}

shock <- z.training %>%
    filter(block >= 55) %>%
    group_by(cell) %>%
    summarise(max = max(z), min = min(z))
shock$z <- ifelse(abs(shock$max)>abs(shock$min), shock$max, shock$min)

type1 <- as.character(shock$cell[shock$z >= 3])
type2 <- as.character(shock$cell[shock$z < 3 & shock$z > -3])
type3 <- as.character(shock$cell[shock$z <= -3])

training <- z.training %>%
    spread(key = "block", value = "z") %>%
    mutate(type = NA)
training$type[training$cell %in% type1] <- "Type I"
training$type[training$cell %in% type2] <- "Type II"
training$type[training$cell %in% type3] <- "Type III"
training$cell <- as.character(training$cell)

temp <- training[, c(-1, -62)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
training[, c(-1, -62)] <- temp
training <- training %>%
    mutate(tone.sum = rowSums(training[, 12:26]), trace.sum = rowSums(training[, 27:56])) %>%
    arrange(desc(tone.sum), desc(trace.sum))
order <- training$cell

col <- colorRamp2(c(1, 0, -1), brewer.pal(3, "RdBu"))
png("z 1sec training.png", height = 1024, width = 1024)
hm1 <- Heatmap(training[,12:26],col = col, name = "Training",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = training$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 6, 11, 15), labels=c("10", "15", "20", "25"), 
                                         side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                    labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm2 <- Heatmap(training[,27:56],col = col, name = "Training",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = training$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 6, 11, 16, 21, 26, 30), labels=c("26", "30", "35", "40", "45", "50", "55"), 
                                         side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm3 <- Heatmap(training[,57:61],col = col, name = "Training",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Shock", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = training$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 5), labels=c("56", "60"), 
                                         side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm1+hm2+hm3
dev.off()



# process dF/F data into Z scores by one sec
source("dF.all.z.R")

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# calculate Z scores of tone for 5 mice
for(i in 1:length(allfolders)){
    dF.all.z(allfolders[i])
}

# read Z score of tone files of 5 mice
z.training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z training all.csv"))
    temp$cell <- temp$cell+100*i
    z.training <- rbind(z.training, temp)
}

shock <- z.training %>%
    filter(time > 55, time < 60) %>%
    group_by(cell) %>%
    summarise(max = max(z), min = min(z))
shock$z <- ifelse(abs(shock$max)>abs(shock$min), shock$max, shock$min)

type1 <- as.character(shock$cell[shock$z >= 3])
type2 <- as.character(shock$cell[shock$z < 3 & shock$z > -3])
type3 <- as.character(shock$cell[shock$z <= -3])

training <- z.training %>%
    spread(key = "time", value = "z") %>%
    mutate(type = NA)
training$type[training$cell %in% type1] <- "Type I"
training$type[training$cell %in% type2] <- "Type II"
training$type[training$cell %in% type3] <- "Type III"
training$cell <- as.character(training$cell)

temp <- training[, c(-1, -765)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
training[, c(-1, -765)] <- temp
training <- training %>%
    mutate(tone.sum = rowSums(training[, 120:295]), trace.sum = rowSums(training[, 296:647])) %>%
    arrange(desc(tone.sum), desc(trace.sum))

col <- colorRamp2(c(1, 0, -1), brewer.pal(3, "RdBu"))
png("z 1sec training.png", height = 1024, width = 1024)
hm1 <- Heatmap(training[,120:295],col = col, name = "Training",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = training$type, gap = unit(3, "mm"),
               #bottom_annotation = columnAnnotation(
                   #link=column_anno_link(at=c(1, 6, 11, 15), labels=c("10", "15", "20", "25"), 
                                         #side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm2 <- Heatmap(training[,296:647],col = col, name = "Training",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = training$type, gap = unit(3, "mm"),
               #bottom_annotation = columnAnnotation(
                #   link=column_anno_link(at=c(1, 6, 11, 16, 21, 26, 30), labels=c("26", "30", "35", "40", "45", "50", "55"), 
                 #                        side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm3 <- Heatmap(training[,648:706],col = col, name = "Training",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Shock", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = training$type, gap = unit(3, "mm"),
               #bottom_annotation = columnAnnotation(
                #   link=column_anno_link(at=c(1, 5), labels=c("56", "60"), 
                 #                        side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm1+hm2+hm3
dev.off()

# read Z score files of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test1 all.csv"))
    temp$cell <- temp$cell+100*i
    test1 <- rbind(test1, temp)
}

test1 <- test1 %>%
    spread(key = "time", value = "z") %>%
    mutate(type = NA)
test1$type[test1$cell %in% type1] <- "Type I"
test1$type[test1$cell %in% type2] <- "Type II"
test1$type[test1$cell %in% type3] <- "Type III"
test1$cell <- as.character(test1$cell)
test1 <- test1[match(order, test1$cell),]

temp <- test1[, c(-1, -554)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test1[, c(-1, -554)] <- temp

png("z 1sec test1.png", height = 1024, width = 1024)
hm1 <- Heatmap(test1[,120:177],col = col, name = "Test 1",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = test1$type, gap = unit(3, "mm"),
               #bottom_annotation = columnAnnotation(
                #   link=column_anno_link(at= c(1, 5), labels=c("10", "15"), 
                 #                        side="bottom"), gp=gpar(fontsize=18)),
        heatmap_legend_param = list(title_gp=gpar(fontsize=18),
                                    labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm2 <- Heatmap(test1[,178:553],col = col, name = "Test 1",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = test1$type, gap = unit(3, "mm"),
               #bottom_annotation = columnAnnotation(
                #   link=column_anno_link(at= c(1, 6, 11, 16, 21, 26, 30), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                 #                        side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18),
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm1+hm2
dev.off()

# read Z score files of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test2 all.csv"))
    temp$cell <- temp$cell+100*i
    test2 <- rbind(test2, temp)
}

test2 <- test2 %>%
    spread(key = "time", value = "z") %>%
    mutate(type = NA)
test2$type[test2$cell %in% type1] <- "Type I"
test2$type[test2$cell %in% type2] <- "Type II"
test2$type[test2$cell %in% type3] <- "Type III"
test2$cell <- as.character(test2$cell)
test2 <- test2[match(order, test2$cell),]

temp <- test2[, c(-1, -554)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test2[, c(-1, -554)] <- temp

png("z 1sec test2.png", height = 1024, width = 1024)
hm1 <- Heatmap(test2[,120:177],col = col, name = "Test 2",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = test2$type, gap = unit(3, "mm"),
               #bottom_annotation = columnAnnotation(
                #   link=column_anno_link(at= c(1, 5), labels=c("10", "15"), 
                 #                        side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18),
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm2 <- Heatmap(test2[,178:553],col = col, name = "Test 2",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = test2$type, gap = unit(3, "mm"),
               #bottom_annotation = columnAnnotation(
                #   link=column_anno_link(at= c(1, 6, 11, 16, 21, 26, 30), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                 #                        side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18),
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm1+hm2
dev.off()

# load z score files of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test3 all.csv"))
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

temp <- test3[, c(-1, -47)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test3[, c(-1, -47)] <- temp

png("z 1sec test3.png", height = 1024, width = 1024)
hm1 <- Heatmap(test3[,12:16],col = col, name = "Test 3",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = test1$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at= c(1, 5), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18),
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm2 <- Heatmap(test3[,17:46],col = col, name = "Test 3",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = test1$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at= c(1, 6, 11, 16, 21, 26, 30), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18),
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm1+hm2
dev.off()

# load z score files of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test4 all.csv"))
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

temp <- test4[, c(-1, -47)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
test4[, c(-1, -47)] <- temp

png("z 1sec test4.png", height = 1024, width = 1024)
hm1 <- Heatmap(test4[,12:16],col = col, name = "Test 4",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = test1$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at= c(1, 5), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18),
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm2 <- Heatmap(test4[,17:46],col = col, name = "Test 4",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = test1$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at= c(1, 6, 11, 16, 21, 26, 30), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18),
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm1+hm2
dev.off()

training.tone <- training %>%
    select(cell, `10`:`24`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "training", period = "tone", pos = sum(z == 1)/15*100, neg = sum(z == -1)/15*100)
training.trace <- training %>%
    select(cell, `25`:`54`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "training", period = "trace", pos = sum(z == 1)/30*100, neg = sum(z == -1)/30*100)

test1.tone <- test1 %>%
    select(cell, `10`:`14`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "test1", period = "tone", pos = sum(z == 1)/5*100, neg = sum(z == -1)/5*100)
test1.trace <- test1 %>%
    select(cell, `15`:`44`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "test1", period = "trace", pos = sum(z == 1)/30*100, neg = sum(z == -1)/30*100)

test2.tone <- test2 %>%
    select(cell, `10`:`14`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "test2", period = "tone", pos = sum(z == 1)/5*100, neg = sum(z == -1)/5*100)
test2.trace <- test2 %>%
    select(cell, `15`:`44`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "test2", period = "trace", pos = sum(z == 1)/30*100, neg = sum(z == -1)/30*100)

test3.tone <- test3 %>%
    select(cell, `10`:`14`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "test3", period = "tone", pos = sum(z == 1)/5*100, neg = sum(z == -1)/5*100)
test3.trace <- test3 %>%
    select(cell, `15`:`44`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "test3", period = "trace", pos = sum(z == 1)/30*100, neg = sum(z == -1)/30*100)

test4.tone <- test4 %>%
    select(cell, `10`:`14`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "test4", period = "tone", pos = sum(z == 1)/5*100, neg = sum(z == -1)/5*100)
test4.trace <- test4 %>%
    select(cell, `15`:`44`) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(phase = "test4", period = "trace", pos = sum(z == 1)/30*100, neg = sum(z == -1)/30*100)

perc <- rbind(training.tone, training.trace, test1.tone, test1.trace, test2.tone, test2.trace,
                  test3.tone, test3.trace, test4.tone, test4.trace)
perc$type <- NA
perc$type[perc$cell %in% type1] <- "Type I"
perc$type[perc$cell %in% type2] <- "Type II"
perc$type[perc$cell %in% type3] <- "Type III"

ggplot(perc[perc$type=="Type I",], aes(phase, pos))+
    geom_line(aes(group = period, color = period))+
    scale_x_discrete(limits = c("training", "test1", "test2", "test3", "test4"))+
    scale_y_continuous(limits = c(0,100))+
    facet_wrap(~cell, ncol = 4)+
    theme(legend.title = element_blank())+
    ggtitle("Type I")
ggsave("lineplot type I.png", height = 6, width = 8)

ggplot(perc[perc$type=="Type II",], aes(phase, pos))+
    geom_line(aes(group = period, color = period))+
    scale_x_discrete(limits = c("training", "test1", "test2", "test3", "test4"))+
    scale_y_continuous(limits = c(0,100))+
    facet_wrap(~cell, ncol = 5)+
    theme(legend.title = element_blank())+
    ggtitle("Type II")
ggsave("lineplot type II.png", height = 14, width = 10)

ggplot(perc[perc$type=="Type III",], aes(phase, pos))+
    geom_line(aes(group = period, color = period))+
    scale_x_discrete(limits = c("training", "test1", "test2", "test3", "test4"))+
    scale_y_continuous(limits = c(0,100))+
    facet_wrap(~cell, ncol = 5)+
    theme(legend.title = element_blank())+
    ggtitle("Type III")
ggsave("lineplot type III.png", height = 14, width = 10)
