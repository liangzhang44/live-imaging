library(tidyverse)
library(readxl)
library(RColorBrewer)
library(circlize)
library(ggpubr)
library(ComplexHeatmap)
source("cusum.z.tone555.R")

# process all trace fear extinction folders for 5 mice
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]
for(i in 1:length(allfolders)){
    cusum.z.tone555(allfolders[i])
}

# load cusum files of 5 mice
all555 <- data.frame()
for(i in 1:length(allfolders)){
    cusum <- read.csv(paste0("./", allfolders[i], "/cusum555 training.csv"))
    cusum$cell <- cusum$cell+100*i
    all555 <- rbind(all555, cusum)
}

train.block <- spread(all555, key = "block", value = "z")
train.block$cell <- as.factor(train.block$cell)
col <- colorRamp2(c(100, 0, -100), brewer.pal(3, "RdBu"))

# read tone data into R
all.tone <- data.frame()
for(i in 1:length(allfolders)){
    tone <- read.csv(paste0("./", allfolders[i], "/z score 515.csv"))
    tone$cell <- tone$cell+100*i
    all.tone <- rbind(all.tone, tone)
}
colnames(all.tone) <- c("Cell", "Training-Tone", "Test 1-Tone", "Test 2-Tone", 
                        "Test 3-Tone", "Test 4-Tone")

# read shock data into R
all.shock <- data.frame()
for(i in 1:length(allfolders)){
    shock <- read.csv(paste0("./", allfolders[i], "/z score 5060.csv"))
    shock$cell <- shock$cell+100*i
    all.shock <- rbind(all.shock, shock)
}
colnames(all.shock) <- c("Cell", "Training-Shock")

# merge shock and tone data
all <- merge(all.shock, all.tone, by="Cell")

type1 <- as.factor(all$Cell[all$`Training-Tone` > 3 & all$`Training-Shock` > 3])
type2 <- as.factor(all$Cell[all$`Training-Tone` > 3 & all$`Training-Shock` < 3])
type3 <- as.factor(all$Cell[all$`Training-Tone` < 3 & all$`Training-Shock` > 3])
type4 <- as.factor(all$Cell[all$`Training-Tone` < 3 & all$`Training-Shock` < 3])
train.block$type <- NA
train.block$type[train.block$cell %in% type1] <- "Type I"
train.block$type[train.block$cell %in% type2] <- "Type II"
train.block$type[train.block$cell %in% type3] <- "Type III"
train.block$type[train.block$cell %in% type4] <- "Type IV"

Heatmap(train.block[,(2:11)],col = col, name = "CuSum",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        column_names_gp = gpar(fontsize = 22),
        show_row_names = FALSE, show_column_names = TRUE,
        split = train.block$type, gap = unit(3, "mm"))

train.block.tidy <- gather(train.block, key = "block", value = "z", -cell, -type)
train.block.mean <- train.block.tidy %>%
    group_by(type, block) %>%
    summarise(z=mean(z))
ggplot(train.block.tidy, aes(x=block, y=z))+
    geom_line(aes(group=cell, color=type))+
    geom_line(data = train.block.mean, color="black", group=1, lwd=1.5)+
    geom_hline(yintercept = 0, lty = 2)+
    scale_y_continuous(limits = c(-400, 400))+
    labs(x="Time (sec)", y="Z Score", title="Training", color="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40",
                              "45", "50", "55"))+
    annotate("rect", xmin = 1, xmax = 4, ymin = -400, ymax = 400, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("training line plot.png")

ggplot(train.block.tidy, aes(x=block, y=z))+
    geom_area(aes(group=cell, fill=type), color="black", lwd=0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    scale_y_continuous(limits = c(-2000, 2000))+
    labs(x="Time (sec)", y="Z Score", title="Training", fill="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40",
                              "45", "50", "55"))+
    annotate("rect", xmin = 1, xmax = 4, ymin = -2000, ymax = 2000, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("training area plot.png")

# load cusum files of 5 mice
test1.block <- data.frame()
for(i in 1:length(allfolders)){
    cusum <- read.csv(paste0("./", allfolders[i], "/cusum545 test1.csv"))
    cusum$cell <- cusum$cell+100*i
    test1.block <- rbind(test1.block, cusum)
}

test1.block <- spread(test1.block, key = "block", value = "z")
test1.block$cell <- as.factor(test1.block$cell)
col <- colorRamp2(c(50, 0, -50), brewer.pal(3, "RdBu"))

test1.block$type <- NA
test1.block$type[test1.block$cell %in% type1] <- "Type I"
test1.block$type[test1.block$cell %in% type2] <- "Type II"
test1.block$type[test1.block$cell %in% type3] <- "Type III"
test1.block$type[test1.block$cell %in% type4] <- "Type IV"

Heatmap(test1.block[,(2:9)],col = col, name = "CuSum",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        column_names_gp = gpar(fontsize = 22),
        show_row_names = FALSE, show_column_names = TRUE,
        split = test1.block$type, gap = unit(3, "mm"))

test1.block.tidy <- gather(test1.block, key = "block", value = "z", -cell, -type)
test1.block.mean <- test1.block.tidy %>%
    group_by(type, block) %>%
    summarise(z=mean(z))
ggplot(test1.block.tidy, aes(x=block, y=z))+
    geom_line(aes(group=cell, color=type), lwd=0.5)+
    geom_line(data = test1.block.mean, color="black", group=1, lwd=1.5)+
    geom_hline(yintercept = 0, lty = 2, lwd = 1.5)+
    scale_y_continuous(limits = c(-200, 200))+
    labs(x="Time (sec)", y="Z Score", title="Test 1", color="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40", "45"))+
    annotate("rect", xmin = 1, xmax = 2, ymin = -200, ymax = 200, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("test 1 line plot.png")

ggplot(test1.block.tidy, aes(x=block, y=z))+
    geom_area(aes(group=cell, fill=type), color="black", lwd=0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    scale_y_continuous(limits = c(-1000, 1000))+
    labs(x="Time (sec)", y="Z Score", title="Test 1", fill="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40",
                              "45", "50", "55"))+
    annotate("rect", xmin = 1, xmax = 2, ymin = -1000, ymax = 1000, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("test 1 area plot.png")

# load cusum files of 5 mice
test2.block <- data.frame()
for(i in 1:length(allfolders)){
    cusum <- read.csv(paste0("./", allfolders[i], "/cusum545 test2.csv"))
    cusum$cell <- cusum$cell+100*i
    test2.block <- rbind(test2.block, cusum)
}

test2.block <- spread(test2.block, key = "block", value = "z")
test2.block$cell <- as.factor(test2.block$cell)
col <- colorRamp2(c(100, 0, -100), brewer.pal(3, "RdBu"))

test2.block$type <- NA
test2.block$type[test2.block$cell %in% type1] <- "Type I"
test2.block$type[test2.block$cell %in% type2] <- "Type II"
test2.block$type[test2.block$cell %in% type3] <- "Type III"
test2.block$type[test2.block$cell %in% type4] <- "Type IV"

Heatmap(test2.block[,(2:9)],col = col, name = "CuSum",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        column_names_gp = gpar(fontsize = 22),
        show_row_names = FALSE, show_column_names = TRUE,
        split = test2.block$type, gap = unit(3, "mm"))

test2.block.tidy <- gather(test2.block, key = "block", value = "z", -cell, -type)
test2.block.mean <- test2.block.tidy %>%
    group_by(type, block) %>%
    summarise(z=mean(z))
ggplot(test2.block.tidy, aes(x=block, y=z))+
    geom_line(aes(group=cell, color=type), lwd=0.5)+
    geom_line(data = test2.block.mean, color="black", group=1, lwd=1.5)+
    geom_hline(yintercept = 0, lty = 2, lwd = 1.5)+
    scale_y_continuous(limits = c(-200, 200))+
    labs(x="Time (sec)", y="Z Score", title="Test 2", color="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40", "45"))+
    annotate("rect", xmin = 1, xmax = 2, ymin = -200, ymax = 200, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("test 2 line plot.png")

ggplot(test2.block.tidy, aes(x=block, y=z))+
    geom_area(aes(group=cell, fill=type), color="black", lwd=0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    scale_y_continuous(limits = c(-1000, 1000))+
    labs(x="Time (sec)", y="Z Score", title="Test 2", fill="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40",
                              "45", "50", "55"))+
    annotate("rect", xmin = 1, xmax = 2, ymin = -1000, ymax = 1000, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("test 2 area plot.png")

test3.block <- data.frame()
for(i in 1:length(allfolders)){
    cusum <- read.csv(paste0("./", allfolders[i], "/cusum545 test3.csv"))
    cusum$cell <- cusum$cell+100*i
    test3.block <- rbind(test3.block, cusum)
}

test3.block <- spread(test3.block, key = "block", value = "z")
test3.block$cell <- as.factor(test3.block$cell)
col <- colorRamp2(c(100, 0, -100), brewer.pal(3, "RdBu"))

test3.block$type <- NA
test3.block$type[test3.block$cell %in% type1] <- "Type I"
test3.block$type[test3.block$cell %in% type2] <- "Type II"
test3.block$type[test3.block$cell %in% type3] <- "Type III"
test3.block$type[test3.block$cell %in% type4] <- "Type IV"

Heatmap(test3.block[,(2:9)],col = col, name = "CuSum",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        column_names_gp = gpar(fontsize = 22),
        show_row_names = FALSE, show_column_names = TRUE,
        split = test3.block$type, gap = unit(3, "mm"))

test3.block.tidy <- gather(test3.block, key = "block", value = "z", -cell, -type)
test3.block.mean <- test3.block.tidy %>%
    group_by(type, block) %>%
    summarise(z=mean(z))
ggplot(test3.block.tidy, aes(x=block, y=z))+
    geom_line(aes(group=cell, color=type), lwd=0.5)+
    geom_line(data = test3.block.mean, color="black", group=1, lwd=1.5)+
    geom_hline(yintercept = 0, lty = 2, lwd = 1.5)+
    scale_y_continuous(limits = c(-200, 200))+
    labs(x="Time (sec)", y="Z Score", title="Test 3", color="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40", "45"))+
    annotate("rect", xmin = 1, xmax = 2, ymin = -200, ymax = 200, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("test 3 line plot.png")

ggplot(test3.block.tidy, aes(x=block, y=z))+
    geom_area(aes(group=cell, fill=type), color="black", lwd=0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    scale_y_continuous(limits = c(-1000, 1000))+
    labs(x="Time (sec)", y="Z Score", title="Test 3", fill="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40",
                              "45", "50", "55"))+
    annotate("rect", xmin = 1, xmax = 2, ymin = -1000, ymax = 1000, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("test 3 area plot.png")

test4.block <- data.frame()
for(i in 1:length(allfolders)){
    cusum <- read.csv(paste0("./", allfolders[i], "/cusum545 test4.csv"))
    cusum$cell <- cusum$cell+100*i
    test4.block <- rbind(test4.block, cusum)
}

test4.block <- spread(test4.block, key = "block", value = "z")
test4.block$cell <- as.factor(test4.block$cell)
col <- colorRamp2(c(100, 0, -100), brewer.pal(3, "RdBu"))

test4.block$type <- NA
test4.block$type[test4.block$cell %in% type1] <- "Type I"
test4.block$type[test4.block$cell %in% type2] <- "Type II"
test4.block$type[test4.block$cell %in% type3] <- "Type III"
test4.block$type[test4.block$cell %in% type4] <- "Type IV"

Heatmap(test4.block[,(2:9)],col = col, name = "CuSum",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        column_names_gp = gpar(fontsize = 22),
        show_row_names = FALSE, show_column_names = TRUE,
        split = test4.block$type, gap = unit(3, "mm"))

test4.block.tidy <- gather(test4.block, key = "block", value = "z", -cell, -type)
test4.block.mean <- test4.block.tidy %>%
    group_by(type, block) %>%
    summarise(z=mean(z))
ggplot(test4.block.tidy, aes(x=block, y=z))+
    geom_line(aes(group=cell, color=type), lwd=0.5)+
    geom_line(data = test4.block.mean, color="black", group=1, lwd=1.5)+
    geom_hline(yintercept = 0, lty = 2, lwd = 1.5)+
    scale_y_continuous(limits = c(-200, 200))+
    labs(x="Time (sec)", y="Z Score", title="Test 4", color="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40", "45"))+
    annotate("rect", xmin = 1, xmax = 2, ymin = -200, ymax = 200, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("test 4 line plot.png")

ggplot(test4.block.tidy, aes(x=block, y=z))+
    geom_area(aes(group=cell, fill=type), color="black", lwd=0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    scale_y_continuous(limits = c(-1000, 1000))+
    labs(x="Time (sec)", y="Z Score", title="Test 4", fill="")+
    scale_x_discrete(labels=c("10", "15", "20", "25", "30", "35", "40",
                              "45", "50", "55"))+
    annotate("rect", xmin = 1, xmax = 2, ymin = -1000, ymax = 1000, alpha = .2, fill="blue")+
    facet_grid(type~.)+
    theme_pubr()
ggsave("test 4 area plot.png")

for(i in 1:length(allfolders)){
    cusum.all.phase(allfolders[i])
}

# load cusum files of 5 mice
all.cusum <- data.frame()
for(i in 1:length(allfolders)){
    cusum <- read.csv(paste0("./", allfolders[i], "/cusum training.csv"))
    cusum$cell <- cusum$cell+100*i
    all.cusum <- rbind(all.cusum, cusum)
}

all.time <- all.cusum %>%
    gather(key = "cycle", value = "residual", -time, -cell) %>%
    spread(key = "time", value = "residual")
all.time$cell <- as.factor(all.time$cell)
col <- colorRamp2(c(1000, 0, -1000), brewer.pal(3, "RdBu"))
cells <- as.factor(all$Cell[all$`Training-Tone` > 3 & all$`Training-Shock` > 3])
all.time.type1 <- all.time[all.time$cell %in% cells,]
Heatmap(all.time.type1[,-(1:2)],col = col, name = "CuSum",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        column_names_gp = gpar(fontsize = 22),
        show_row_names = FALSE, show_column_names = FALSE,
        split = all.time.type1$cycle, gap = unit(3, "mm"))

cells <- as.factor(all$Cell[all$`Training-Tone` > 3 & all$`Training-Shock` < 3])
all.time.type2 <- all.time[all.time$cell %in% cells,]
Heatmap(all.time.type2[,-(1:2)],col = col, name = "CuSum",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        column_names_gp = gpar(fontsize = 22),
        show_row_names = FALSE, show_column_names = FALSE,
        split = all.time.type2$cycle, gap = unit(3, "mm"))

all.cusum$cell <- as.factor(all.cusum$cell)

cells <- c("101", "202", "303", "404", "505")
ggplot(all.cusum[all.cusum$cell %in% cells[1:3],], aes(x=time, y=cell, fill=residual))+
    geom_tile()+
    facet_grid(cycle~.)+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white")
    #annotate("rect", xmin = 10, xmax = 25, ymin = 0, ymax = 80, alpha = .2, fill="blue")+
    #annotate("rect", xmin = 55, xmax = 56, ymin = 0, ymax = 80, alpha = .2, fill="red")
ggsave("allcusum.png")

# process all trace fear extinction folders with control cycles for 5 mice
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]
for(i in 1:length(allfolders)){
    cusum.z.control(allfolders[i])
}

# load z score files of 5 mice
z.training <- data.frame()
for(i in 1:length(allfolders)){
    training <- read.csv(paste0("./", allfolders[i], "/z training cycles.csv"))
    training$cell <- training$cell+100*i
    z.training <- rbind(z.training, tone)
}
d_temp$cell <- as.factor(d_temp$cell)
ggplot(d_temp, aes(x=time, y=cell, fill=z))+
    geom_tile()+
    scale_fill_gradient2(high = "red", low = "blue")
