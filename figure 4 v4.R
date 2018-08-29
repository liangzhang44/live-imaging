library(tidyverse)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(zoo)
library(EBImage)

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# read dF/F data of training, last 2 cycles
late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z training late2.csv"))
    temp <- temp %>%
        mutate(cell = cell + 100*i)
    late2 <- rbind(late2, temp)
}

late2.mean <- late2 %>%
    group_by(time) %>%
    summarise(z = mean(z))
late.tone.mean <- mean(late2.mean$z[late2.mean$time < 10 & late2.mean$time > 5])
late.tone.sd <- sd(late2.mean$z[late2.mean$time < 10 & late2.mean$time > 5])
late2 <- late2 %>%
    mutate(z = (z - late.tone.mean)/late.tone.sd)

late2 <- spread(late2, key = "cell", value = "z")
for(i in 2:ncol(late2)){
    late2[5:(nrow(late2)-4), i] <- rollmean(late2[,i], 9)
}
late2 <- gather(late2, key = "cell", value = "z", -time)

firing <- late2 %>%
    filter(time < 10)
firing$z[firing$z < 15] <- 0
firing$z[firing$z >= 15] <- 1
p <- sum(firing$z == 1)/nrow(firing)

training.late2 <- late2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")

temp <- training.late2[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
training.late2[, -1] <- temp

training.late2 <- training.late2 %>%
    mutate(tone.mean = round(rowMeans(training.late2[, 2:177]), 1), 
           trace.mean = rowMeans(training.late2[, 178:529])) %>%
    arrange(desc(tone.mean), desc(trace.mean))
order <- training.late2$cell
training.late2$type <- ifelse(training.late2$trace.mean > 10*p, "Trace Cells", "Non-Trace Cells")
training.late2$cell <- 1:80
training.late2 <- column_to_rownames(training.late2, var = "cell")


# read z score data of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test1 all.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    test1 <- rbind(test1, temp)
}

test1.mean <- test1 %>%
    group_by(time) %>%
    summarise(z = mean(z))
test1.tone.mean <- mean(test1.mean$z[test1.mean$time < 10 & test1.mean$time > 5])
test1.tone.sd <- sd(test1.mean$z[test1.mean$time < 10 & test1.mean$time > 5])
test1 <- test1 %>%
    mutate(z = (z - test1.tone.mean)/test1.tone.sd)

test1 <- spread(test1, key = "cell", value = "z")
for(i in 2:ncol(test1)){
    test1[5:(nrow(test1)-4), i] <- rollmean(test1[,i], 9)
}
test1 <- gather(test1, key = "cell", value = "z", -time)

test1 <- test1 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

temp <- test1[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
test1[, -1] <- temp
test1 <- test1[match(order, test1$cell),]
test1$type <- training.late2$type
test1 <- filter(test1, type == "Trace Cells")

# figure 4a
col <- colorRamp2(c(1, 0, -1), brewer.pal(3, "RdYlBu"))
png("new figure 4a.png", height = 1200, width = 600)
hm1 <- Heatmap(test1[,2:60],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           at = c(15, 0, -15), labels_gp=gpar(fontsize=18),
                                           grid_height = unit(6, "mm"), grid_width = unit(6, "mm")))
hm2 <- Heatmap(test1[,61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, column_title = "Test 1", column_title_side = "top", column_title_gp = gpar(fontsize = 24))
dev.off()

img4a <- readImage("new figure 4a.png")
f4a <- ggplot()+
    geom_blank()+
    background_image(img4a)

# read z score data of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test2 all.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    test2 <- rbind(test2, temp)
}

test2.mean <- test2 %>%
    group_by(time) %>%
    summarise(z = mean(z))
test2.tone.mean <- mean(test2.mean$z[test2.mean$time < 10 & test2.mean$time > 5])
test2.tone.sd <- sd(test2.mean$z[test2.mean$time < 10 & test2.mean$time > 5])
test2 <- test2 %>%
    mutate(z = (z - test2.tone.mean)/test2.tone.sd)

test2 <- spread(test2, key = "cell", value = "z")
for(i in 2:ncol(test2)){
    test2[5:(nrow(test2)-4), i] <- rollmean(test2[,i], 9)
}
test2 <- gather(test2, key = "cell", value = "z", -time)

test2 <- test2 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

temp <- test2[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
test2[, -1] <- temp
test2 <- test2[match(order, test2$cell),]
test2$type <- training.late2$type
test2 <- filter(test2, type == "Trace Cells")

# figure 4b
png("new figure 4b.png", height = 1200, width = 600)
hm1 <- Heatmap(test2[,2:60],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           at = c(15, 0, -15), labels_gp=gpar(fontsize=18),
                                           grid_height = unit(6, "mm"), grid_width = unit(6, "mm")))
hm2 <- Heatmap(test2[,61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, column_title = "Test 2", column_title_side = "top", column_title_gp = gpar(fontsize = 24))
dev.off()

img4b <- readImage("new figure 4b.png")
f4b <- ggplot()+
    geom_blank()+
    background_image(img4b)

# read z score data of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test3 all.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    test3 <- rbind(test3, temp)
}

test3.mean <- test3 %>%
    group_by(time) %>%
    summarise(z = mean(z))
test3.tone.mean <- mean(test3.mean$z[test3.mean$time < 10 & test3.mean$time > 5])
test3.tone.sd <- sd(test3.mean$z[test3.mean$time < 10 & test3.mean$time > 5])
test3 <- test3 %>%
    mutate(z = (z - test3.tone.mean)/test3.tone.sd)

test3 <- spread(test3, key = "cell", value = "z")
for(i in 2:ncol(test3)){
    test3[5:(nrow(test3)-4), i] <- rollmean(test3[,i], 9)
}
test3 <- gather(test3, key = "cell", value = "z", -time)

test3 <- test3 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")
test3$cell <- as.character(test3$cell)

temp <- test3[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
test3[, -1] <- temp
test3 <- test3[match(order, test3$cell),]
test3$type <- training.late2$type
test3 <- filter(test3, type == "Trace Cells")

# figure 4c
png("new figure 4c.png", height = 1200, width = 600)
hm1 <- Heatmap(test3[,2:60],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24), 
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           at = c(15, 0, -15), labels_gp=gpar(fontsize=18),
                                           grid_height = unit(6, "mm"), grid_width = unit(6, "mm")))
hm2 <- Heatmap(test3[,61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, column_title = "Test 3", column_title_side = "top", column_title_gp = gpar(fontsize = 24))
dev.off()

img4c <- readImage("new figure 4c.png")
f4c <- ggplot()+
    geom_blank()+
    background_image(img4c)

# read z score data of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test4 all.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    test4 <- rbind(test4, temp)
}

test4.mean <- test4 %>%
    group_by(time) %>%
    summarise(z = mean(z))
test4.tone.mean <- mean(test4.mean$z[test4.mean$time < 10 & test4.mean$time > 5])
test4.tone.sd <- sd(test4.mean$z[test4.mean$time < 10 & test4.mean$time > 5])
test4 <- test4 %>%
    mutate(z = (z - test4.tone.mean)/test4.tone.sd)

test4 <- spread(test4, key = "cell", value = "z")
for(i in 2:ncol(test4)){
    test4[5:(nrow(test4)-4), i] <- rollmean(test4[,i], 9)
}
test4 <- gather(test4, key = "cell", value = "z", -time)

test4 <- test4 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")
test4$cell <- as.character(test4$cell)

temp <- test4[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
test4[, -1] <- temp
test4 <- test4[match(order, test4$cell),]
test4$type <- training.late2$type
test4 <- filter(test4, type == "Trace Cells")

# figure 4d
png("new figure 4d.png", height = 1200, width = 600)
hm1 <- Heatmap(test4[,2:60],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           at = c(15, 0, -15), labels_gp=gpar(fontsize=18),
                                           grid_height = unit(6, "mm"), grid_width = unit(6, "mm")))
hm2 <- Heatmap(test4[,61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24), 
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, column_title = "Test 4", column_title_side = "top", column_title_gp = gpar(fontsize = 24))
dev.off()

img4d <- readImage("new figure 4d.png")
f4d <- ggplot()+
    geom_blank()+
    background_image(img4d)

figure4 <- ggarrange(f4a, f4b, f4c, f4d, labels = c("A", "B", "C", "D"), nrow = 1, ncol = 4)
figure4 <- annotate_figure(figure4, fig.lab = "Figure 4", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure4, filename = "new figure 4.png", height = 8, width = 16)
