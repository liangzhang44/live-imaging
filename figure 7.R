library(tidyverse)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(zoo)
library(magick)

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
    arrange(desc(trace.mean), desc(tone.mean))
order <- training.late2$cell
training.late2$type <- ifelse(training.late2$trace.mean > 10*p, "Trace Cells", "Non-Trace Cells")

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

firing <- test1 %>%
    filter(time < 10)
firing$z[firing$z < 15] <- 0
firing$z[firing$z >= 15] <- 1
p <- sum(firing$z == 1)/nrow(firing)

ranking <- test1 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

temp <- ranking[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
ranking[, -1] <- temp

ranking <- ranking %>%
    mutate(tone.mean = round(rowMeans(ranking[, 2:60]), 1), 
           trace.mean = round(rowMeans(ranking[, 61:411]), 1)) %>%
    arrange(desc(trace.mean), desc(tone.mean))
order1 <- ranking$cell
ranking$active <- ifelse(ranking$trace.mean > 10*p, "Active", "Not Active")

test1 <- test1 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

test1 <- test1[match(order, test1$cell),]
test1$type <- training.late2$type

test1 <- test1[match(order1, test1$cell),]
test1$active <- ranking$active
test1 <- filter(test1, type == "Non-Trace Cells")

# figure 4a
pie1 <- test1 %>%
    group_by(active) %>%
    summarise(number = n())
pie1$active <- factor(pie1$active, levels = c("Not Active", "Active"))
f4a1 <- ggplot(pie1, aes(x = factor(1), y = number, fill = active)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start = 0)+
    scale_fill_manual(values = c("grey", "red"))+
    geom_text(aes(y = cumsum(number)-0.5*number, label = number), size = 5)+
    guides(fill = FALSE)+
    ggtitle("Test 1")+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

col <- colorRamp2(c(20, 0, -20), brewer.pal(3, "RdYlBu"))
pdf("new figure 4a2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test1[,2:60],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Non-Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 20),
               split = test1$active, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "continuous",
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(test1[,61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 20),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, heatmap_legend_side = "top")
dev.off()

img4a2 <- image_read_pdf("new figure 4a2.pdf")
f4a2 <- ggplot()+
    geom_blank()+
    background_image(img4a2)

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

firing <- test2 %>%
    filter(time < 10)
firing$z[firing$z < 15] <- 0
firing$z[firing$z >= 15] <- 1
p <- sum(firing$z == 1)/nrow(firing)

ranking <- test2 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

temp <- ranking[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
ranking[, -1] <- temp

ranking <- ranking %>%
    mutate(tone.mean = round(rowMeans(ranking[, 2:60]), 1), 
           trace.mean = round(rowMeans(ranking[, 61:411]), 1)) %>%
    arrange(desc(trace.mean), desc(tone.mean))
order2 <- ranking$cell
ranking$active <- ifelse(ranking$trace.mean > 10*p, "Active", "Not Active")

test2 <- test2 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

test2 <- test2[match(order, test2$cell),]
test2$type <- training.late2$type

test2 <- test2[match(order2, test2$cell),]
test2$active <- ranking$active
test2 <- filter(test2, type == "Non-Trace Cells")

# figure 4b
pie2 <- test2 %>%
    group_by(active) %>%
    summarise(number = n())
pie2$active <- factor(pie2$active, levels = c("Not Active", "Active"))
f4b1 <- ggplot(pie2, aes(x = factor(1), y = number, fill = active)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start = 0)+
    scale_fill_manual(values = c("grey", "red"))+
    geom_text(aes(y = cumsum(number)-0.5*number, label = number), size = 5)+
    guides(fill = FALSE)+
    ggtitle("Test 2")+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

col <- colorRamp2(c(20, 0, -20), brewer.pal(3, "RdYlBu"))
pdf("new figure 4b2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test2[,2:60],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Non-Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 20),
               split = test2$active, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "continuous",
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(test2[,61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 20),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, heatmap_legend_side = "top")
dev.off()

img4b2 <- image_read_pdf("new figure 4b2.pdf")
f4b2 <- ggplot()+
    geom_blank()+
    background_image(img4b2)

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

firing <- test3 %>%
    filter(time < 10)
firing$z[firing$z < 15] <- 0
firing$z[firing$z >= 15] <- 1
p <- sum(firing$z == 1)/nrow(firing)

ranking <- test3 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

temp <- ranking[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
ranking[, -1] <- temp

ranking <- ranking %>%
    mutate(tone.mean = round(rowMeans(ranking[, 2:60]), 1), 
           trace.mean = round(rowMeans(ranking[, 61:411]), 1)) %>%
    arrange(desc(trace.mean), desc(tone.mean))
order3 <- ranking$cell
ranking$active <- ifelse(ranking$trace.mean > 10*p, "Active", "Not Active")

test3 <- test3 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

test3 <- test3[match(order, test3$cell),]
test3$type <- training.late2$type

test3 <- test3[match(order3, test3$cell),]
test3$active <- ranking$active
test3 <- filter(test3, type == "Non-Trace Cells")

# figure 4c
pie3 <- test3 %>%
    group_by(active) %>%
    summarise(number = n())
pie3$active <- factor(pie3$active, levels = c("Not Active", "Active"))
f4c1 <- ggplot(pie3, aes(x = factor(1), y = number, fill = active)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start = 0)+
    scale_fill_manual(values = c("grey", "red"))+
    geom_text(aes(y = cumsum(number)-0.5*number, label = number), size = 5)+
    guides(fill = FALSE)+
    ggtitle("Test 3")+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

col <- colorRamp2(c(20, 0, -20), brewer.pal(3, "RdYlBu"))
pdf("new figure 4c2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test3[,2:60],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Non-Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 20),
               split = test3$active, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "continuous",
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(test3[,61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 20),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, heatmap_legend_side = "top")
dev.off()

img4c2 <- image_read_pdf("new figure 4c2.pdf")
f4c2 <- ggplot()+
    geom_blank()+
    background_image(img4c2)

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

firing <- test4 %>%
    filter(time < 10)
firing$z[firing$z < 15] <- 0
firing$z[firing$z >= 15] <- 1
p <- sum(firing$z == 1)/nrow(firing)

ranking <- test4 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

temp <- ranking[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
ranking[, -1] <- temp

ranking <- ranking %>%
    mutate(tone.mean = round(rowMeans(ranking[, 2:60]), 1), 
           trace.mean = round(rowMeans(ranking[, 61:411]), 1)) %>%
    arrange(desc(trace.mean), desc(tone.mean))
order4 <- ranking$cell
ranking$active <- ifelse(ranking$trace.mean > 10*p, "Active", "Not Active")

test4 <- test4 %>%
    filter(time > 10, time < 45) %>%
    spread(key = "time", value = "z")

test4 <- test4[match(order, test4$cell),]
test4$type <- training.late2$type

test4 <- test4[match(order4, test4$cell),]
test4$active <- ranking$active
test4 <- filter(test4, type == "Non-Trace Cells")

# figure 4d
pie4 <- test4 %>%
    group_by(active) %>%
    summarise(number = n())
pie4$active <- factor(pie4$active, levels = c("Not Active", "Active"))
f4d1 <- ggplot(pie4, aes(x = factor(1), y = number, fill = active)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start = 0)+
    scale_fill_manual(values = c("grey", "red"))+
    geom_text(aes(y = cumsum(number)-0.5*number, label = number), size = 5)+
    guides(fill = FALSE)+
    ggtitle("Test 4")+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

col <- colorRamp2(c(20, 0, -20), brewer.pal(3, "RdYlBu"))
pdf("new figure 4d2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test4[,2:60],col = col, name = "Z Score",
               cluster_columns = FALSE, cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Non-Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 20),
               split = test4$active, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "continuous",
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(test4[,61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 20),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, heatmap_legend_side = "top")
dev.off()

img4d2 <- image_read_pdf("new figure 4d2.pdf")
f4d2 <- ggplot()+
    geom_blank()+
    background_image(img4d2)

f4a <- ggarrange(f4a1, f4a2, ncol = 1, nrow = 2, heights = c(1, 4))
f4b <- ggarrange(f4b1, f4b2, ncol = 1, nrow = 2, heights = c(1, 4))
f4c <- ggarrange(f4c1, f4c2, ncol = 1, nrow = 2, heights = c(1, 4))
f4d <- ggarrange(f4d1, f4d2, ncol = 1, nrow = 2, heights = c(1, 4))
figure7 <- ggarrange(f4a, f4b, f4c, f4d, labels = c("A", "B", "C", "D"), nrow = 1, ncol = 4)
figure7 <- annotate_figure(figure7, fig.lab = "Figure 4", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure7, filename = "new figure 7.pdf", height = 10, width = 16)
