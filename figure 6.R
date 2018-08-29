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


# read z score data of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test3 early2.csv"))
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
#p <- sum(firing$z == 1)/nrow(firing)

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
test3 <- filter(test3, type == "Trace Cells")

# figure 4c
pie3 <- test3 %>%
    group_by(active) %>%
    summarise(number = n())
pie3$active <- factor(pie3$active, levels = c("Not Active", "Active"))
f6a1 <- ggplot(pie3, aes(x = factor(1), y = number, fill = active)) +
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
pdf("new figure 6a2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test3[,2:60],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Trace Cells",
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

img6a2 <- image_read_pdf("new figure 6a2.pdf")
f6a2 <- ggplot()+
    geom_blank()+
    background_image(img6a2)

# read z score data of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test3 late2.csv"))
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
#p <- sum(firing$z == 1)/nrow(firing)

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
test3 <- filter(test3, type == "Trace Cells")

# figure 4c
pie3 <- test3 %>%
    group_by(active) %>%
    summarise(number = n())
pie3$active <- factor(pie3$active, levels = c("Not Active", "Active"))
f6b1 <- ggplot(pie3, aes(x = factor(1), y = number, fill = active)) +
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
pdf("new figure 6b2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test3[,2:60],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Trace Cells",
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

img6b2 <- image_read_pdf("new figure 6b2.pdf")
f6b2 <- ggplot()+
    geom_blank()+
    background_image(img6b2)

f6a <- ggarrange(f6a1, f6a2, ncol = 1, nrow = 2, heights = c(1, 4))
f6b <- ggarrange(f6b1, f6b2, ncol = 1, nrow = 2, heights = c(1, 4))

figure6 <- ggarrange(f6a, f6b, labels = c("A", "B"), nrow = 1, ncol = 2)
figure6 <- annotate_figure(figure6, fig.lab = "Figure 6", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure6, filename = "new figure 6.pdf", height = 10, width = 8)