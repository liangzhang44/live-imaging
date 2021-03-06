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

ranking <- late2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")

temp <- ranking[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
ranking[, -1] <- temp

ranking <- ranking %>%
    mutate(tone.mean = round(rowMeans(ranking[, 2:177]), 1), 
           trace.mean = round(rowMeans(ranking[, 178:529]), 1)) %>%
    arrange(desc(trace.mean), desc(tone.mean))
order <- ranking$cell
ranking$type <- ifelse(ranking$trace.mean > 10*p, "Trace Cells", "Non-Trace Cells")

training.late2 <- late2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")

training.late2 <- training.late2[match(order, training.late2$cell),]
training.late2$cell <- 1:80
rownames(training.late2) <- NULL
training.late2 <- column_to_rownames(training.late2, var = "cell")
training.late2$type <- ranking$type

# figure 3b
col <- colorRamp2(c(20, 0, -20), brewer.pal(3, "RdYlBu"))
pdf("new figure 3b.pdf", height = 12, width = 6)
hm1 <- Heatmap(training.late2[,1:176],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_names_side = "left",
               show_row_names = TRUE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               split = factor(training.late2$type, levels = c("Trace Cells", "Non-Trace Cells")), gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 174), labels=c("10", "15", "20", "25"), 
                                         side="bottom"), gp=gpar(fontsize=22)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=20), color_bar = "continuous", 
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(training.late2[,177:528],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", 
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("26", "30", "35", "40", "45", "50", "55"), 
                                         side="bottom"), gp=gpar(fontsize=22)))
draw(hm1+hm2, column_title = "Training (Sessions 6&7)", column_title_side = "top", 
     column_title_gp = gpar(fontsize = 24), heatmap_legend_side = "top")
dev.off()

img3b <- image_read_pdf("new figure 3b.pdf")
f3b <- ggplot()+
    geom_blank()+
    background_image(img3b)


# read dF/F data of training, first 2 cycles
early2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z training early2.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    early2 <- rbind(early2, temp)
}

early2.mean <- early2 %>%
    group_by(time) %>%
    summarise(z = mean(z))
early.tone.mean <- mean(early2.mean$z[early2.mean$time < 10 & early2.mean$time > 5])
early.tone.sd <- sd(early2.mean$z[early2.mean$time < 10 & early2.mean$time > 5])
early2.tone <- early2 %>%
    filter(time > 10, time < 55) %>%
    mutate(z = (z - early.tone.mean)/early.tone.sd)

training.early2 <- early2.tone %>%
    spread(key = "time", value = "z")
training.early2$cell <- as.character(training.early2$cell)

training.early2 <- training.early2[match(order, training.early2$cell),]
training.early2$cell <- 1:80
training.early2$type <- training.late2$type
rownames(training.early2) <- NULL
training.early2 <- column_to_rownames(training.early2, var = "cell")

# figure 3a
pdf("new figure 3a.pdf", height = 12, width = 6)
hm1 <- Heatmap(training.early2[,1:176],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_names_side = "left",
               show_row_names = TRUE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               split = factor(training.early2$type, levels = c("Trace Cells", "Non-Trace Cells")), gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 174), labels=c("10", "15", "20", "25"), 
                                         side="bottom"), gp=gpar(fontsize=22)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=20), color_bar = "continuous", 
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(training.early2[,177:528],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", 
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("26", "30", "35", "40", "45", "50", "55"), 
                                         side="bottom"), gp=gpar(fontsize=22)))
draw(hm1+hm2, column_title = "Training (Sessions 1&2)", column_title_side = "top", 
     column_title_gp = gpar(fontsize = 24), heatmap_legend_side = "top")
dev.off()

img3a <- image_read_pdf("new figure 3a.pdf")
f3a <- ggplot()+
    geom_blank()+
    background_image(img3a)

figure3 <- ggarrange(f3a, f3b, labels = c("A", "B"))
figure3 <- annotate_figure(figure3, fig.lab = "Figure 3", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure3, filename = "new figure 3.pdf", height = 9, width = 9)
