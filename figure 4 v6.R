library(tidyverse)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(zoo)
library(magick)
source("normalization.R")

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# normalize training data, last 2 sessions
late2 <- normalization("z training late2.csv")

# calculate baseline firing rate
firing <- filter(late2, time > 5, time < 10)
cutoff <- 3 * sd(firing$z)
p <- sum(firing$z >= cutoff)/nrow(firing)

# calculate firing rate of the trace period
late2.firing <- late2 %>%
    filter(time > 25, time < 55) %>%
    group_by(cell) %>%
    summarise(firing = sum(z >= cutoff)/n())
# subset tone and trace
late2 <- late2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")
# calculate mean z scores of trace and tone
late2$firing <- late2.firing$firing
late2 <- late2 %>%
    mutate(tone.mean = rowMeans(late2[, 2:177]), 
           trace.mean = rowMeans(late2[, 178:529])) %>%
    arrange(desc(trace.mean), desc(tone.mean))
order <- late2$cell
# determine trace cells
late2$type <- ifelse(late2$firing > 10*p, "Trace Cells", "Non-Trace Cells")

# normalize test data
test1 <- normalization("z test1 all.csv")
test2 <- normalization("z test2 all.csv")
test3 <- normalization("z test3 all.csv")
test4 <- normalization("z test4 all.csv")

# calculate baseline firing rate
firing1 <- filter(test1, time > 5, time < 10)
firing2 <- filter(test2, time > 5, time < 10)
firing3 <- filter(test3, time > 5, time < 10)
firing4 <- filter(test4, time > 5, time < 10)

basefiring <- rbind(firing1, firing2, firing3, firing4)
cutoff <- 3 * sd(basefiring$z)
p1 <- sum(basefiring$z >= cutoff)/nrow(basefiring)
p2 <- sum(basefiring$z <= -1*cutoff)/nrow(basefiring)

# calculate firing rate of the trace period, rank by trace activity, determine activation
tests <- list(test1, test2, test3, test4)
tests.wide <- data.frame()
for(i in 1:4){
    temp <- tests[[i]]
    firing <- temp %>%
        filter(time > 15, time < 45) %>%
        group_by(cell) %>%
        summarise(activation = sum(z >= cutoff)/n(),
              inhibition = -1*sum(z <= -1*cutoff)/n())
    firing$change <- ifelse(abs(firing$activation) > abs(firing$inhibition),
                              firing$activation, firing$inhibition)
    # rearrange by late2 order
    temp <- temp %>%
        filter(time > 10, time < 45) %>%
        spread(key = "time", value = "z")
    temp$change <- firing$change
    temp <- temp[match(order, temp$cell),]
    temp$type <- late2$type
    temp <- temp %>%
    mutate(tone.mean = rowMeans(temp[, 2:60]), 
           trace.mean = rowMeans(temp[, 61:411])) %>%
    arrange(desc(trace.mean), desc(tone.mean))
    # determine activity
    temp$activity <- NA
    temp$activity[temp$change < -10*p2] <- "Inhibited"
    temp$activity[temp$change > 10*p1] <- "Activated"
    temp$activity[is.na(temp$activity)] <- "Unchanged"
    # select out trace cells
    temp <- filter(temp, type == "Trace Cells")
    temp <- mutate(temp, test = paste0("test", i))
    tests.wide <- rbind(tests.wide, temp)
}

pies <- tests.wide %>%
    group_by(test, activity) %>%
    summarise(number = n()) %>%
    spread(key = "test", value = "number")
pies[is.na(pies)] <- 0

# figure 4a
f4a1 <- ggplot(pies, aes(x = activity, y = test1, fill = activity)) +
    geom_bar(stat = "identity", width = 0.5)+
    #coord_polar("y", start = 0)+
    scale_fill_manual(values = c("red", "blue", "grey"))+
    xlim("", "Activated", "Unchanged", "Inhibited", "")+
    ylim(0, 55)+
    geom_text(aes(label = test1), size = 5, vjust = -0.2)+
    guides(fill = FALSE)+
    ggtitle("Test 1")+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12, angle = 30, vjust = 1, hjust = 0.8),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

test1 <- tests.wide[tests.wide$test == "test1",]
col <- colorRamp2(c(20, 0, -20), brewer.pal(3, "RdYlBu"))
pdf("new figure 4a2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test1[, 2:60],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom", row_title = "Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = factor(test1$activity, levels = c("Activated", "Unchanged", "Inhibited")), 
               gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "continuous",
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(test1[, 61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, column_title = "Time (sec)", column_title_side = "bottom", 
     column_title_gp = gpar(fontsize = 20), heatmap_legend_side = "top")
dev.off()

img4a2 <- image_read_pdf("new figure 4a2.pdf")
f4a2 <- ggplot()+
    geom_blank()+
    background_image(img4a2)

# figure 4b
f4b1 <- ggplot(pies, aes(x = activity, y = test2, fill = activity)) +
    geom_bar(stat = "identity", width = 0.5)+
    #coord_polar("y", start = 0)+
    scale_fill_manual(values = c("red", "blue", "grey"))+
    xlim("", "Activated", "Unchanged", "Inhibited", "")+
    ylim(0, 55)+
    geom_text(aes(label = test2), size = 5, vjust = -0.2)+
    guides(fill = FALSE)+
    ggtitle("Test 2")+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12, angle = 30, vjust = 1, hjust = 0.8),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

test2 <- tests.wide[tests.wide$test == "test2",]
pdf("new figure 4b2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test2[, 2:60],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom", row_title = "Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = factor(test2$activity, levels = c("Activated", "Unchanged", "Inhibited")), 
               gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "continuous",
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(test2[, 61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, column_title = "Time (sec)", column_title_side = "bottom", 
     column_title_gp = gpar(fontsize = 20), heatmap_legend_side = "top")
dev.off()

img4b2 <- image_read_pdf("new figure 4b2.pdf")
f4b2 <- ggplot()+
    geom_blank()+
    background_image(img4b2)

# figure 4c
f4c1 <- ggplot(pies, aes(x = activity, y = test3, fill = activity)) +
    geom_bar(stat = "identity", width = 0.5)+
    #coord_polar("y", start = 0)+
    scale_fill_manual(values = c("red", "blue", "grey"))+
    xlim("", "Activated", "Unchanged", "Inhibited", "")+
    ylim(0, 55)+
    geom_text(aes(label = test3), size = 5, vjust = -0.2)+
    guides(fill = FALSE)+
    ggtitle("Test 3")+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12, angle = 30, vjust = 1, hjust = 0.8),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

test3 <- tests.wide[tests.wide$test == "test3",]
pdf("new figure 4c2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test3[, 2:60],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom", row_title = "Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = factor(test3$activity, levels = c("Activated", "Unchanged", "Inhibited")), 
               gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "continuous",
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(test3[, 61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, column_title = "Time (sec)", column_title_side = "bottom", 
     column_title_gp = gpar(fontsize = 20), heatmap_legend_side = "top")
dev.off()

img4c2 <- image_read_pdf("new figure 4c2.pdf")
f4c2 <- ggplot()+
    geom_blank()+
    background_image(img4c2)

# figure 4d
f4d1 <- ggplot(pies, aes(x = activity, y = test4, fill = activity)) +
    geom_bar(stat = "identity", width = 0.5)+
    #coord_polar("y", start = 0)+
    scale_fill_manual(values = c("red", "blue", "grey"))+
    xlim("", "Activated", "Unchanged", "Inhibited", "")+
    ylim(0, 55)+
    geom_text(aes(label = test4), size = 5, vjust = -0.2)+
    guides(fill = FALSE)+
    ggtitle("Test 4")+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12, angle = 30, vjust = 1, hjust = 0.8),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

test4 <- tests.wide[tests.wide$test=="test4",]
pdf("new figure 4d2.pdf", height = 8, width = 4)
hm1 <- Heatmap(test4[, 2:60],col = col, name = "Z Score",
               cluster_columns = FALSE, cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom", row_title = "Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = factor(test4$activity, levels = c("Activated", "Unchanged", "Inhibited")), 
               gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                         side="bottom"), gp=gpar(fontsize=20)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "continuous",
                                           labels_gp=gpar(fontsize=18), legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(test4[, 61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                         side="bottom"), gp=gpar(fontsize=20)))
draw(hm1+hm2, column_title = "Time (sec)", column_title_side = "bottom", 
     column_title_gp = gpar(fontsize = 20), heatmap_legend_side = "top")
dev.off()

img4d2 <- image_read_pdf("new figure 4d2.pdf")
f4d2 <- ggplot()+
    geom_blank()+
    background_image(img4d2)

f4a <- ggarrange(f4a1, f4a2, ncol = 1, nrow = 2, heights = c(1, 4))
f4b <- ggarrange(f4b1, f4b2, ncol = 1, nrow = 2, heights = c(1, 4))
f4c <- ggarrange(f4c1, f4c2, ncol = 1, nrow = 2, heights = c(1, 4))
f4d <- ggarrange(f4d1, f4d2, ncol = 1, nrow = 2, heights = c(1, 4))
figure4 <- ggarrange(f4a, f4b, f4c, f4d, labels = c("A", "B", "C", "D"), nrow = 1, ncol = 4)
figure4 <- annotate_figure(figure4, fig.lab = "Figure 4", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure4, filename = "new figure 4.pdf", height = 10, width = 16)