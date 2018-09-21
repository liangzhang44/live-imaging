library(tidyverse)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(zoo)
library(magick)
source("normalization.R")

# normalize training data, first 2 sessions
early2 <- normalization("z training early2.csv")

# normalize training data, last 2 sessions
late2 <- normalization("z training late2.csv")

# calculate baseline firing rate
firing <- filter(rbind(early2, late2), time < 10)
cutoff <- 3 * sd(firing$z)
p <- sum(firing$z >= cutoff)/nrow(firing)

# calculate firing rate of the trace period
late2.firing <- late2 %>%
    filter(time > 25, time < 55) %>%
    group_by(cell) %>%
    summarise(firing = (sum(z >= cutoff) - sum(z <= -1*cutoff))/n())

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

# figure 3b
col <- colorRamp2(c(20, 0, -20), brewer.pal(3, "RdYlBu"))
pdf("figure 3b.pdf", height = 6, width = 3)
hm1 <- Heatmap(late2[,2:177],col = col, name = "Training (6&7)",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 14),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 14),
               split = factor(late2$type, levels = c("Trace Cells", "Non-Trace Cells")), 
               gap = unit(2, "mm"),
               bottom_annotation = columnAnnotation(
                   link = column_anno_link(at = c(1, 60, 118, 174), 
                                           labels = c("10", "15", "20", "25"), 
                                           side = "bottom"), gp = gpar(fontsize = 22)),
               heatmap_legend_param = list(title_gp = gpar(fontsize = 10), 
                                           color_bar = "continuous", 
                                           labels_gp = gpar(fontsize = 12), 
                                           legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(late2[,178:529],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 14),
               column_title_side = "bottom", 
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 14),
               bottom_annotation = columnAnnotation(
                   link = column_anno_link(at = c(1, 60, 118, 177, 236, 295, 351), 
                                           labels = c("26", "30", "35", "40", "45", "50", "55"), 
                                           side = "bottom"), gp = gpar(fontsize = 22)))
draw(hm1+hm2, column_title = "Time (sec)", column_title_side = "bottom", 
     column_title_gp = gpar(fontsize = 14), heatmap_legend_side = "top")
dev.off()

img3b <- image_read_pdf("figure 3b.pdf")
f3b <- ggplot()+
    geom_blank()+
    background_image(img3b)


# rearrange by late2 order
early2 <- early2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")
early2 <- early2[match(order, early2$cell),]
early2$type <- late2$type

# calculate mean z scores of trace and tone
early2 <- early2 %>%
    mutate(tone.mean = rowMeans(early2[, 2:177]), 
           trace.mean = rowMeans(early2[, 178:529])) %>%
    arrange(desc(trace.mean), desc(tone.mean))

# figure 3a
pdf("figure 3a.pdf", height = 6, width = 3)
hm1 <- Heatmap(early2[,2:177],col = col, name = "Training (1&2)",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 14),
               column_title_side = "bottom", row_names_side = "left",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 14),
               split = factor(early2$type, levels = c("Trace Cells", "Non-Trace Cells")), 
               gap = unit(2, "mm"),
               bottom_annotation = columnAnnotation(
                   link = column_anno_link(at = c(1, 60, 118, 174), 
                                           labels = c("10", "15", "20", "25"), 
                                           side = "bottom"), gp = gpar(fontsize = 22)),
               heatmap_legend_param = list(title_gp = gpar(fontsize = 10), 
                                           color_bar = "continuous", 
                                           labels_gp = gpar(fontsize = 12), 
                                           legend_direction = "horizontal",
                                           title_position = "lefttop"))
hm2 <- Heatmap(early2[,178:529],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 14),
               column_title_side = "bottom", 
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 14),
               bottom_annotation = columnAnnotation(
                   link = column_anno_link(at = c(1, 60, 118, 177, 236, 295, 351), 
                                           labels = c("26", "30", "35", "40", "45", "50", "55"), 
                                           side = "bottom"), gp = gpar(fontsize = 22)))
draw(hm1+hm2, column_title = "Time (sec)", column_title_side = "bottom", 
     column_title_gp = gpar(fontsize = 14), heatmap_legend_side = "top")
dev.off()

img3a <- image_read_pdf("figure 3a.pdf")
f3a <- ggplot()+
    geom_blank()+
    background_image(img3a)

figure3 <- ggarrange(f3a, f3b, labels = c("A", "B"))
figure3 <- annotate_figure(figure3, fig.lab = "Figure 3", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure3, filename = "figure 3.pdf", height = 11.6, width = 11.6, units = "cm")