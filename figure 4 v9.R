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

# normalize test data, calculate baseline firing rate
firing.all <- data.frame()
for (i in 1:4) {
    assign(paste0("test", i), normalization(paste0("z test", i, " all.csv")))
    firing <- filter(get(paste0("test", i)), time < 10)
    firing.all <- rbind(firing.all, firing)
}
cutoff <- 3 * sd(firing.all$z)
p1 <- sum(firing.all$z >= cutoff)/nrow(firing.all)
p2 <- sum(firing.all$z <= -1*cutoff)/nrow(firing.all)

# calculate firing rate of the trace period, rank by trace activity, determine activation
tests.wide <- data.frame()
for(i in 1:4){
    assign("temp", get(paste0("test", i)))
    firing <- temp %>%
        filter(time > 15, time < 45) %>%
        group_by(cell) %>%
        summarise(change = (sum(z >= cutoff) - sum(z <= -1*cutoff))/n())
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
    temp$activity <- "Unchanged"
    temp$activity[temp$change < -10*p2] <- "Inhibited"
    temp$activity[temp$change > 10*p1] <- "Activated"
    # select out trace cells
    temp <- temp %>%
        filter(type == "Trace Cells") %>%
        mutate(test = paste0("test", i))
    tests.wide <- rbind(tests.wide, temp)
}

bars <- tests.wide %>%
    group_by(test, activity) %>%
    summarise(number = n()) %>%
    spread(key = "test", value = "number")
bars[is.na(bars)] <- 0
col <- colorRamp2(c(20, 0, -20), brewer.pal(3, "RdYlBu"))

# bar graphs and heatmaps for four tests
for (i in 1:4) {
    assign("temp1",
           ggplot(bars, aes(x = activity, y = get(paste0("test", i)), fill = activity)) +
    geom_bar(stat = "identity", width = 0.5)+
    coord_flip()+
    #coord_polar("y", start = 0)+
    scale_fill_manual(values = c("red", "blue", "grey"))+
    xlim("Inhibited", "Unchanged", "Activated")+
    ylim(0, 55)+
    geom_text(aes(label = get(paste0("test", i))), size = 3, hjust = -0.2)+
    guides(fill = FALSE)+
    ggtitle(paste("Test", i))+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 10, face = "bold", hjust = 0.4)))
    
    temp <- tests.wide[tests.wide$test == paste0("test", i),]
    pdf(paste0("figure 4", letters[i], "2.pdf"), height = 6, width = 3)
    hm1 <- Heatmap(temp[, 2:60],col = col, name = paste("Test", i),
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 16),
               column_title_side = "bottom", row_title = "Trace Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 16),
               split = factor(temp$activity, levels = c("Activated", "Unchanged", "Inhibited")), 
               gap = unit(2, "mm"),
               bottom_annotation = columnAnnotation(
                   link = column_anno_link(at = c(1, 58), labels = c("10", "15"), 
                                           side = "bottom"), gp = gpar(fontsize = 20)),
               heatmap_legend_param = list(title_gp = gpar(fontsize = 16), 
                                           color_bar = "continuous",
                                           labels_gp = gpar(fontsize = 13), 
                                           legend_direction = "horizontal",
                                           title_position = "lefttop"))
    hm2 <- Heatmap(temp[, 61:411],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 16),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 16),
               bottom_annotation = columnAnnotation(
                   link = column_anno_link(at = c(1, 60, 118, 177, 236, 295, 351), 
                                           labels = c("16", "20", "25", "30", "35", "40", "45"), 
                                           side = "bottom"), gp = gpar(fontsize = 20)))
    draw(hm1+hm2, column_title = "Time (sec)", column_title_side = "bottom", 
        column_title_gp = gpar(fontsize = 16), heatmap_legend_side = "top")
    dev.off()
    img <- image_read_pdf(paste0("figure 4", letters[i], "2.pdf"))
    temp2 <- ggplot()+
        geom_blank()+
        background_image(img)
    assign(paste0("f4", letters[i]),
           ggarrange(temp1, temp2, ncol = 1, nrow = 2, heights = c(1, 4)))
}

figure4 <- ggarrange(f4a, f4b, f4c, f4d, labels = c("A", "B", "C", "D"), nrow = 1, ncol = 4)
figure4 <- annotate_figure(figure4, fig.lab = "Figure 4", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure4, filename = "figure 4.pdf", height = 11, width = 17.6, units = "cm")
