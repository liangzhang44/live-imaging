library(tidyverse)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(zoo)
library(magick)
source("normalization.R")

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
    temp <- temp %>%
        mutate(tone.mean = rowMeans(temp[, 2:60]), 
               trace.mean = rowMeans(temp[, 61:411])) %>%
        arrange(desc(trace.mean), desc(tone.mean))
    # determine activity
    temp$activity <- "Unchanged"
    temp$activity[temp$change < -10*p2] <- "Inhibited"
    temp$activity[temp$change > 10*p1] <- "Activated"
    # select out trace cells
    temp <- mutate(temp, test = paste0("test", i))
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
               ylim(0, 83)+
               geom_text(aes(label = get(paste0("test", i))), size = 5, hjust = -0.2)+
               guides(fill = FALSE)+
               ggtitle(paste("Test", i))+
               theme_minimal()+
               theme(axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     panel.border = element_blank(),
                     panel.grid = element_blank(),
                     axis.ticks = element_blank(),
                     axis.text.y = element_text(size = 12),
                     plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
    
    temp <- tests.wide[tests.wide$test == paste0("test", i),]
    pdf(paste0("figure 4", letters[i], "2.pdf"), height = 10, width = 4)
    hm1 <- Heatmap(temp[, 2:60],col = col, name = "Z Score",
                   cluster_columns = FALSE,cluster_rows = FALSE,
                   column_title = "Tone", column_title_gp = gpar(fontsize = 20),
                   column_title_side = "bottom", row_title = "Trace Cells",
                   show_row_names = FALSE, show_column_names = FALSE,
                   row_title_gp = gpar(fontsize = 18),
                   split = factor(temp$activity, 
                                  levels = c("Activated", "Unchanged", "Inhibited")), 
                   gap = unit(3, "mm"),
                   bottom_annotation = columnAnnotation(
                       link=column_anno_link(at=c(1, 58), labels=c("10", "15"), 
                                             side="bottom"), gp=gpar(fontsize=20)),
                   heatmap_legend_param = list(title_gp=gpar(fontsize=18), 
                                               color_bar = "continuous",
                                               labels_gp=gpar(fontsize=18), 
                                               legend_direction = "horizontal",
                                               title_position = "lefttop"))
    hm2 <- Heatmap(temp[, 61:411],col = col, show_heatmap_legend = FALSE,
                   cluster_columns = FALSE,cluster_rows = FALSE,
                   column_title = "Trace", column_title_gp = gpar(fontsize = 20),
                   column_title_side = "bottom",
                   show_row_names = FALSE, show_column_names = FALSE,
                   row_title_gp = gpar(fontsize = 18),
                   bottom_annotation = columnAnnotation(
                       link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), 
                                             labels=c("16", "20", "25", "30", "35", "40", "45"), 
                                             side="bottom"), gp=gpar(fontsize=20)))
    draw(hm1+hm2, column_title = "Time (sec)", column_title_side = "bottom", 
         column_title_gp = gpar(fontsize = 20), heatmap_legend_side = "top")
    dev.off()
    img <- image_read_pdf(paste0("figure 4", letters[i], "2.pdf"))
    temp2 <- ggplot()+
        geom_blank()+
        background_image(img)
    assign(paste0("f4", letters[i]),
           ggarrange(temp1, temp2, ncol = 1, nrow = 2, heights = c(1, 5)))
}

figure4 <- ggarrange(f4a, f4b, f4c, f4d, labels = c("A", "B", "C", "D"), nrow = 1, ncol = 4)
figure4 <- annotate_figure(figure4, fig.lab = "Figure 4", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure4, filename = "figure 4.pdf", height = 12, width = 16)