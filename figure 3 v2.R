library(tidyverse)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)

# process dF/F data into Z scores
source("dF.all.z.R")

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

for(i in 1:length(allfolders)){
    dF.all.z(allfolders[i])
}

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
late2.tone <- late2 %>%
    filter(time > 10, time < 55) %>%
    mutate(z = (z - late.tone.mean)/late.tone.sd)

training.late2 <- late2.tone %>%
    spread(key = "time", value = "z")
training.late2$cell <- as.character(training.late2$cell)

training.late2 <- training.late2 %>%
    mutate(tone.mean = rowMeans(training.late2[, -1]), trace.mean = rowMeans(training.late2[, 178:529])) %>%
    arrange(desc(tone.mean))
order <- training.late2$cell

# figure 3b
col <- colorRamp2(c(15, 0, -15), brewer.pal(3, "RdYlBu"))
png("new figure 3b.png", height = 1200, width = 600)
hm1 <- Heatmap(training.late2[,2:177],col = col, name = "Z Score",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Tone", column_title_gp = gpar(fontsize = 24),
        column_title_side = "bottom", row_title = "Cells",
        show_row_names = FALSE, show_column_names = FALSE,
        row_title_gp = gpar(fontsize = 24),
        #split = training$type, gap = unit(3, "mm"),
        bottom_annotation = columnAnnotation(
            link=column_anno_link(at=c(1, 60, 118, 174), labels=c("10", "15", "20", "25"), 
                                  side="bottom"), gp=gpar(fontsize=22)),
        heatmap_legend_param = list(title_gp=gpar(fontsize=20), color_bar = "continuous", labels_gp=gpar(fontsize=18),
                                    grid_height = unit(6, "mm"), grid_width = unit(6, "mm")))
hm2 <- Heatmap(training.late2[,178:529],col = col, show_heatmap_legend = FALSE,
            cluster_columns = FALSE,cluster_rows = FALSE,
            column_title = "Trace", column_title_gp = gpar(fontsize = 24),
            column_title_side = "bottom",
            show_row_names = FALSE, show_column_names = FALSE,
            row_title_gp = gpar(fontsize = 24),
            #split = training$type, gap = unit(3, "mm"),
            bottom_annotation = columnAnnotation(
                link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("26", "30", "35", "40", "45", "50", "55"), 
                                      side="bottom"), gp=gpar(fontsize=22)))
draw(hm1+hm2, column_title = "Training (Sessions 6&7)", column_title_side = "top", column_title_gp = gpar(fontsize = 24))
dev.off()

img3b <- readImage("new figure 3b.png")
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

# figure 3a
png("new figure 3a.png", height = 1200, width = 600)
hm1 <- Heatmap(training.early2[,2:177],col = col, name = "Z Score",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom", row_title = "Cells",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               #split = training$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 174), labels=c("10", "15", "20", "25"), 
                                         side="bottom"), gp=gpar(fontsize=22)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=20), color_bar = "continuous", labels_gp=gpar(fontsize=18),
                                           grid_height = unit(6, "mm"), grid_width = unit(6, "mm")))
hm2 <- Heatmap(training.early2[,178:529],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 24),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24),
               #split = training$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("26", "30", "35", "40", "45", "50", "55"), 
                                         side="bottom"), gp=gpar(fontsize=22)))
draw(hm1+hm2, column_title = "Training (Sessions 1&2)", column_title_side = "top", column_title_gp = gpar(fontsize = 24))
dev.off()

img3a <- readImage("new figure 3a.png")
f3a <- ggplot()+
    geom_blank()+
    background_image(img3a)

figure3 <- ggarrange(f3a, f3b, labels = c("A", "B"))
figure3 <- annotate_figure(figure3, fig.lab = "Figure 3", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure3, filename = "new figure 3.png", height = 9, width = 9)

#hist(training.late2$trace.mean, breaks = 40)
