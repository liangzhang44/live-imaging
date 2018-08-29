library(tidyverse)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(zoo)
library(magick)
source("normalization.R")

# normalize training data, last 2 sessions
late2 <- normalization("z training late2.csv")

# calculate baseline firing rate
firing <- filter(late2, time < 10)
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
late2$type <- ifelse(late2$firing > p, "Trace Cells", "Non-Trace Cells")

# normalize training data, first 2 sessions
early2 <- normalization("z training early2.csv")

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


late2 <- filter(late2, type == "Trace Cells")
early2 <- filter(early2, type == "Trace Cells")


# figure 3b
col <- colorRamp2(c(20, 0, -20), brewer.pal(3, "RdYlBu"))
pdf("figure 3b.pdf", height = 6, width = 4)
Heatmap(late2[,178:529],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE, row_title = "Cells",
               column_title = "Time (sec)", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom", 
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24))
dev.off()


# figure 3a
pdf("figure 3a.pdf", height = 6, width = 4)
Heatmap(early2[,178:529],col = col, show_heatmap_legend = FALSE,
               cluster_columns = FALSE,cluster_rows = FALSE, row_title = "Cells",
               column_title = "Time (sec)", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom", 
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 24))
dev.off()
