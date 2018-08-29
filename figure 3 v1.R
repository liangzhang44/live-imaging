library(tidyverse)
library(factoextra)
library(corrplot)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(EBImage)

# list all folders of z scores
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# read tone data into R
all.tone <- data.frame()
for(i in 1:length(allfolders)){
    tone <- read.csv(paste0("./", allfolders[i], "/z score 515.csv"))
    tone$cell <- tone$cell+100*i
    all.tone <- rbind(all.tone, tone)
}
colnames(all.tone) <- c("Cell", "Training", "Test 1", "Test 2", "Test 3", "Test 4")

# read shock data into R
all.shock <- data.frame()
for(i in 1:length(allfolders)){
    shock <- read.csv(paste0("./", allfolders[i], "/z score 5060.csv"))
    shock$cell <- shock$cell+100*i
    all.shock <- rbind(all.shock, shock)
}
colnames(all.shock) <- c("Cell", "Shock")

# merge shock and tone data
all <- merge(all.shock, all.tone, by="Cell")

# figure 3A
all.scaled <- column_to_rownames(all.tone, var = "Cell")
all.scaled <- as.data.frame(scale(all.scaled))
all.scaled.t <- as.data.frame(t(all.scaled))
res <- eclust(all.scaled.t, "hclust", k=2, graph = FALSE)

f3a <- fviz_dend(res, rect = TRUE, horiz = TRUE, palette = "jco", 
                 cex = 1, lwd = 1.2, main = NULL)+
    theme(axis.ticks = element_blank())+
    theme(axis.title = element_blank())+
    theme(axis.text = element_blank())

# figure 3B
all$Type <- NA
all[all$Training > 3 & all$Shock > 3, "Type"] <- "Type I"
all[all$Training > 3 & all$Shock < 3, "Type"] <- "Type II"
all[all$Training < 3 & all$Shock > 3, "Type"] <- "Type III"
all[all$Training < 3 & all$Shock < 3, "Type"] <- "Type IV"
all1 <- column_to_rownames(all, var="Cell")
all2 <- column_to_rownames(all.tone, var = "Cell")
all2 <- as.data.frame(all2>0)
all2$positive <- apply(all2, 1, sum)

all.scaled$Type <- all1$Type
all.scaled$Positive <- all2$positive
all.scaled[, -(6:7)] <- round(all.scaled[, -(6:7)], 1)
all.scaled <- arrange(all.scaled, desc(Positive), desc(`Test 1`, desc(`Test 2`), desc(`Test 3`)))

anno = anno_histogram(as.matrix(all.scaled[,-(6:7)]), 
                    gp = gpar(fill = "orange"), which = "row")
anno = anno_density(as.matrix(all.scaled[,-(6:7)]), type = "heatmap", which = "row")

ha_right = HeatmapAnnotation(violin=anno, which = "row", width = unit(8, "cm"))
col <- colorRamp2(c(2, 0, -2), brewer.pal(3, "RdBu"))
png("figure 3b.png", height = 1024, width = 1024)
Heatmap(all.scaled[,-(6:7)], name = "Z score", col = col,
        heatmap_legend_param = list(title_gp = gpar(fontsize = 20),
                                    grid_height = unit(12, "mm"),
                                    grid_width = unit(12, "mm"),
                                    label_gp = gpar(fontsize = 16)),
        column_title = "Phases", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        column_names_gp = gpar(fontsize = 22),
        row_title_gp = gpar(fontsize = 26),
        split = all.scaled$Type, gap = unit(6, "mm"),
        cluster_columns = FALSE, show_row_names = FALSE,
        show_row_dend = FALSE, cluster_rows = FALSE,
        raster_device = "png")+
    ha_right
dev.off()
img3b <- readImage("figure 3b.png")
df <- data.frame()
f3b <- ggplot(df)+
    geom_blank()+
    background_image(img3b)

# figure 3C
all.scaled.type1 <- all.scaled[all.scaled$Type=="Type I",]
all.scaled.type1$Type <- NULL
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
cor1 <- get_dist(all.scaled.type1, method="pearson")
f3c <- fviz_dist(cor1, show_labels = FALSE, 
                 gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))+
    labs(fill="Distance", title = "Type I")+
    theme(legend.title = element_text(size = 16), 
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 16))+
    guides(fill=guide_legend())

# figure 3D
all.scaled.type2 <- all.scaled[all.scaled$Type=="Type II",]
all.scaled.type2$Type <- NULL
cor2 <- get_dist(all.scaled.type2, method="pearson")
f3d <- fviz_dist(cor2, show_labels = FALSE, 
                 gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))+
    labs(fill="Distance", title = "Type II")+
    theme(legend.title = element_text(size = 16), 
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 16))+
    guides(fill=guide_legend())

# figure 3E
all.scaled.type3 <- all.scaled[all.scaled$Type=="Type III",]
all.scaled.type3$Type <- NULL
cor3 <- get_dist(all.scaled.type3, method="pearson")
f3e <- fviz_dist(cor3, show_labels = FALSE, 
                 gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))+
    labs(fill="Distance", title = "Type III")+
    theme(legend.title = element_text(size = 16), 
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 16))+
    guides(fill=guide_legend())

# figure 3F
all.scaled.type4 <- all.scaled[all.scaled$Type=="Type IV",]
all.scaled.type4$Type <- NULL
cor4 <- get_dist(all.scaled.type4, method="pearson")
f3f <- fviz_dist(cor4, show_labels = FALSE, 
                 gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))+
    labs(fill="Distance", title = "Type IV")+
    theme(legend.title = element_text(size = 16), 
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 16))+
    guides(fill=guide_legend())

figure3 <- ggarrange(ggarrange(f3a, f3b, labels = c("A", "B"), ncol = 1, nrow = 2, heights = c(1, 2)),
                     ggarrange(f3c, f3d, f3e, f3f, labels = c("C", "D", "E", "F"), ncol = 1, nrow = 4), 
                     ncol = 2, widths = c(2, 1))
figure3 <- annotate_figure(figure3, fig.lab = "Figure 3", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure3, filename = "figure 3.png", height = 12, width = 12)
