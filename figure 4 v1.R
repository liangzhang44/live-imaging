library(tidyverse)
library(ggpubr)
library(EBImage)
library(VennDiagram)

# figure 4A
fired.tone <- list(Training = all.tone$Cell[all.tone$Training>3],
                   `Test 1` = all.tone$Cell[all.tone$`Test 1`>3], 
                   `Test 2` = all.tone$Cell[all.tone$`Test 2`>3])
venn.diagram(fired.tone, filename = "venn tone 3.png", imagetype = "png",
             col = "black",
             lty = "dotted",
             lwd = 4,
             fill = c("cornflowerblue", "green", "yellow"),
             alpha = 0.50,
             cex = 1.5,
             fontfamily = "serif",
             fontface = "bold",
             cat.col = c("darkblue", "darkgreen", "orange"),
             cat.cex = 1.5,
             cat.fontfamily = "serif",
             cat.default.pos = "text",
             cat.dist = c(0.06, 0.06, 0.03),
             cat.pos = 0)

img4a <- readImage("venn tone 3.png")
df <- data.frame()
f4a <- ggplot(df)+
    geom_blank()+
    background_image(img4a)

# figure 4B
fired.tone <- list(Training = all.tone$Cell[all.tone$Training>3],
                   `Test 1` = all.tone$Cell[all.tone$`Test 1`>3], 
                   `Test 2` = all.tone$Cell[all.tone$`Test 2`>3],
                   `Test 3` = all.tone$Cell[all.tone$`Test 3`>3])
venn.diagram(fired.tone, filename = "venn tone 4.png", imagetype = "png",
             col = "black",
             lty = "dotted",
             lwd = 4,
             fill = c("cornflowerblue", "green", "yellow", "darkorchid1"),
             alpha = 0.50,
             cex = 1.5,
             fontfamily = "serif",
             fontface = "bold",
             cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4"),
             cat.cex = 1.5,
             cat.fontfamily = "serif",
             cat.default.pos = "text",
             cat.dist = c(0.06, 0.06, 0.03, 0.03),
             cat.pos = 0)

img4b <- readImage("venn tone 4.png")
df <- data.frame()
f4b <- ggplot(df)+
    geom_blank()+
    background_image(img4b)

# figure 4C
fired.tone <- list(Training = all.tone$Cell[all.tone$Training>3],
                   `Test 1` = all.tone$Cell[all.tone$`Test 1`>3], 
                   `Test 2` = all.tone$Cell[all.tone$`Test 2`>3],
                   `Test 3` = all.tone$Cell[all.tone$`Test 3`>3], 
                   `Test 4` = all.tone$Cell[all.tone$`Test 4`>3])
venn.diagram(fired.tone, filename = "venn tone 5.png", imagetype = "png",
             col = "black",
             lty = "dotted",
             lwd = 4,
             fill = c("cornflowerblue", "green", "yellow", "darkorchid1", "red"),
             alpha = 0.50,
             cex = 1.5,
             fontfamily = "serif",
             fontface = "bold",
             cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4", "darkred"),
             cat.cex = 1.5,
             cat.fontfamily = "serif",
             cat.default.pos = "text",
             cat.dist = c(0.06, 0.06, 0.06, 0.06, 0.06),
             cat.pos = 0)

img4c <- readImage("venn tone 5.png")
df <- data.frame()
f4c <- ggplot(df)+
    geom_blank()+
    background_image(img4c)

# figure 4D
all.tone1 <- all[all$Training > 3 & all$`Test 1` > 3 & all$`Test 2` > 3, -2]
all.tone1 <- gather(all.tone1, key = "Phase", value = "Z", -Type, -Cell)

f4d <- ggboxplot(all.tone1, x = "Phase", y = "Z", color = "Phase", facet.by = "Type",
          legend = "none", ylab = "Z Score", title = "Consecutively activated cells") +
    rotate_x_text(angle = 45)+
    stat_compare_means(method = "anova", label.y = 40, paired = TRUE)+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = "Training", label.y = 35, paired = TRUE)

# figure 4E
all.tone2 <- all[all$Training < 3 & all$`Test 1` < 3 & all$`Test 2` < 3, -2]
all.tone2 <- gather(all.tone2, key = "Phase", value = "Z", -Type, -Cell)

f4e <- ggboxplot(all.tone2, x = "Phase", y = "Z", color = "Phase", facet.by = "Type",
          legend = "none", ylab = "Z Score", title = "Consecutively inactive cells") +
    rotate_x_text(angle = 45)+
    stat_compare_means(method = "anova", label.y = 25, paired = TRUE)+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = "Training", label.y = 20, paired = TRUE) 

figure4 <- ggarrange(ggarrange(f4a, f4b, f4c, labels = c("A", "B", "C"), ncol = 3, nrow = 1),
                     ggarrange(f4d, f4e, labels = c("D", "E"), ncol = 2, nrow = 1), 
                     ncol = 1, nrow = 2)
figure4 <- annotate_figure(figure4, fig.lab = "Figure 4", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure4, filename = "figure 4.png", height = 8, width = 12)
