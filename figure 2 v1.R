library(tidyverse)
library(ggpubr)
library(EBImage)

# figure 2A
img1 <- readImage("figure 2a.png")
df <- data.frame()
f2a <- ggplot(df)+
    geom_blank()+
    background_image(img1)
    
samplecells <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0001.csv")
samplecells <- samplecells[,c(2,9,10,11)]
colnames(samplecells) <- c("Time", "Cell7", "Cell8", "Cell9")
samplecells5060 <- filter(samplecells, Time>50, Time<60)
samplecells5060$Time <- samplecells5060$Time-55
samplecells5060.f2b <- gather(samplecells5060, key = "Cell", value = "Fluorescence", -Time)

# figure 2B
f2b <- ggplot(samplecells5060.f2b, aes(Time, Fluorescence, color=Cell))+
    geom_line()+
    scale_y_continuous(limits = c(280, 392))+
    annotate("rect", xmin = 0, xmax = 1, ymin = 280, ymax = 385, fill="red", alpha=0.4)+
    annotate("text", x=0.5, y=392, label="Shock", color="red", size=6)+
    scale_color_brewer(palette = "Set1", guide=FALSE)+
    labs(x="Time (sec)", y="Fluorescence (AU)")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme_pubr()
# figure 2C???
samplecells5060$Cell7 <- samplecells5060$Cell7-mean(samplecells5060$Cell7[samplecells5060$Time<0])
samplecells5060$Cell7 <- cumsum(samplecells5060$Cell7)
samplecells5060$Cell8 <- samplecells5060$Cell8-mean(samplecells5060$Cell8[samplecells5060$Time<0])
samplecells5060$Cell8 <- cumsum(samplecells5060$Cell8)
samplecells5060$Cell9 <- samplecells5060$Cell9-mean(samplecells5060$Cell9[samplecells5060$Time<0])
samplecells5060$Cell9 <- cumsum(samplecells5060$Cell9)
samplecells5060.f2c <- gather(samplecells5060, key = "Cell", value = "CuSum", -Time)

f2c <- ggplot(samplecells5060.f2c, aes(Time, CuSum, color=Cell))+
    geom_line()+
    #scale_y_continuous(limits = c(280, 392))+
    annotate("rect", xmin = 0, xmax = 1, ymin = 280, ymax = 385, fill="red", alpha=0.4)+
    annotate("text", x=0.5, y=392, label="Shock", color="red", size=6)+
    scale_color_brewer(palette = "Set1", guide=FALSE)+
    labs(x="Time (sec)", y="Fluorescence (AU)")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme_pubr()

# figure 2C
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
colnames(all.tone) <- c("Cell", "Training-Tone", "Test 1-Tone", "Test 2-Tone", 
                        "Test 3-Tone", "Test 4-Tone")

# read shock data into R
all.shock <- data.frame()
for(i in 1:length(allfolders)){
    shock <- read.csv(paste0("./", allfolders[i], "/z score 5060.csv"))
    shock$cell <- shock$cell+100*i
    all.shock <- rbind(all.shock, shock)
}
colnames(all.shock) <- c("Cell", "Training-Shock")

# merge shock and tone data
all <- merge(all.shock, all.tone, by="Cell")

# Venn diagram of tone and shock
fired <- list(Shock = all.shock$Cell[all.shock$`Training-Shock`>3], 
                    Tone = all.tone$Cell[all.tone$`Training-Tone`>3])
venn.diagram(fired, filename = "venn shock.png", imagetype = "png",
             col = "black",
             lty = "dotted",
             lwd = 4,
             fill = c("green", "red"),
             alpha = 0.50,
             cex = 1.5,
             fontfamily = "serif",
             fontface = "bold",
             cat.col = c("darkgreen", "darkred"),
             cat.cex = 1.5,
             cat.fontfamily = "serif",
             cat.default.pos = "outer")
img2 <- readImage("venn shock.png")
f2c <- ggplot(df)+
    geom_blank()+
    background_image(img2)

# figure 2D
all1 <- column_to_rownames(all, var="Cell")
all1$Type <- NA
all1[all1$`Training-Tone` > 3 & all1$`Training-Shock` > 3, "Type"] <- "Type I"
all1[all1$`Training-Tone` > 3 & all1$`Training-Shock` < 3, "Type"] <- "Type II"
all1[all1$`Training-Tone` < 3 & all1$`Training-Shock` > 3, "Type"] <- "Type III"
all1[all1$`Training-Tone` < 3 & all1$`Training-Shock` < 3, "Type"] <- "Type IV"
all1 <- gather(all1, key = "Phase", value = "Z", -Type)

f2d <- ggboxplot(all1, x = "Phase", y = "Z", color = "Phase", legend = "none",
                 ylab = "Z Score", font.label = list(size = 8)) +
    rotate_x_text(angle = 45)+
    stat_compare_means(method = "anova", label.y = 55, label.x = 2, paired = TRUE)+
    stat_compare_means(label = "p.signif", method = "t.test",
                       ref.group = "Training-Shock", label.y = 47, paired = TRUE)+
    facet_grid(.~Type)

figure2 <- ggarrange(ggarrange(f2a, f2b, f2c, labels = c("A", "B", "C"), ncol = 3),
                     f2d, labels = c("", "D"), nrow = 2)
figure2 <- annotate_figure(figure2, fig.lab = "Figure 2", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure2, filename = "figure 2.png", height = 9, width = 12)
