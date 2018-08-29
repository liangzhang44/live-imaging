library(tidyverse)
library(ggpubr)
library(EBImage)
library(zoo)

# figure 2a
img <- readImage("new figure 2a.png")
ggplot()+
    geom_blank()+
    background_image(img)

# insets
df <- data.frame(
    left = c(seq(1, length.out = 7, by = 2), seq(46, length.out = 5, by = 2),
             seq(67, length.out = 5, by = 2), seq(88, length.out = 5, by = 2),
             seq(109, length.out = 5, by = 2)),
    right = c(seq(2, length.out = 7, by = 2), seq(47, length.out = 5, by = 2),
              seq(68, length.out = 5, by = 2), seq(89, length.out = 5, by = 2),
              seq(110, length.out = 5, by = 2)),
    up = rep(2, 27),
    down = rep(-2, 27)
)

diagram1 <- ggplot(df, aes(xmin=left, xmax=right, ymin=down, ymax=up))+
    ylim(-7, 7)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("rect", xmin = 0, xmax = 15, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 45, xmax = 56, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 66, xmax = 77, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 87, xmax = 98, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 108, xmax = 119, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("segment", x = 1.5, xend = 1.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("segment", x = 15, xend = 45, y = 0, yend = 0)+
    annotate("segment", x = 56, xend = 66, y = 0, yend = 0)+
    annotate("segment", x = 77, xend = 87, y = 0, yend = 0)+
    annotate("segment", x = 98, xend = 108, y = 0, yend = 0)+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 4)
inset1 <- ggplotGrob(diagram1)

diagram2 <- ggplot(df, aes(xmin=left, xmax=right, ymin=down, ymax=up))+
    ylim(-7, 7)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("rect", xmin = 0, xmax = 15, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 45, xmax = 56, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 66, xmax = 77, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 87, xmax = 98, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 108, xmax = 119, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("segment", x = 13.5, xend = 13.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("segment", x = 15, xend = 45, y = 0, yend = 0)+
    annotate("segment", x = 56, xend = 66, y = 0, yend = 0)+
    annotate("segment", x = 77, xend = 87, y = 0, yend = 0)+
    annotate("segment", x = 98, xend = 108, y = 0, yend = 0)+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 4)
inset2 <- ggplotGrob(diagram2)

# figure 2b
cycle1 <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0001.csv")
cycle1 <- cycle1[,c(2,10,11)]
colnames(cycle1) <- c("Time", "Cell8", "Cell 9")
cycle1[5:(dim(cycle1)[1]-4),2] <- rollmean(cycle1[,2],9)
cycle1[5:(dim(cycle1)[1]-4),3] <- rollmean(cycle1[,3],9)
cycle1 <- gather(cycle1, key = "Cell", value = "Fluorescence", -Time)
ggplot(cycle1, aes(Time, Fluorescence, color=Cell))+
    geom_line()+
    scale_y_continuous(limits = c(300, 430))+
    annotation_custom(grob = inset1, xmin = 0, xmax = 65, ymin = 400, ymax = 430)+
    annotate("rect", xmin = 10, xmax = 25, ymin = 300, ymax = 395, fill="blue", alpha=0.2)+
    annotate("rect", xmin = 55.5, xmax = 56.5, ymin = 300, ymax = 395, fill="red", alpha=0.2)+
    annotate("text", x = 17.5, y = 400, label="Tone", color="blue", size=6)+
    annotate("text", x = 56, y = 400, label="Shock", color="red", size=6)+
    annotate("segment", x = 61, xend = 59, y = 398, yend = 385, arrow = arrow(length = unit(0.1, "inches")))+
    scale_color_brewer(palette = "Set1", guide=FALSE)+
    labs(x="Time (sec)", y="Fluorescence (AU)", title = "Session 1")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme_pubr()

# figure 2c
cycle7 <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0007.csv")
cycle7 <- cycle7[,c(2,10,11)]
colnames(cycle7) <- c("Time", "Cell8", "Cell9")

cycle7[5:(dim(cycle7)[1]-4),2] <- rollmean(cycle7[,2],9)
cycle7[5:(dim(cycle7)[1]-4),3] <- rollmean(cycle7[,3],9)
cycle7 <- cycle7[5:(dim(cycle7)[1]-4),]
temp <- filter(cycle7, Time < 10 | Time > 63)
model1 <- try(nls(Cell8 ~ SSasymp(Time, Asym, r0, lrc), data = temp), silent = TRUE)
model2 <- try(nls(Cell9 ~ SSasymp(Time, Asym, r0, lrc), data = temp), silent = TRUE)

curve <- cycle7
curve$Cell8 <- predict(model1, list(Time = cycle7$Time))
curve$Cell9 <- predict(model2, list(Time = cycle7$Time))

cycle7 <- gather(cycle7, key = "Cell", value = "Fluorescence", -Time)
curve <- gather(curve, key = "Cell", value = "Fluorescence", -Time)

ggplot(cycle7, aes(Time, Fluorescence, color=Cell))+
    geom_line()+
    geom_line(data = curve, aes(Time, Fluorescence), lty = 2, lwd = 2)+
    scale_y_continuous(limits = c(270, 375))+
    #annotation_custom(grob = inset2, xmin = 0, xmax = 65, ymin = 345, ymax = 375)+
    annotate("rect", xmin = 10, xmax = 25, ymin = 270, ymax = 340, fill="blue", alpha=0.2)+
    annotate("rect", xmin = 55.5, xmax = 56.5, ymin = 270, ymax = 340, fill="red", alpha=0.2)+
    annotate("text", x = 17.5, y = 345, label="Tone", color="blue", size=6)+
    annotate("text", x = 56, y = 345, label="Shock", color="red", size=6)+
    annotate("segment", x = 18, xend = 16, y = 335, yend = 323, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("segment", x = 60, xend = 58, y = 343, yend = 331, arrow = arrow(length = unit(0.1, "inches")))+
    scale_color_brewer(palette = "Set1", guide=FALSE)+
    labs(x="Time (sec)", y="Fluorescence (AU)", title = "Session 7")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme_pubr()

# figure 2d
mouse5.2 <- read.csv("./20161122 trace fear extinction 2/dF training late2.csv")
for(i in 1:(ncol(mouse5.2)-1)){
    #mouse5.2[,i+1] <- mouse5.2[,i+1]*100+3*(i-1)
    mouse5.2[5:(nrow(mouse5.2)-4),i+1] <- rollmean(mouse5.2[,i+1], 9)
}
mouse5.2 <- mouse5.2 %>%
    dplyr::select(time, cell1, cell2, cell3, cell5, cell8, cell9) %>%
    gather(key = "cell", value = "dF", -time) %>%
    mutate(cell=parse_number(cell))
mouse5.2$cell <- factor(mouse5.2$cell)
mouse5.2$dF <- mouse5.2$dF*100
f2d <- ggplot(mouse5.2, aes(x=time, y=dF, color=cell))+
    #geom_line()+
    geom_smooth(span = 0.1, se = FALSE)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -3, ymax = 3, fill="blue", alpha=0.4)+
    annotate("rect", xmin = 55, xmax = 57, ymin = -3, ymax = 3, fill="red", alpha=0.4)+
    annotate("text", x = 17.5, y = 3.5, label="Tone", color="blue", size=4)+
    annotate("text", x = 56, y = 3.5, label="Shock", color="red", size=4)+
    facet_wrap(~cell)+
    theme_pubr()+
    labs(x="Time (sec)", y=expression(Delta*"F/F (%)"))+
    theme(legend.position = "none", strip.text = element_blank())



figure2.top <- ggarrange(f2a, f2b, f2c, f2d, labels = c("A", "B", "C", "D"), nrow = 1, ncol = 4)
figure2.bottom <- ggarrange(f3a, f3b, labels = c("E", "F"), nrow = 1, ncol = 2, widths = c(3, 1))
figure2 <- ggarrange(figure2.top, figure2.bottom, labels = c("", ""), nrow = 2, ncol = 1)
figure2 <- annotate_figure(figure2, fig.lab = "Figure 2", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure2, filename = "new figure 2.png", height = 9, width = 18)
