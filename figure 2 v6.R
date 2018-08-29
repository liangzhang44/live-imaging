library(tidyverse)
library(ggpubr)
library(EBImage)
library(zoo)
library(ggsci)
library(ez)
library(multcomp)
source("normalization.dF.R")

# figure 2a
img2a <- readImage("figure 2a.png")
f2a <- ggplot()+
    geom_blank()+
    background_image(img2a)

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

# diagram for f2b
diagram1 <- ggplot(df, aes(xmin = left, xmax = right, ymin = down, ymax = up))+
    ylim(-6, 10)+
    xlim(-20, 120)+
    theme_void()+
    geom_rect(fill = c(rep("green", 7), rep("orange", 20)))+
    annotate("segment", x = 3.5, xend = 3.5, y = 6, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("text", x = 10, y = 7, label = "Session 2", size = 5)+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = 55, xend = 67, y = 0, yend = 0)+
    annotate("segment", x = 76, xend = 88, y = 0, yend = 0)+
    annotate("segment", x = 97, xend = 109, y = 0, yend = 0)+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 4)

#diagram for f2c
diagram2 <- ggplot(df, aes(xmin = left, xmax = right, ymin = down, ymax = up))+
    ylim(-6, 10)+
    xlim(-20, 120)+
    theme_void()+
    geom_rect(fill = c(rep("green", 7), rep("orange", 20)))+
    annotate("segment", x = 11.5, xend = 11.5, y = 6, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("text", x = 11.5, y = 7, label = "Session 6", size = 5)+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = 55, xend = 67, y = 0, yend = 0)+
    annotate("segment", x = 76, xend = 88, y = 0, yend = 0)+
    annotate("segment", x = 97, xend = 109, y = 0, yend = 0)+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 4)

#diagram for f2d
pal <- pal_jama("default")(7)
diagram3 <- ggplot(df, aes(xmin=left, xmax=right, ymin=down, ymax=up))+
    ylim(-6, 10)+
    xlim(-70, 170)+
    theme_void()+
    geom_rect(fill = c(rep("green", 7), rep("orange", 20)))+
    annotate("segment", x = 1.5, xend = 1.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.1, "inches")), color = pal[1])+
    annotate("segment", x = 3.5, xend = 3.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.1, "inches")), color = pal[1])+
    annotate("segment", x = 11.5, xend = 11.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.1, "inches")), color = pal[2])+
    annotate("segment", x = 13.5, xend = 13.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.1, "inches")), color = pal[2])+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = 55, xend = 67, y = 0, yend = 0)+
    annotate("segment", x = 76, xend = 88, y = 0, yend = 0)+
    annotate("segment", x = 97, xend = 109, y = 0, yend = 0)+
    annotate("text", x = 2.5, y = 7.5, label = "1&2", size = 5, color = pal[1])+
    annotate("text", x = 12.5, y = 7.5, label = "6&7", size = 5, color = pal[2])+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 4)

# figure 2b
cycle2 <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0002.csv")
cycle2 <- cycle2[,c(2, 6, 7)]
colnames(cycle2) <- c("Time", "Cell4", "Cell5")
# calculate moving average
cycle2[2:(dim(cycle2)[1]-1),2] <- rollmean(cycle2[,2],3)
cycle2[2:(dim(cycle2)[1]-1),3] <- rollmean(cycle2[,3],3)
cycle2 <- cycle2[2:(dim(cycle2)[1]-1),]
# fit models
temp1 <- filter(cycle2, Time < 10 | Time > 63)
model1 <- try(nls(Cell4 ~ SSasymp(Time, Asym, r0, lrc), data = temp1), silent = TRUE)
model2 <- try(nls(Cell5 ~ SSasymp(Time, Asym, r0, lrc), data = temp1), silent = TRUE)
# calculate fitted curves
curve1 <- cycle2
curve1$Cell4 <- predict(model1, list(Time = cycle2$Time))
curve1$Cell5 <- predict(model2, list(Time = cycle2$Time))

cycle2 <- gather(cycle2, key = "Cell", value = "Fluorescence", -Time)
curve1 <- gather(curve1, key = "Cell", value = "Fluorescence", -Time)

f2b2 <- ggplot(cycle2, aes(Time, Fluorescence, color = Cell))+
    geom_smooth(span = 0.05, se = FALSE)+
    geom_line(data = curve1, aes(Time, Fluorescence), lty = 2, lwd = 1.5)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -Inf, ymax = 400, fill = "blue", alpha = 0.4)+
    annotate("rect", xmin = 55.5, xmax = 56.5, ymin = -Inf, ymax = 400, fill = "red", alpha = 0.4)+
    annotate("text", x = 17.5, y = 403, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 56, y = 403, label = "Shock", color = "red", size = 5)+
    annotate("text", x = 62, y = 370, label = "Cell a", color = "darkblue", size = 5)+
    annotate("text", x = 62, y = 346, label = "Cell b", color = "red", size = 5)+
    annotate("text", x = 32.5, y = 410, label = "Training (Session 2)", size = 6)+
    annotate("segment", x = 10.5, xend = 17.5, y = 389, yend = 389,
             arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    scale_color_aaas(guide = FALSE)+
    labs(x = "Time (sec)", y = "Fluorescence (AU)")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme_pubr()

f2b <- ggarrange(diagram1, f2b2, ncol = 1, nrow = 2, heights = c(1, 3))

# figure 2c
cycle6 <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0006.csv")
cycle6 <- cycle6[,c(2, 6, 7)]
colnames(cycle6) <- c("Time", "Cell4", "Cell5")
# calculate moving average
cycle6[2:(dim(cycle6)[1]-1),2] <- rollmean(cycle6[,2],3)
cycle6[2:(dim(cycle6)[1]-1),3] <- rollmean(cycle6[,3],3)
cycle6 <- cycle6[2:(dim(cycle6)[1]-1),]
# fit models
temp2 <- filter(cycle6, Time < 10 | Time > 63)
model1 <- try(nls(Cell4 ~ SSasymp(Time, Asym, r0, lrc), data = temp2), silent = TRUE)
model2 <- try(nls(Cell5 ~ SSasymp(Time, Asym, r0, lrc), data = temp2), silent = TRUE)
# calculate fitted curves
curve2 <- cycle6
curve2$Cell4 <- predict(model1, list(Time = cycle6$Time))
curve2$Cell5 <- predict(model2, list(Time = cycle6$Time))

cycle6 <- gather(cycle6, key = "Cell", value = "Fluorescence", -Time)
curve2 <- gather(curve2, key = "Cell", value = "Fluorescence", -Time)

f2c2 <- ggplot(cycle6, aes(Time, Fluorescence, color=Cell))+
    geom_smooth(span = 0.05, se = FALSE)+
    geom_line(data = curve2, aes(Time, Fluorescence), lty = 2, lwd = 1.5)+
    #scale_y_continuous(limits = c(305, 370))+
    annotate("rect", xmin = 10, xmax = 25, ymin = -Inf, ymax = 360, fill = "blue", alpha = 0.4)+
    annotate("rect", xmin = 55.5, xmax = 56.5, ymin = -Inf, ymax = 360, fill = "red", alpha = 0.4)+
    annotate("text", x = 17.5, y = 363, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 56, y = 363, label = "Shock", color = "red", size = 5)+
    annotate("text", x = 62, y = 335, label = "Cell a", color = "darkblue", size = 5)+
    annotate("text", x = 62, y = 308, label = "Cell b", color = "red", size = 5)+
    annotate("text", x = 32.5, y = 370, label = "Training (Session 6)", size = 6)+
    annotate("segment", x = 15, xend = 47, y = 353, yend = 340, 
             arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    annotate("segment", x = 16, xend = 25, y = 335, yend = 330, 
             arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    annotate("segment", x = 58, xend = 62, y = 323, yend = 323, 
             arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    scale_color_aaas(guide = FALSE)+
    labs(x = "Time (sec)", y = "Fluorescence (AU)")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme_pubr()

f2c <- ggarrange(diagram2, f2c2, ncol = 1, nrow = 2, heights = c(1, 3))


# normalize training data, first 2 sessions
early2 <- normalization.dF("dF training early2.csv")
early2 <- mutate(early2, phase = "Training (Sessions 1&2)")

# normalize training data, last 2 sessions
late2 <- normalization.dF("dF training late2.csv")
late2 <- mutate(late2, phase = "Training (Sessions 6&7)")

training <- rbind(early2, late2)

#tone.aov <- ezANOVA(data = training, dv = z, wid = cell, within_full = time,
#                    between = phase, type = 2, return_aov = TRUE)
#tone.aov$ANOVA # p = 0.016
#summary(glht(tone.aov$aov,linfct=mcp(phase="Tukey")))

pvalues <- compare_means(z ~ phase, data = training, 
              group.by = "time", paired = TRUE, p.adjust.method = "BH")
pvalues <- filter(pvalues, p.adj < 0.05)

# figure 2d
f2d2 <- ggline(training, x = "time", y = "z", add = "mean_se", color = "phase", 
              numeric.x.axis = TRUE, size = 0.2)+
    annotate("rect", xmin = 0, xmax = 65, ymin = -13, ymax = -11, fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -13, ymax = -11, fill = "blue")+
    annotate("rect", xmin = 55, xmax = 56, ymin = -13, ymax = -11, fill = "red")+
    annotate("text", x = 17.5, y = -10, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 55.5, y = -10, label = "Shock", color = "red", size = 5)+
    annotate("text", x = 40, y = -10, label = "Trace", size = 5)+
    annotate("text", x = 63, y = -4, label = "Training (1&2)", size = 5, color = pal[1])+
    annotate("text", x = 63, y = 6, label = "Training (6&7)", size = 5, color = pal[2])+
    geom_vline(xintercept = 10, lty = 2, color = "blue")+
    geom_vline(xintercept = 25, lty = 2, color = "blue")+
    geom_vline(xintercept = 55, lty = 2, color = "red")+
    geom_vline(xintercept = 56, lty = 2, color = "red")+
    geom_hline(yintercept = 0, lty = 2)+
    annotate("text", x = pvalues$time, y = 10, label = "*", size = 5)+
    labs(x = "Time (sec)", y= "Z Score")+
    scale_color_jama(guide = FALSE)+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          legend.title = element_blank(), legend.text = element_text(size = 14))

f2d <- ggarrange(diagram3, f2d2, ncol = 1, nrow = 2, heights = c(1, 3))

figure2.top <- ggarrange(f2a, f2b, f2c, labels = c("A", "B", "C"), nrow = 1, ncol = 3)
figure2 <- ggarrange(figure2.top, f2d, labels = c("", "D"), nrow = 2, ncol = 1)
figure2 <- annotate_figure(figure2, fig.lab = "Figure 2", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure2, filename = "figure 2.pdf", height = 8, width = 12)