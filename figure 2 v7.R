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
    annotate("segment", x = 3.5, xend = 3.5, y = 6, yend = 2.5, 
             arrow = arrow(length = unit(0.2, "cm")))+
    annotate("text", x = 10, y = 8, label = "Session 2", size = 3)+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = 55, xend = 67, y = 0, yend = 0)+
    annotate("segment", x = 76, xend = 88, y = 0, yend = 0)+
    annotate("segment", x = 97, xend = 109, y = 0, yend = 0)+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 2.5)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 2.5)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 2.5)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 2.5)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 2.5)

#diagram for f2c
diagram2 <- ggplot(df, aes(xmin = left, xmax = right, ymin = down, ymax = up))+
    ylim(-6, 10)+
    xlim(-20, 120)+
    theme_void()+
    geom_rect(fill = c(rep("green", 7), rep("orange", 20)))+
    annotate("segment", x = 11.5, xend = 11.5, y = 6, yend = 2.5, 
             arrow = arrow(length = unit(0.2, "cm")))+
    annotate("text", x = 11.5, y = 8, label = "Session 6", size = 3)+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = 55, xend = 67, y = 0, yend = 0)+
    annotate("segment", x = 76, xend = 88, y = 0, yend = 0)+
    annotate("segment", x = 97, xend = 109, y = 0, yend = 0)+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 2.5)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 2.5)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 2.5)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 2.5)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 2.5)

#diagram for f2d
pal <- pal_uchicago("default")(7)
diagram3 <- ggplot(df, aes(xmin = left, xmax = right, ymin = down, ymax = up))+
    ylim(-6, 10)+
    xlim(-70, 170)+
    theme_void()+
    geom_rect(fill = c(rep("green", 7), rep("orange", 20)))+
    annotate("segment", x = 1.5, xend = 1.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.2, "cm")), color = pal[2])+
    annotate("segment", x = 3.5, xend = 3.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.2, "cm")), color = pal[2])+
    annotate("segment", x = 11.5, xend = 11.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.2, "cm")), color = pal[1])+
    annotate("segment", x = 13.5, xend = 13.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.2, "cm")), color = pal[1])+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = 55, xend = 67, y = 0, yend = 0)+
    annotate("segment", x = 76, xend = 88, y = 0, yend = 0)+
    annotate("segment", x = 97, xend = 109, y = 0, yend = 0)+
    annotate("text", x = 2.5, y = 7.5, label = "1&2", size = 3, color = pal[2])+
    annotate("text", x = 12.5, y = 7.5, label = "6&7", size = 3, color = pal[1])+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 4)

# figure 2b
cycle2 <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0002.csv")
cycle2 <- cycle2[,c(2, 6, 7)]
colnames(cycle2) <- c("Time", "Cell4", "Cell5")

for (i in 2:3) {
    thesub <- cycle2[,c(1,i)]
    names(thesub) <- c('time','signal')
    # calculate moving average
    thesub$signal[2:(dim(thesub)[1]-1)] <- rollmean(thesub$signal, 3)
    temp <- filter(thesub, time < 10 | time > 63)
    # fit models
    model <- try(nls(signal ~ SSasymp(time, Asym, r0, lrc), data = temp), silent = TRUE)
    # calculate fitted curves
    fit <- predict(model, list(time = thesub$time))
    cycle2[,i] <- thesub$signal - fit
    baseline <- mean(cycle2[cycle2$Time < 10, i])
    cycle2[,i] <- (cycle2[,i] - baseline)/model$m$getPars()["Asym"]*100
}

cycle2 <- gather(cycle2, key = "Cell", value = "Fluorescence", -Time)

f2b2 <- ggplot(cycle2, aes(Time, Fluorescence, color = Cell))+
    geom_smooth(span = 0.1, se = FALSE, size = 0.8)+
    annotate("rect", xmin = 0, xmax = 65, ymin = -7, ymax = -5.5, fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -7, ymax = -5.5, fill = "blue")+
    annotate("segment", x = 56, xend = 56, y = -7, yend = -5.5, color = "red")+
    annotate("text", x = 17.5, y = -4.5, label = "Tone", color = "blue", size = 3)+
    annotate("text", x = 55.5, y = -4.5, label = "Shock", color = "red", size = 3)+
    annotate("text", x = 40, y = -4.5, label = "Trace", size = 3)+
    geom_vline(xintercept = 10, lty = 2, color = "blue")+
    geom_vline(xintercept = 25, lty = 2, color = "blue")+
    geom_vline(xintercept = 56, lty = 2, color = "red")+
    geom_hline(yintercept = 0, lty = 2)+
    annotate("text", x = 40, y = 3, label = "Cell a", color = "darkblue", size = 3)+
    annotate("text", x = 40, y = -2, label = "Cell b", color = "red", size = 3)+
    expand_limits(y = 5)+
    #annotate("segment", x = 10.5, xend = 17.5, y = 389, yend = 389,
    #         arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    scale_color_aaas(guide = FALSE)+
    labs(x = "Time (sec)", y = expression(Delta*"F/F (%)"), title = "Training (Session 2)")+
    theme_pubr()+
    theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8),
          plot.title = element_text(size = 10, hjust = 0.5))

f2b <- ggarrange(diagram1, f2b2, ncol = 1, nrow = 2, heights = c(1, 3))

# figure 2c
cycle6 <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0006.csv")
cycle6 <- cycle6[,c(2, 6, 7)]
colnames(cycle6) <- c("Time", "Cell4", "Cell5")

for (i in 2:3) {
    thesub <- cycle6[,c(1,i)]
    names(thesub) <- c('time','signal')
    # calculate moving average
    thesub$signal[2:(dim(thesub)[1]-1)] <- rollmean(thesub$signal, 3)
    temp <- filter(thesub, time < 10 | time > 63)
    # fit models
    model <- try(nls(signal ~ SSasymp(time, Asym, r0, lrc), data = temp), silent = TRUE)
    # calculate fitted curves
    fit <- predict(model, list(time = thesub$time))
    cycle6[,i] <- thesub$signal - fit
    baseline <- mean(cycle6[cycle6$Time < 10, i])
    cycle6[,i] <- (cycle6[,i] - baseline)/model$m$getPars()["Asym"]*100
}

cycle6 <- gather(cycle6, key = "Cell", value = "Fluorescence", -Time)

f2c2 <- ggplot(cycle6, aes(Time, Fluorescence, color = Cell))+
    geom_smooth(span = 0.1, se = FALSE, size = 0.8)+
    annotate("rect", xmin = 0, xmax = 65, ymin = -7, ymax = -5.5, fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -7, ymax = -5.5, fill = "blue")+
    annotate("segment", x = 56, xend = 56, y = -7, yend = -5.5, color = "red")+
    annotate("text", x = 17.5, y = -4.5, label = "Tone", color = "blue", size = 3)+
    annotate("text", x = 55.5, y = -4.5, label = "Shock", color = "red", size = 3)+
    annotate("text", x = 40, y = -4.5, label = "Trace", size = 3)+
    geom_vline(xintercept = 10, lty = 2, color = "blue")+
    geom_vline(xintercept = 25, lty = 2, color = "blue")+
    geom_vline(xintercept = 56, lty = 2, color = "red")+
    geom_hline(yintercept = 0, lty = 2)+
    annotate("text", x = 42, y = 3, label = "Cell a", color = "darkblue", size = 3)+
    annotate("text", x = 42, y = -0.5, label = "Cell b", color = "red", size = 3)+
    expand_limits(y = 5)+
    #annotate("segment", x = 15, xend = 47, y = 353, yend = 340, 
    #         arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    #annotate("segment", x = 16, xend = 25, y = 335, yend = 330, 
    #         arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    #annotate("segment", x = 58, xend = 62, y = 323, yend = 323, 
    #         arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    scale_color_aaas(guide = FALSE)+
    labs(x = "Time (sec)", y = expression(Delta*"F/F (%)"), title = "Training (Session 6)")+
    theme_pubr()+
    theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8),
          plot.title = element_text(size = 10, hjust = 0.5))
    
f2c <- ggarrange(diagram2, f2c2, ncol = 1, nrow = 2, heights = c(1, 3))


# normalize training data, first 2 sessions
early2 <- normalization.dF("dF training early2.csv") %>%
    mutate(phase = "Sessions 1&2")

# normalize training data, last 2 sessions
late2 <- normalization.dF("dF training late2.csv") %>%
    mutate(phase = "Sessions 6&7")

# normalize training data, session 8
#control <- normalization.dF("dF training session8.csv") %>%
#    mutate(phase = "Session 8")

training <- rbind(early2, late2)
training$phase <- factor(training$phase, 
                         levels = c("Sessions 6&7", "Sessions 1&2"))
#tone.aov <- ezANOVA(data = training, dv = z, wid = cell, within_full = time,
#                    between = phase, type = 2, return_aov = TRUE)
#tone.aov$ANOVA # p = 0.015, F(1,158) = 6.05
#summary(glht(tone.aov$aov,linfct=mcp(phase="Tukey")))

pvalues <- compare_means(z ~ phase, data = training[training$phase %in% c("Sessions 1&2", "Sessions 6&7"),], 
                         group.by = "time", paired = TRUE, p.adjust.method = "BH")
pvalues <- filter(pvalues, p.adj < 0.05)

# figure 2d
f2d2 <- ggplot(training, aes(time, z, color = phase))+
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, size = 0.1) +
    stat_summary(fun.y = mean, geom = "point", size = 0.2)+
    annotate("rect", xmin = 0, xmax = 65, ymin = -8, ymax = -6, fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -8, ymax = -6, fill = "blue")+
    annotate("rect", xmin = 55, xmax = 56, ymin = -8, ymax = -6, fill = "red")+
    annotate("text", x = 17.5, y = -4.5, label = "Tone", color = "blue", size = 4)+
    annotate("text", x = 55.5, y = -4.5, label = "Shock", color = "red", size = 4)+
    annotate("text", x = 40, y = -4.5, label = "Trace", size = 4)+
    #annotate("text", x = 63, y = -4, label = "Training (1&2)", size = 5, color = pal[1])+
    #annotate("text", x = 63, y = 6, label = "Training (6&7)", size = 5, color = pal[2])+
    geom_vline(xintercept = 10, lty = 2, color = "blue")+
    geom_vline(xintercept = 25, lty = 2, color = "blue")+
    geom_vline(xintercept = 55, lty = 2, color = "red")+
    geom_vline(xintercept = 56, lty = 2, color = "red")+
    geom_hline(yintercept = 0, lty = 2)+
    annotate("text", x = pvalues$time, y = 10, label = "*", size = 4)+
    labs(x = "Time (sec)", y= "Z Score")+
    scale_color_uchicago()+
    theme_pubr()+
    theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10),
          legend.title = element_blank(), legend.text = element_text(size = 8),
          legend.position = "right")

f2d <- ggarrange(diagram3, f2d2, ncol = 1, nrow = 2, heights = c(1, 3))

figure2.top <- ggarrange(f2a, f2b, f2c, labels = c("A", "B", "C"), nrow = 1, ncol = 3)
figure2 <- ggarrange(figure2.top, f2d, labels = c("", "D"), nrow = 2, ncol = 1)
figure2 <- annotate_figure(figure2, fig.lab = "Figure 2", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure2, filename = "figure 2.pdf", height = 11.7, width = 17.6, units = "cm")
