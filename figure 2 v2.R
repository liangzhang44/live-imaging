library(tidyverse)
library(ggpubr)
library(EBImage)
library(zoo)
library(ggsci)
library(ez)

# figure 2a
img <- readImage("new figure 2a.png")
f2a <- ggplot()+
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

diagram1 <- ggplot(df, aes(xmin = left, xmax = right, ymin = down, ymax = up))+
    ylim(-7, 7.5)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("segment", x = 3.5, xend = 3.5, y = 6, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("text", x = 10, y = 7, label = "Session 2", size = 4)+
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

diagram2 <- ggplot(df, aes(xmin = left, xmax = right, ymin = down, ymax = up))+
    ylim(-7, 7.5)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("segment", x = 11.5, xend = 11.5, y = 6, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("text", x = 11.5, y = 7, label = "Session 6", size = 4)+
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

diagram3 <- ggplot(df, aes(xmin=left, xmax=right, ymin=down, ymax=up))+
    ylim(-7, 7)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("segment", x = 1.5, xend = 1.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("segment", x = 3.5, xend = 3.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("segment", x = 11.5, xend = 11.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")), color = "orange3")+
    annotate("segment", x = 13.5, xend = 13.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")), color = "orange3")+
    annotate("segment", x = 15, xend = 45, y = 0, yend = 0)+
    annotate("segment", x = 56, xend = 66, y = 0, yend = 0)+
    annotate("segment", x = 77, xend = 87, y = 0, yend = 0)+
    annotate("segment", x = 98, xend = 108, y = 0, yend = 0)+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 4)
inset3 <- ggplotGrob(diagram3)

# figure 2b
cycle2 <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0002.csv")
cycle2 <- cycle2[,c(2, 6, 7)]
colnames(cycle2) <- c("Time", "Cell4", "Cell5")

cycle2[5:(dim(cycle2)[1]-4),2] <- rollmean(cycle2[,2],9)
cycle2[5:(dim(cycle2)[1]-4),3] <- rollmean(cycle2[,3],9)
cycle2 <- cycle2[5:(dim(cycle2)[1]-4),]

temp1 <- filter(cycle2, Time < 10 | Time > 60)
model1 <- try(nls(Cell4 ~ SSasymp(Time, Asym, r0, lrc), data = temp1), silent = TRUE)
model2 <- try(nls(Cell5 ~ SSasymp(Time, Asym, r0, lrc), data = temp1), silent = TRUE)

curve1 <- cycle2
curve1$Cell4 <- predict(model1, list(Time = cycle2$Time))
curve1$Cell5 <- predict(model2, list(Time = cycle2$Time))

cycle2 <- gather(cycle2, key = "Cell", value = "Fluorescence", -Time)
curve1 <- gather(curve1, key = "Cell", value = "Fluorescence", -Time)

f2b <- ggplot(cycle2, aes(Time, Fluorescence, color = Cell))+
    #geom_line()+
    geom_smooth(span = 0.05, se = FALSE)+
    geom_line(data = curve1, aes(Time, Fluorescence), lty = 2, lwd = 1.5)+
    scale_y_continuous(limits = c(340, 430))+
    annotation_custom(grob = inset1, xmin = 0, xmax = 65, ymin = 397, ymax = 427)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -Inf, ymax = 400, fill = "blue", alpha = 0.4)+
    annotate("rect", xmin = 55.5, xmax = 56.5, ymin = -Inf, ymax = 400, fill = "red", alpha = 0.4)+
    annotate("text", x = 17.5, y = 397, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 56, y = 397, label = "Shock", color = "red", size = 5)+
    annotate("text", x = 62, y = 370, label = "a", color = "red", size = 6)+
    annotate("text", x = 62, y = 348, label = "b", color = "blue", size = 6)+
    annotate("segment", x = 11, xend = 18, y = 388, yend = 388,
             arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    scale_color_brewer(palette = "Set1", guide = FALSE)+
    labs(x = "Time (sec)", y = "Fluorescence (AU)")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme_pubr()

# figure 2c
cycle7 <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0006.csv")
cycle7 <- cycle7[,c(2, 6, 7)]
colnames(cycle7) <- c("Time", "Cell4", "Cell5")

cycle7[5:(dim(cycle7)[1]-4),2] <- rollmean(cycle7[,2],9)
cycle7[5:(dim(cycle7)[1]-4),3] <- rollmean(cycle7[,3],9)
cycle7 <- cycle7[5:(dim(cycle7)[1]-4),]

temp2 <- filter(cycle7, Time < 10 | Time > 60)
model1 <- try(nls(Cell4 ~ SSasymp(Time, Asym, r0, lrc), data = temp2), silent = TRUE)
model2 <- try(nls(Cell5 ~ SSasymp(Time, Asym, r0, lrc), data = temp2), silent = TRUE)

curve2 <- cycle7
curve2$Cell4 <- predict(model1, list(Time = cycle7$Time))
curve2$Cell5 <- predict(model2, list(Time = cycle7$Time))

cycle7 <- gather(cycle7, key = "Cell", value = "Fluorescence", -Time)
curve2 <- gather(curve2, key = "Cell", value = "Fluorescence", -Time)

f2c <- ggplot(cycle7, aes(Time, Fluorescence, color=Cell))+
    #geom_line()+
    geom_smooth(span = 0.05, se = FALSE)+
    geom_line(data = curve2, aes(Time, Fluorescence), lty = 2, lwd = 1.5)+
    scale_y_continuous(limits = c(305, 390))+
    annotation_custom(grob = inset2, xmin = 0, xmax = 65, ymin = 357, ymax = 387)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -Inf, ymax = 360, fill = "blue", alpha = 0.4)+
    annotate("rect", xmin = 55.5, xmax = 56.5, ymin = -Inf, ymax = 360, fill = "red", alpha = 0.4)+
    annotate("text", x = 17.5, y = 357, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 56, y = 357, label = "Shock", color = "red", size = 5)+
    annotate("text", x = 62, y = 335, label = "a", color = "red", size = 6)+
    annotate("text", x = 62, y = 310, label = "b", color = "blue", size = 6)+
    annotate("segment", x = 15, xend = 47, y = 353, yend = 340, 
             arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    annotate("segment", x = 16, xend = 25, y = 335, yend = 330, 
             arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    annotate("segment", x = 58, xend = 62, y = 323, yend = 323, 
             arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    scale_color_brewer(palette = "Set1", guide = FALSE)+
    labs(x = "Time (sec)", y = "Fluorescence (AU)")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme_pubr()


allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# read dF/F data of training, first 2 cycles
early2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF training early2.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    early2 <- rbind(early2, temp)
}
early2$phase <- "Sessions 1&2"

# read dF/F data of training, last 2 cycles
late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF training late2.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    late2 <- rbind(late2, temp)
}
late2$phase <- "Sessions 6&7"

training.all <- rbind(early2, late2)

# figure 2d
training.tone <- training.all %>%
    filter(time > 5, time <55) %>%
    mutate(z = NA)

training.tone.mean <- training.tone %>%
    group_by(time, phase) %>%
    summarise(dF = mean(dF))

early.base.mean <- mean(training.tone.mean$dF[training.tone.mean$phase == "Sessions 1&2" & training.tone.mean$time < 10])
early.base.sd <- sd(training.tone.mean$dF[training.tone.mean$phase == "Sessions 1&2" & training.tone.mean$time < 10])
training.tone$z[training.tone$phase == "Sessions 1&2"] <- 
    (training.tone$dF[training.tone$phase == "Sessions 1&2"] - early.base.mean)/early.base.sd

late.base.mean <- mean(training.tone.mean$dF[training.tone.mean$phase == "Sessions 6&7" & training.tone.mean$time < 10])
late.base.sd <- sd(training.tone.mean$dF[training.tone.mean$phase == "Sessions 6&7" & training.tone.mean$time < 10])
training.tone$z[training.tone$phase == "Sessions 6&7"] <- 
    (training.tone$dF[training.tone$phase == "Sessions 6&7"] - late.base.mean)/late.base.sd

tone.aov <- ezANOVA(data = training.tone, dv = z, wid = cell, within_full = time,
                    between = phase, type = 2, return_aov = TRUE)
tone.aov$ANOVA # p = 0.004
#summary(glht(tone.aov$aov,linfct=mcp(phase="Tukey")))
#TukeyHSD(tone.aov$aov)

f2d <- ggline(training.tone, x = "time", y = "z", add = "mean_se", color = "phase", 
              numeric.x.axis = TRUE, size = 0.2)+
    #stat_compare_means(aes(group = phase), label = "p.signif", method = "t.test",
    #hide.ns = TRUE, label.y = 10, paired = TRUE)+
    annotation_custom(grob = inset3, xmin = 25, xmax = 55, ymin = 9, ymax = 15)+
    annotate("rect", xmin = 5, xmax = 55, ymin = -7, ymax = -5, color = "black", fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -7, ymax = -5, alpha = 1, fill = "blue")+
    annotate("segment", x = 55, xend = 55, y = -7, yend = -5, color = "red")+
    annotate("text", x = 17.5, y = -6, label = "Tone", color = "white", size = 4)+
    annotate("text", x = 55, y = -6, label = "Shock", color = "red", size = 4)+
    annotate("text", x = 40, y = -6, label = "Trace", size = 4)+
    geom_vline(xintercept = 10, lty = 2, color = "blue")+
    geom_vline(xintercept = 25, lty = 2, color = "blue")+
    geom_vline(xintercept = 55, lty = 2, color = "red")+
    #annotate("segment", x = 18, xend = 55, y = 13.5, yend = 13.5)+
    #annotate("text", x = 36, y = 14, label="*", size=6)+
    labs(x = "Time (sec)", y= "Z Score")+
    scale_color_jama()+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          legend.title = element_blank(), legend.text = element_text(size = 14),
          legend.justification = c(0.43, 1))

# figure 2e
training.shock <- training.all %>%
    filter(time > 51) %>%
    mutate(z = NA)

training.shock.mean <- training.shock %>%
    group_by(time, phase) %>%
    summarise(dF = mean(dF))

early.shock.mean <- mean(training.shock.mean$dF[training.shock.mean$phase == "Sessions 1&2" & training.shock.mean$time < 56])
early.shock.sd <- sd(training.shock.mean$dF[training.shock.mean$phase == "Sessions 1&2" & training.shock.mean$time < 56])
training.shock$z[training.shock$phase == "Sessions 1&2"] <- 
    (training.shock$dF[training.shock$phase == "Sessions 1&2"] - early.shock.mean)/early.shock.sd

late.shock.mean <- mean(training.shock.mean$dF[training.shock.mean$phase == "Sessions 6&7" & training.shock.mean$time < 56])
late.shock.sd <- sd(training.shock.mean$dF[training.shock.mean$phase == "Sessions 6&7" & training.shock.mean$time < 56])
training.shock$z[training.shock$phase == "Sessions 6&7"] <- 
    (training.shock$dF[training.shock$phase == "Sessions 6&7"] - late.shock.mean)/late.shock.sd

shock.aov <- ezANOVA(data = training.shock, dv = z, wid = cell, within_full = time,
                     between = phase, type = 2, return_aov = TRUE)
shock.aov$ANOVA # p = 0.063

f2e <- ggline(training.shock, x = "time", y = "z", add = "mean_se", color = "phase", 
              numeric.x.axis = TRUE, size = 0.2)+
    #stat_compare_means(label = "p.signif", aes(group = phase), hide.ns = TRUE, label.y = 4, paired = TRUE)+
    annotate("rect", xmin = 50, xmax = 65, ymin = -5, ymax = -4, color = "black", fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 55, xmax = 56, ymin = -5, ymax = -4, alpha = 0.5, fill = "red")+
    annotate("text", x = 55.5, y = -4.5, label = "Shock", color = "red", size = 4)+
    annotate("text", x = 52.5, y = -4.5, label = "Trace", size = 4)+
    geom_vline(xintercept = 55, lty = 2, color = "red")+
    geom_vline(xintercept = 56, lty = 2, color = "red")+
    labs(x="Time (sec)", y="Z Score")+
    scale_color_jama()+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          legend.title = element_blank(), legend.text = element_text(size = 14))

figure2.top <- ggarrange(f2a, f2b, f2c, labels = c("A", "B", "C"), nrow = 1, ncol = 3)
figure2.bottom <- ggarrange(f2d, f2e, labels = c("D", "E"), nrow = 1, ncol = 2, widths = c(2, 1))
figure2 <- ggarrange(figure2.top, figure2.bottom, labels = c("", ""), nrow = 2, ncol = 1)
figure2 <- annotate_figure(figure2, fig.lab = "Figure 2", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure2, filename = "new figure 2.png", height = 8, width = 12)
