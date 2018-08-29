library(tidyverse)
library(ggpubr)
library(ggsci)
library(multcomp)
library(ez)

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

diagram3 <- ggplot(df, aes(xmin=left, xmax=right, ymin=down, ymax=up))+
    ylim(-7, 7)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("rect", xmin = 0, xmax = 15, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 45, xmax = 56, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 66, xmax = 77, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 87, xmax = 98, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("rect", xmin = 108, xmax = 119, ymin = -2.5, ymax = 2.5, alpha = 0, color = "black", lty = 2)+
    annotate("segment", x = 1.5, xend = 1.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("segment", x = 3.5, xend = 3.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("segment", x = 11.5, xend = 11.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")), color = "orange")+
    annotate("segment", x = 13.5, xend = 13.5, y = 6.5, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")), color = "orange")+
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
early2$phase <- "Session 1/2"

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
late2$phase <- "Session 6/7"

training.all <- rbind(early2, late2)

# figure 3a
training.tone <- training.all %>%
    filter(time > 5, time <55) %>%
    mutate(z = NA)

training.tone.mean <- training.tone %>%
    group_by(time, phase) %>%
    summarise(dF = mean(dF))

early.base.mean <- mean(training.tone.mean$dF[training.tone.mean$phase == "Session 1/2" & training.tone.mean$time < 10])
early.base.sd <- sd(training.tone.mean$dF[training.tone.mean$phase == "Session 1/2" & training.tone.mean$time < 10])
training.tone$z[training.tone$phase == "Session 1/2"] <- 
    (training.tone$dF[training.tone$phase == "Session 1/2"] - early.base.mean)/early.base.sd

late.base.mean <- mean(training.tone.mean$dF[training.tone.mean$phase == "Session 6/7" & training.tone.mean$time < 10])
late.base.sd <- sd(training.tone.mean$dF[training.tone.mean$phase == "Session 6/7" & training.tone.mean$time < 10])
training.tone$z[training.tone$phase == "Session 6/7"] <- 
    (training.tone$dF[training.tone$phase == "Session 6/7"] - late.base.mean)/late.base.sd

tone.aov <- ezANOVA(data = training.tone, dv = z, wid = cell, within_full = time,
                    between = phase, type = 2, return_aov = TRUE)
tone.aov$ANOVA # p = 0.036
#summary(glht(tone.aov$aov,linfct=mcp(phase="Tukey")))
#TukeyHSD(tone.aov$aov)

f3a <- ggline(training.tone, x = "time", y = "z", add = "mean_se", color = "phase", 
       numeric.x.axis = TRUE, size = 0.2)+
    #stat_compare_means(aes(group = phase), label = "p.signif", method = "t.test",
                       #hide.ns = TRUE, label.y = 10, paired = TRUE)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -7, ymax = 13, fill = "blue", alpha = 0.3)+
    annotate("text", x = 17.5, y = 13.5, label = "Tone", color = "blue", size = 8)+
    annotation_custom(grob = inset3, xmin = 35, xmax = 55, ymin = 10, ymax = 15)+
    #annotate("segment", x = 18, xend = 55, y = 13.5, yend = 13.5)+
    #annotate("text", x = 36, y = 14, label="*", size=6)+
    labs(x = "Time (sec)", y= "Z Score")+
    scale_color_jama()+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          legend.title = element_blank(), legend.text = element_text(size = 14))

# figure 3b
training.shock <- training.all %>%
    filter(time > 51) %>%
    mutate(z = NA)

training.shock.mean <- training.shock %>%
    group_by(time, phase) %>%
    summarise(dF = mean(dF))

early.shock.mean <- mean(training.shock.mean$dF[training.shock.mean$phase == "Session 1/2" & training.shock.mean$time < 56])
early.shock.sd <- sd(training.shock.mean$dF[training.shock.mean$phase == "Session 1/2" & training.shock.mean$time < 56])
training.shock$z[training.shock$phase == "Session 1/2"] <- 
    (training.shock$dF[training.shock$phase == "Session 1/2"] - early.shock.mean)/early.shock.sd

late.shock.mean <- mean(training.shock.mean$dF[training.shock.mean$phase == "Session 6/7" & training.shock.mean$time < 56])
late.shock.sd <- sd(training.shock.mean$dF[training.shock.mean$phase == "Session 6/7" & training.shock.mean$time < 56])
training.shock$z[training.shock$phase == "Session 6/7"] <- 
    (training.shock$dF[training.shock$phase == "Session 6/7"] - late.shock.mean)/late.shock.sd

shock.aov <- ezANOVA(data = training.shock, dv = z, wid = cell, within_full = time,
                    between = phase, type = 2, return_aov = TRUE)
shock.aov$ANOVA # p = 0.063

f3b <- ggline(training.shock, x = "time", y = "z", add = "mean_se", color = "phase", 
       numeric.x.axis = TRUE, size = 0.2)+
    #stat_compare_means(label = "p.signif", aes(group = phase), hide.ns = TRUE, label.y = 4, paired = TRUE)+
    annotate("rect", xmin = 55, xmax = 56, ymin = -5, ymax = 5, fill = "red", alpha = 0.3)+
    annotate("text", x = 55.5, y = 5.5, label = "Shock", color = "red", size = 8)+
    labs(x="Time (sec)", y="Z Score")+
    scale_color_jama()+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          legend.title = element_blank(), legend.text = element_text(size = 14))

# figure 3c
training.trace <- training.all %>%
    filter(time < 55) %>%
    mutate(z = NA)

training.trace.mean <- training.trace %>%
    group_by(time, phase) %>%
    summarise(dF = mean(dF))

early.trace.mean <- mean(training.trace.mean$dF[training.trace.mean$phase == "Early" & training.trace.mean$time < 10])
early.trace.sd <- sd(training.trace.mean$dF[training.trace.mean$phase == "Early" & training.trace.mean$time < 10])
training.trace$z[training.trace$phase == "Early"] <- 
    (training.trace$dF[training.trace$phase == "Early"] - early.trace.mean)/early.trace.sd

late.trace.mean <- mean(training.trace.mean$dF[training.trace.mean$phase == "Late" & training.trace.mean$time < 10])
late.trace.sd <- sd(training.trace.mean$dF[training.trace.mean$phase == "Late" & training.trace.mean$time < 10])
training.trace$z[training.trace$phase == "Late"] <- 
    (training.trace$dF[training.trace$phase == "Late"] - late.trace.mean)/late.trace.sd

training.trace.cell <- training.trace %>%
    filter(time > 25) %>%
    group_by(cell, phase) %>%
    summarise(z = mean(z))
training.trace.cell$cell <- as.character(training.trace.cell$cell)

f3c <- ggline(training.trace.cell, x = "phase", y = "z", group = "cell", color = "grey")+
    geom_boxplot(alpha = 0)+
    stat_compare_means(paired = TRUE, method = "t.test")+
    labs(x = "Means of Two Cycles", y = "Z Score")

figure3 <- ggarrange(f3a, f3b, f3c, labels = c("A", "B", "C"), nrow = 3, ncol = 1)
figure3 <- annotate_figure(figure3, fig.lab = "Figure 3", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure3, filename = "new figure 3.png", height = 9, width = 5)
