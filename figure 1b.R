library(tidyverse)
library(readxl)
library(ggpubr)

# figure 1b behavior-training
training <- read_xlsx("behavior scoring.xlsx", sheet = 1)
training[,7:8] <- NULL
training$Mean <- rowMeans(training[,-1])/10*100
f1b <- ggscatter(training, x = "Time", y = "Mean", size = 1)+
    geom_smooth(span=0.1, se=TRUE)+
    annotate("rect", xmin = seq(10, length.out = 7, by = 260), xmax = seq(25, length.out = 7, by = 260), 
             ymin = -10, ymax = 100, alpha = .8, fill="blue")+
    annotate("rect", xmin = seq(55, length.out = 7, by = 260), xmax = seq(57, length.out = 7, by = 260), 
             ymin = -10, ymax = 100, fill="red")+
    annotate("rect", xmin = seq(0, length.out = 7, by = 260), xmax = seq(65, length.out = 7, by = 260), 
             ymin = -10, ymax = 100, alpha = .3, fill="green")+
    annotate("rect", xmin = 0, xmax = 1820, ymin = 130, ymax = 145, color = "black", fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 280, xmax = 700, ymin = 130, ymax = 145, alpha = 1, fill = "blue")+
    annotate("segment", x = 1540, xend = 1540, y = 130, yend = 145, size = 1.5, color = "red")+
    annotate("text", x = 490, y = 152, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 1540, y = 152, label = "Shock", color = "red", size = 5)+
    annotate("text", x = 1120, y = 152, label = "Trace", size = 5)+
    annotate("text", x = 140, y = 138, label = "10 sec", size = 5)+
    annotate("text", x = 490, y = 138, label = "15 sec", size = 5, color = "white")+
    annotate("text", x = 1120, y = 138, label = "30 sec", size = 5)+
    annotate("text", x = 1680, y = 138, label = "10 sec", size = 5)+
    annotate("segment", x = 0, xend = 260, y = 130, yend = 105, lty = 2)+
    annotate("segment", x = 260+65, xend = 1820, y = 105, yend = 130, lty = 2)+
    annotate("segment", x = 260, xend = 260, y = 100, yend = 105, lty = 1)+
    annotate("segment", x = 260+65, xend = 260+65, y = 100, yend = 105, lty = 1)+
    annotate("text", x = 30, y = -16, label = "1", size = 5)+
    annotate("text", x = 30+260, y = -16, label = "2", size = 5)+
    annotate("text", x = 30+260*2, y = -16, label = "3", size = 5)+
    annotate("text", x = 30+260*3, y = -16, label = "4", size = 5)+
    annotate("text", x = 30+260*4, y = -16, label = "5", size = 5)+
    annotate("text", x = 30+260*5, y = -16, label = "6", size = 5)+
    annotate("text", x = 30+260*6, y = -16, label = "7", size = 5)+
    annotate("text", x = 1780, y = -16, label = "(Session)", size = 5)+
    scale_y_continuous(breaks = c(0, 50, 100, 150), labels = c("0", "50", "100", ""))+
    labs(x="Time (sec)", y="Freezing (%)")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          plot.title = element_text(size = 16))
ggsave(f1b, filename = "figure 1b.pdf", height = 4, width = 7)