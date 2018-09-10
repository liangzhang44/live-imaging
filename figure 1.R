library(tidyverse)
library(readxl)
library(ggpubr)

# figure 1a schematic diagram
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

f1a <- ggplot(df, aes(xmin = left, xmax = right, ymin = down, ymax = up))+
    ylim(-8, 8)+
    theme_void()+
    geom_rect(fill = c(rep("green", 7), rep("orange", 20)))+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = seq(55, length.out = 3, by = 21), 
             xend = seq(67, length.out = 3, by = 21), y = 0, yend = 0)+
    annotate("text", x = 30, y = 1, label = "3 hours", color = "blue", size = 4)+
    annotate("text", x = seq(61, length.out = 3, by = 21), y = 1, 
             label = "1 hour", color = "blue", size = 4)+
    annotate("text", x = 7.5, y = -3.5, label = "Conditioning", size = 5)+
    annotate("text", x = 81, y = 3.5, label = "Retrieval/Extinction", size = 5)+
    annotate("text", x = 7.5, y = -2.5, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = 2.5, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = 2.5, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = 2.5, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = 2.5, label = "Test 4", size = 4)+
    annotate("rect", xmin = 0, xmax = 97.5, ymin = 5.5, ymax = 7, fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 15, xmax = 37.5, ymin = 5.5, ymax = 7, fill = "blue")+
    annotate("segment", x = 82.5, xend = 82.5, y = 5.5, yend = 7, size = 1.5, color = "#D55E00")+
    annotate("text", x = 26, y = 7.5, label = "Tone", color = "blue", size = 4)+
    annotate("text", x = 82.5, y = 7.5, label = "Shock", color = "#D55E00", size = 4)+
    annotate("text", x = 60, y = 7.5, label = "Trace", size = 4)+
    annotate("text", x = 7.5, y = 6.25, label = "10 sec", size = 4)+
    annotate("text", x = 26, y = 6.25, label = "15 sec", size = 4, color = "white")+
    annotate("text", x = 60, y = 6.25, label = "30 sec", size = 4)+
    annotate("text", x = 90, y = 6.25, label = "9 sec", size = 4)+
    annotate("segment", x = 0, xend = 3, y = 5.5, yend = 2.5, lty = 2)+
    annotate("segment", x = 4, xend = 97.5, y = 2.5, yend = 5.5, lty = 2)+
    annotate("segment", x = 3, xend = 3, y = 2, yend = 2.5, lty = 1)+
    annotate("segment", x = 4, xend = 4, y = 2, yend = 2.5, lty = 1)+
    annotate("rect", xmin = 51.5, xmax = 122, ymin = -7, ymax = -5.5, fill = "orange", alpha = 0.4)+
    annotate("rect", xmin = 66.5, xmax = 74, ymin = -7, ymax = -5.5, fill = "blue")+
    annotate("segment", x = 119, xend = 119, y = -5.5, yend = -7, lty = 6, size = 0.7, color = "#D55E00")+
    annotate("text", x = 70.25, y = -7.5, label = "Tone", color = "blue", size = 4)+
    annotate("text", x = 96.5, y = -7.5, label = "Trace", size = 4)+
    annotate("text", x = 59, y = -6.25, label = "10 sec", size = 4)+
    annotate("text", x = 70.25, y = -6.25, label = "5 sec", size = 4, color = "white")+
    annotate("text", x = 96.5, y = -6.25, label = "30 sec", size = 4)+
    annotate("segment", x = 51.5, xend = 115, y = -5.5, yend = -2.5, lty = 2)+
    annotate("segment", x = 116, xend = 122, y = -2.5, yend = -5.5, lty = 2)+
    annotate("segment", x = 115, xend = 115, y = -2, yend = -2.5, lty = 1)+
    annotate("segment", x = 116, xend = 116, y = -2, yend = -2.5, lty = 1)

# figure 1b behavior-training
training <- read_xlsx("behavior scoring.xlsx", sheet = 1)
training[,7:8] <- NULL
training$Mean <- rowMeans(training[,-1])/10*100
f1b <- ggscatter(training, x = "Time", y = "Mean", size = 1)+
    annotate("rect", xmin = seq(0, length.out = 7, by = 260), 
             xmax = seq(65, length.out = 7, by = 260), 
             ymin = -10, ymax = 100, fill = "green", alpha = 0.4)+
    annotate("rect", xmin = seq(10, length.out = 7, by = 260), 
             xmax = seq(25, length.out = 7, by = 260), 
             ymin = -10, ymax = 100, fill = "blue")+
    annotate("segment", x = seq(55, length.out = 7, by = 260), 
             xend = seq(55, length.out = 7, by = 260), 
             y = -10, yend = 100, color = "#D55E00")+
    geom_smooth(span = 0.1, se = TRUE)+
    annotate("rect", xmin = 0, xmax = 1820, ymin = 130, ymax = 150, fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 280, xmax = 700, ymin = 130, ymax = 150, fill = "blue")+
    annotate("segment", x = 1540, xend = 1540, y = 130, yend = 150, size = 1.5, color = "#D55E00")+
    annotate("text", x = 490, y = 156, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 1540, y = 156, label = "Shock", color = "#D55E00", size = 5)+
    annotate("text", x = 1120, y = 156, label = "Trace", size = 5)+
    annotate("text", x = 140, y = 140, label = "10 sec", size = 5)+
    annotate("text", x = 490, y = 140, label = "15 sec", size = 5, color = "white")+
    annotate("text", x = 1120, y = 140, label = "30 sec", size = 5)+
    annotate("text", x = 1680, y = 140, label = "9 sec", size = 5)+
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
    labs(x="Time (sec)", y="Freezing (%)", title = "Conditioning")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          plot.title = element_text(size = 16))

# figure 1c behavior tests
test <- data.frame(Time = seq(-50, 850, by = 10), 
                   Test1 = NA, Test2 = NA, Test3 = NA, Test4 = NA)
for (i in 1:4) {
    assign(paste0("test", i), 
           read_xlsx("behavior scoring.xlsx", sheet = i+1, range = "A1:F92"))
    test[,i+1] <- rowMeans(get(paste0("test", i))[,-1])/10*100
}
colnames(test) <- c("Time", "Test 1", "Test 2", "Test 3", "Test 4")
test <- gather(test, key = "Test", value = "Freezing", -Time)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
f1c <- ggscatter(test, x = "Time", y = "Freezing", color = "Test", size = 1)+
    scale_colour_manual(values = cbPalette)+
    annotate("rect", xmin = seq(0, length.out = 5, by = 170), 
             xmax = seq(47, length.out = 5, by = 170), 
             ymin = -10, ymax = 50, alpha = .4, fill = "orange")+
    annotate("rect", xmin = seq(10, length.out = 5, by = 170), 
             xmax = seq(15, length.out = 5, by = 170), 
             ymin = -10, ymax = 50, fill = "blue")+
    geom_smooth(aes(group = Test, color = Test), span = 0.1)+
    annotate("rect", xmin = 0, xmax = 850, ymin = 70, ymax = 84, fill = "orange", alpha = 0.4)+
    annotate("rect", xmin = 181, xmax = 271, ymin = 70, ymax = 84, alpha = 1, fill = "blue")+
    annotate("segment", x = 814, xend = 814, y = 70, yend = 84, lty = 6, size = 0.7, color = "#D55E00")+
    annotate("text", x = 226, y = 88, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 543, y = 88, label = "Trace", size = 5)+
    annotate("text", x = 90, y = 77, label = "10 sec", size = 5)+
    annotate("text", x = 226, y = 77, label = "5 sec", size = 5, color = "white")+
    annotate("text", x = 543, y = 77, label = "30 sec", size = 5)+
    annotate("segment", x = 0, xend = 170, y = 70, yend = 55, lty = 2)+
    annotate("segment", x = 170+47, xend = 850, y = 55, yend = 70, lty = 2)+
    annotate("segment", x = 170, xend = 170, y = 50, yend = 55, lty = 1)+
    annotate("segment", x = 170+47, xend = 170+47, y = 50, yend = 55, lty = 1)+
    annotate("text", x = 25, y = -15, label = "1", size = 5)+
    annotate("text", x = 25+170, y = -15, label = "2", size = 5)+
    annotate("text", x = 25+170*2, y = -15, label = "3", size = 5)+
    annotate("text", x = 25+170*3, y = -15, label = "4", size = 5)+
    annotate("text", x = 25+170*4, y = -15, label = "5", size = 5)+
    annotate("text", x = 800, y = -15, label = "(Trial)", size = 5)+
    labs(x = "Time (sec)", y = "Freezing (%)", title = "Retrieval/Extinction")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
          legend.title = element_blank(), legend.position = "right", 
          legend.text = element_text(size = 12), plot.title = element_text(size = 16))

figure1 <- ggarrange(f1a, f1b, f1c, labels = c("A", "B", "C"), nrow = 3, ncol = 1)
figure1 <- annotate_figure(figure1, fig.lab = "Figure 1", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure1, filename = "figure 1.pdf", height = 10, width = 7)
