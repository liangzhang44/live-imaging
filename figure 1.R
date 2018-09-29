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
    geom_rect(fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = seq(55, length.out = 3, by = 21), 
             xend = seq(67, length.out = 3, by = 21), y = 0, yend = 0)+
    annotate("text", x = 30, y = 1, label = "3 hours", color = "blue", size = 3)+
    annotate("text", x = seq(61, length.out = 3, by = 21), y = 1, 
             label = "1 hour", color = "blue", size = 3)+
    annotate("text", x = 7.5, y = -3.5, label = "Conditioning", size = 4)+
    annotate("text", x = 81, y = 3.5, label = "Retrieval/Extinction", size = 4)+
    annotate("text", x = 7.5, y = -2.5, label = "Training", size = 3)+
    annotate("text", x = seq(50.5, length.out = 4, by = 21), y = 2.5, 
             label = paste("Test", 1:4), size = 3)+
    annotate("rect", xmin = c(0, 15, 37.5), xmax = c(97.5, 37.5, 82.5), ymin = 5.5, ymax = 7, 
             fill = c("green", "#56B4E9", "orange"))+
    annotate("segment", x = 82.5, xend = 82.5, y = 5.5, yend = 7, color = "red")+
    annotate("text", x = c(26, 60, 82.5), y = 7.5, label = c("Tone", "Trace", "Shock"), 
             color = c("#56B4E9", "orange", "red"), size = 3)+
    annotate("text", x = c(7.5, 26, 60, 90), y = 6.25, size = 3,
             label = paste(c(10, 15, 30, 9), "sec"))+
    annotate("segment", x = 0, xend = 3, y = 5.5, yend = 2.5, lty = 2)+
    annotate("segment", x = 4, xend = 97.5, y = 2.5, yend = 5.5, lty = 2)+
    annotate("segment", x = 3, xend = 3, y = 2, yend = 2.5, lty = 1)+
    annotate("segment", x = 4, xend = 4, y = 2, yend = 2.5, lty = 1)+
    annotate("rect", xmin = c(51.5, 66.5, 74), xmax = c(122, 74, 119), ymin = -7, ymax = -5.5, 
             fill = c("yellow", "#56B4E9", "orange"))+
    annotate("segment", x = 119, xend = 119, y = -5.5, yend = -7, lty = 6, size = 0.7, color = "red")+
    annotate("text", x = c(70.25, 96.5), y = -7.5, label = c("Tone", "Trace"),
             color = c("#56B4E9", "orange"), size = 3)+
    annotate("text", x = c(59, 70.25, 96.5), y = -6.25, size = 3, 
             label = paste(c(10, 5, 30), "sec"))+
    annotate("segment", x = 51.5, xend = 115, y = -5.5, yend = -2.5, lty = 2)+
    annotate("segment", x = 116, xend = 122, y = -2.5, yend = -5.5, lty = 2)+
    annotate("segment", x = 115, xend = 115, y = -2, yend = -2.5, lty = 1)+
    annotate("segment", x = 116, xend = 116, y = -2, yend = -2.5, lty = 1)

# figure 1b behavior-training
training <- read_xlsx("behavior scoring.xlsx", sheet = 1)
training[,7:8] <- NULL
training$Mean <- rowMeans(training[,-1])/10*100
f1b <- ggplot(training, aes(Time, Mean))+
    annotate("rect", xmin = seq(0, length.out = 7, by = 260), 
             xmax = seq(65, length.out = 7, by = 260), 
             ymin = -10, ymax = 100, fill = "green", alpha = 0.4)+
    annotate("rect", xmin = seq(10, length.out = 7, by = 260), 
             xmax = seq(25, length.out = 7, by = 260), 
             ymin = -10, ymax = 100, fill = "#56B4E9")+
    annotate("rect", xmin = seq(25, length.out = 7, by = 260), 
             xmax = seq(55, length.out = 7, by = 260), 
             ymin = -10, ymax = 100, fill = "orange")+
    annotate("segment", x = seq(55, length.out = 7, by = 260), 
             xend = seq(55, length.out = 7, by = 260), 
             y = -10, yend = 100, color = "red")+
    geom_smooth(span = 0.1, se = TRUE)+
    geom_point(size = 0.5)+
    annotate("rect", xmin = c(0, 280, 700), xmax = c(1820, 700, 1540), ymin = 130, ymax = 160, 
             fill = c("green", "#56B4E9", "orange"))+
    annotate("segment", x = 1540, xend = 1540, y = 130, yend = 160, color = "red")+
    annotate("text", x = c(490, 1120, 1540), y = 168, label = c("Tone", "Trace", "Shock"),
             color = c("#56B4E9", "orange", "red"), size = 3)+
    annotate("text", x = c(140, 490, 1120, 1680), y = 145, size = 3, 
             label = paste(c(10, 15, 30, 9), "sec"))+
    annotate("segment", x = 0, xend = 260, y = 130, yend = 105, lty = 2)+
    annotate("segment", x = 260+65, xend = 1820, y = 105, yend = 130, lty = 2)+
    annotate("segment", x = 260, xend = 260, y = 100, yend = 105, lty = 1)+
    annotate("segment", x = 260+65, xend = 260+65, y = 100, yend = 105, lty = 1)+
    annotate("text", x = seq(30, length.out = 7, by = 260), y = -16, 
             label = 1:7, size = 3)+
    annotate("text", x = 1780, y = -16, label = "(Session)", size = 3)+
    scale_y_continuous(breaks = c(0, 50, 100, 150), labels = c("0", "50", "100", ""))+
    labs(x="Time (sec)", y="Freezing (%)", title = "Conditioning")+
    theme_pubr()+
    theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8),
          plot.title = element_text(size = 10))
    
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

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")
f1c <- ggplot(test, aes(Time, Freezing, color = Test))+
    scale_colour_manual(values = cbPalette)+
    annotate("rect", xmin = seq(0, length.out = 5, by = 170), 
             xmax = seq(47, length.out = 5, by = 170), 
             ymin = -10, ymax = 50, fill = "yellow")+
    annotate("rect", xmin = seq(10, length.out = 5, by = 170), 
             xmax = seq(15, length.out = 5, by = 170), 
             ymin = -10, ymax = 50, fill = "#56B4E9")+
    annotate("rect", xmin = seq(15, length.out = 5, by = 170), 
             xmax = seq(45, length.out = 5, by = 170), 
             ymin = -10, ymax = 50, fill = "orange")+
    annotate("segment", x = seq(45, length.out = 5, by = 170), 
             xend = seq(45, length.out = 5, by = 170), 
             y = -10, yend = 50, color = "red", lty = 6)+
    geom_smooth(aes(group = Test), span = 0.1, size = 0.5)+
    geom_point(size = 0.5)+
    annotate("rect", xmin = c(0, 181, 271), xmax = c(850, 271, 814), ymin = 70, ymax = 84, 
             fill = c("yellow", "#56B4E9", "orange"))+
    annotate("segment", x = 814, xend = 814, y = 70, yend = 84, lty = 6, size = 0.7, color = "red")+
    annotate("text", x = c(226, 543), y = 90, label = c("Tone", "Trace"),
             color = c("#56B4E9", "orange"), size = 3)+
    annotate("text", x = c(90, 226, 543), y = 77, size = 3, 
             label = paste(c(10, 5, 30), "sec"))+
    annotate("segment", x = 0, xend = 170, y = 70, yend = 55, lty = 2)+
    annotate("segment", x = 170+47, xend = 850, y = 55, yend = 70, lty = 2)+
    annotate("segment", x = 170, xend = 170, y = 50, yend = 55, lty = 1)+
    annotate("segment", x = 170+47, xend = 170+47, y = 50, yend = 55, lty = 1)+
    annotate("text", x = seq(25, length.out = 5, by = 170), y = -15, size = 3, 
             label = 1:5)+
    annotate("text", x = 800, y = -15, label = "(Trial)", size = 3)+
    labs(x = "Time (sec)", y = "Freezing (%)", title = "Retrieval/Extinction")+
    theme_pubr()+
    theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8), 
          legend.title = element_blank(), legend.position = "right", 
          legend.text = element_text(size = 8), plot.title = element_text(size = 10))
    

figure1 <- ggarrange(f1a, f1b, f1c, labels = c("A", "B", "C"), nrow = 3, ncol = 1)
figure1 <- annotate_figure(figure1, fig.lab = "Figure 1", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure1, filename = "figure 1.pdf", height = 16, width = 11.6, units = "cm")
