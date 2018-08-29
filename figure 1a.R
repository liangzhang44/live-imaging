library(tidyverse)

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

f1a <- ggplot(df, aes(xmin=left, xmax=right, ymin=down, ymax=up))+
    ylim(-8, 8)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = seq(55, length.out = 3, by = 21), xend = seq(67, length.out = 3, by = 21), y = 0, yend = 0)+
    annotate("text", x = 30, y = 1, label = "3 hours", color = "blue", size = 4)+
    annotate("text", x = seq(61, length.out = 3, by = 21), y = 1, label = "1 hour", color = "blue", size = 4)+
    annotate("text", x = 7.5, y = -2.5, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = 2.5, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = 2.5, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = 2.5, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = 2.5, label = "Test 4", size = 4)+
    annotate("rect", xmin = 0, xmax = 97.5, ymin = 5.5, ymax = 7, color = "black", fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 15, xmax = 37.5, ymin = 5.5, ymax = 7, alpha = 1, fill = "blue")+
    annotate("segment", x = 82.5, xend = 82.5, y = 5.5, yend = 7, size = 1.5, color = "red")+
    annotate("text", x = 26, y = 7.5, label = "Tone", color = "blue", size = 4)+
    annotate("text", x = 82.5, y = 7.5, label = "Shock", color = "red", size = 4)+
    annotate("text", x = 60, y = 7.5, label = "Trace", size = 4)+
    annotate("text", x = 7.5, y = 6.25, label = "10 sec", size = 4)+
    annotate("text", x = 26, y = 6.25, label = "15 sec", size = 4, color = "white")+
    annotate("text", x = 60, y = 6.25, label = "30 sec", size = 4)+
    annotate("text", x = 90, y = 6.25, label = "10 sec", size = 4)+
    annotate("segment", x = 0, xend = 3, y = 5.5, yend = 2.5, lty = 2)+
    annotate("segment", x = 4, xend = 97.5, y = 2.5, yend = 5.5, lty = 2)+
    annotate("segment", x = 3, xend = 3, y = 2, yend = 2.5, lty = 1)+
    annotate("segment", x = 4, xend = 4, y = 2, yend = 2.5, lty = 1)+
    annotate("rect", xmin = 51.5, xmax = 119, ymin = -7, ymax = -5.5, color = "black", fill = "yellow", alpha = 0.4)+
    annotate("rect", xmin = 66.5, xmax = 74, ymin = -7, ymax = -5.5, alpha = 1, fill = "blue")+
    annotate("text", x = 70.25, y = -7.5, label = "Tone", color = "blue", size = 4)+
    annotate("text", x = 96.5, y = -7.5, label = "Trace", size = 4)+
    annotate("text", x = 59, y = -6.25, label = "10 sec", size = 4)+
    annotate("text", x = 70.25, y = -6.25, label = "5 sec", size = 4, color = "white")+
    annotate("text", x = 96.5, y = -6.25, label = "30 sec", size = 4)+
    annotate("segment", x = 51.5, xend = 115, y = -5.5, yend = -2.5, lty = 2)+
    annotate("segment", x = 116, xend = 119, y = -2.5, yend = -5.5, lty = 2)+
    annotate("segment", x = 115, xend = 115, y = -2, yend = -2.5, lty = 1)+
    annotate("segment", x = 116, xend = 116, y = -2, yend = -2.5, lty = 1)
ggsave(f1a, filename = "figure 1a.pdf", height = 4, width = 7)
