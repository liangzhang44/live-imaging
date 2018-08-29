library(tidyverse)
library(readxl)
library(ggpubr)
library(gganimate)

# figure 1c behavior tests
test <- data.frame(Time=seq(-50, 850, by=10), Test1=NA, Test2=NA, Test3=NA, Test4=NA)
for (i in 1:4) {
    assign(paste0("test", i), 
           read_xlsx("behavior scoring.xlsx", sheet = i+1, range = "A1:F92"))
    test[,i+1] <- rowMeans(get(paste0("test", i))[,-1])/10*100
}
colnames(test) <- c("Time", "Test 1", "Test 2", "Test 3", "Test 4")
test <- gather(test, key = "Test", value = "Freezing", -Time)

p <- ggscatter(test, x = "Time", y = "Freezing", color = "Test", size = 1)+
    geom_smooth(aes(group = Test, color = Test), span = 0.1)+
    annotate("rect", xmin = seq(10, length.out = 5, by = 170), xmax = seq(15, length.out = 5, by = 170), 
             ymin = -10, ymax = 50, alpha = .8, fill = "blue")+
    annotate("rect", xmin = seq(0, length.out = 5, by = 170), xmax = seq(47, length.out = 5, by = 170), 
             ymin = -10, ymax = 50, alpha = .4, fill = "yellow")+
    annotate("rect", xmin = 0, xmax = 850, ymin = 70, ymax = 80, color = "black", fill = "yellow", alpha = 0.4)+
    annotate("rect", xmin = 190, xmax = 285, ymin = 70, ymax = 80, alpha = 1, fill = "blue")+
    annotate("text", x = 237, y = 85, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 568, y = 85, label = "Trace", size = 5)+
    annotate("text", x = 90, y = 75, label = "10 sec", size = 5)+
    annotate("text", x = 237, y = 75, label = "5 sec", size = 5, color = "white")+
    annotate("text", x = 570, y = 75, label = "30 sec", size = 5)+
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
    labs(x = "Time (sec)", y = "Freezing (%)")+
    transition_time(Test)+
    #enter_appear() +
    #exit_fade()+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
          legend.title = element_blank(), legend.position = "right", legend.text = element_text(size = 12),
          plot.title = element_text(size = 16))
animate(p, height = 400, width = 700)
