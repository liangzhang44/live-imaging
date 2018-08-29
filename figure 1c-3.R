library(tidyverse)
library(readxl)
library(ggpubr)
library(plotly)

Sys.setenv("plotly_username"="hughzl")
Sys.setenv("plotly_api_key"="nMiiir92IHHEpAnzYxfb")

# figure 1c behavior tests
test <- data.frame(Time=seq(-50, 850, by=10), Test1=NA, Test2=NA, Test3=NA, Test4=NA)
for (i in 1:4) {
    assign(paste0("test", i), 
           read_xlsx("behavior scoring.xlsx", sheet = i+1, range = "A1:F92"))
    test[,i+1] <- rowMeans(get(paste0("test", i))[,-1])/10*100
}
colnames(test) <- c("Time", "Test 1", "Test 2", "Test 3", "Test 4")
test <- gather(test, key = "Test", value = "Freezing", -Time)

p <- ggplot(test, aes(x = Time, y = Freezing, color = Test))+
    geom_point(aes(frame = Time))+
    annotate("rect", xmin = seq(10, length.out = 5, by = 170), xmax = seq(15, length.out = 5, by = 170), 
             ymin = -10, ymax = 50, alpha = .8, fill = "blue")+
    annotate("rect", xmin = seq(0, length.out = 5, by = 170), xmax = seq(47, length.out = 5, by = 170), 
             ymin = -10, ymax = 50, alpha = .4, fill = "yellow")+
    geom_text(aes(x = Time+10, y = Freezing + 3, label = Test, frame = Time))+
    annotate("text", x = 25, y = -15, label = "1", size = 5)+
    annotate("text", x = 25+170, y = -15, label = "2", size = 5)+
    annotate("text", x = 25+170*2, y = -15, label = "3", size = 5)+
    annotate("text", x = 25+170*3, y = -15, label = "4", size = 5)+
    annotate("text", x = 25+170*4, y = -15, label = "5", size = 5)+
    annotate("text", x = 800, y = -15, label = "(Trial)", size = 5)+
    labs(x = "Time (sec)", y = "Freezing (%)")+
    theme_pubr()+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
          legend.position = "none", plot.title = element_text(size = 16))

p <- ggplotly(p, height = 400, width = 700) %>%
    animation_opts(frame = 200, easing = "linear", redraw = FALSE)
api_create(p, filename="figure 1c")