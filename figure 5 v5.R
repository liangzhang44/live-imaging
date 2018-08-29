library(tidyverse)
library(readxl)
library(ggpubr)
library(ggsci)
source("normalization.R")

# read behavior data, training
training <- read_xlsx("behavior scoring.xlsx", sheet = 1)
training[,7:8] <- NULL

early <- filter(training, (Time > 10 & Time < 60) | (Time > 270 & Time < 320))
early <- early %>%
    gather(key = "mouse", value = "freezing", -Time) %>%
    mutate(mouse = parse_number(mouse)) %>%
    group_by(mouse) %>%
    summarise(Freezing = mean(freezing)/10*100) %>%
    mutate(Phase = "early")

late <- filter(training, (Time > 1310 & Time < 1360) | (Time > 1570 & Time < 1620))
late <- late %>%
    gather(key = "mouse", value = "freezing", -Time) %>%
    mutate(mouse = parse_number(mouse)) %>%
    group_by(mouse) %>%
    summarise(Freezing = mean(freezing)/10*100) %>%
    mutate(Phase = "late")
freezing <- rbind(early, late)

# read behavior data, tests
for (i in 1:4) {
    temp <- read_xlsx("behavior scoring.xlsx", sheet = i+1, range = "A1:F92")
    temp <- temp %>%
        filter((Time > 10 & Time < 60) | (Time > 180 & Time < 230) | 
                   (Time > 350 & Time < 400) | (Time > 520 & Time < 570) | 
                   (Time > 690 & Time < 740)) %>%
        gather(key = "mouse", value = "freezing", -Time) %>%
        mutate(mouse = parse_number(mouse)) %>%
        group_by(mouse) %>%
        summarise(Freezing = mean(freezing)/10*100) %>%
        mutate(Phase = paste0("test", i))
    freezing <- rbind(freezing, temp)
}

freezing$Type <- "Test"
freezing$Type[1:10] <- "Training"

# figure 5a average freezing
f5a <- ggbarplot(freezing, x = "Phase", y = "Freezing", add = "mean_se", fill = "Type")+
    scale_fill_lancet(guide = FALSE)+
    labs(y = "Freezing (%)")+
    scale_x_discrete(labels = c("Training (1&2)", "Training (6&7)",
                                "Test 1", "Test 2", "Test 3", "Test 4"))+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
          axis.title.x = element_blank())

# normalize training data, first 2 sessions
early2 <- normalization("z training early2.csv")

# normalize training data, last 2 sessions
late2 <- normalization("z training late2.csv")

# calculate baseline firing rate
firing <- filter(rbind(early2, late2), time < 10)
cutoff <- 3 * sd(firing$z)
p1 <- sum(firing$z >= cutoff)/nrow(firing)
p2 <- sum(firing$z <= -1*cutoff)/nrow(firing)

# calculate firing rate of the trace period
late2.firing <- late2 %>%
    filter(time > 25, time < 55) %>%
    group_by(cell) %>%
    summarise(firing = (sum(z >= cutoff) - sum(z <= -1*cutoff))/n())

# subset tone and trace
late2 <- late2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")

# determine status of cells
late2$firing <- late2.firing$firing
late2$type <- "Unchanged"
late2$type[late2$firing > 10 * p1] <- "Activated"
late2$type[late2$firing < -10 * p2] <- "Inhibited"

late2 <- late2 %>%
    mutate(Phase = "late") %>%
    group_by(type, Phase) %>%
    summarise(Number = n())
    
# calculate firing rate of the trace period
early2.firing <- early2 %>%
    filter(time > 25, time < 55) %>%
    group_by(cell) %>%
    summarise(firing = (sum(z >= cutoff) - sum(z <= -1*cutoff))/n())

# rearrange by late2 order
early2 <- early2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")

# determine trace cells
early2$firing <- early2.firing$firing
early2$type <- "Unchanged"
early2$type[early2$firing > 10*p1] <- "Activated"
early2$type[early2$firing < -10*p2] <- "Inhibited"

early2 <- early2 %>%
    mutate(Phase = "early") %>%
    group_by(type, Phase) %>%
    summarise(Number = n())
    
# normalize test data, calculate baseline firing rate
firing.all <- data.frame()
for (i in 1:4) {
    assign(paste0("test", i), normalization(paste0("z test", i, " all.csv")))
    firing <- filter(get(paste0("test", i)), time < 10)
    firing.all <- rbind(firing.all, firing)
}
cutoff <- 3 * sd(firing.all$z)
p1 <- sum(firing.all$z >= cutoff)/nrow(firing.all)
p2 <- sum(firing.all$z <= -1*cutoff)/nrow(firing.all)

# calculate firing rate of the trace period, rank by trace activity, determine activation
tests.wide <- data.frame()
for(i in 1:4){
    assign("temp", get(paste0("test", i)))
    firing <- temp %>%
        filter(time > 15, time < 45) %>%
        group_by(cell) %>%
        summarise(change = (sum(z >= cutoff) - sum(z <= -1*cutoff))/n())
    
    # determine activity
    temp <- temp %>%
        filter(time > 10, time < 45) %>%
        spread(key = "time", value = "z")
    temp$change <- firing$change
    temp$type <- "Unchanged"
    temp$type[temp$change < -10*p2] <- "Inhibited"
    temp$type[temp$change > 10*p1] <- "Activated"
    # select out trace cells
    temp <- temp %>%
        mutate(Phase = paste0("test", i))
    tests.wide <- rbind(tests.wide, temp)
}

tests.wide <- tests.wide %>%
    group_by(type, Phase) %>%
    summarise(Number = n())

active <- rbind(early2, late2, tests.wide)
active <- active %>%
    mutate(Number = Number/80*100) %>%
    spread(key = "type", value = "Number")


# combine activity data and behavior data
correlation <- merge(active, freezing, by = "Phase")
correlation$Phase <-  rep(c("Training (1&2)", "Training (6&7)", "Test 1",
                            "Test 2", "Test 3", "Test 4"), each = 5)

# calculate coordinates for labels
labels <- correlation %>%
    group_by(Phase) %>%
    summarise(Freezing = mean(Freezing), Activated = mean(Activated))
labels$Type <- "Test"
labels$Type[5:6] <- "Training"

# calculate correlation
model <- lm(Freezing ~ Activated, data = correlation)
summary(model) # R-squared: 0.39, p: 0.0002

# figure 5b correlation plot
f5b <- ggplot(correlation, aes(x = Activated, y = Freezing, color = Type))+
    stat_smooth(method = lm, color = "orange")+
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 2) +
    stat_summary(fun.y = mean, geom = "point", size = 1.5)+
    scale_color_lancet(guide = FALSE)+
    annotate("text", x = 5, y = 60, size = 5,
             label = paste("p =", format(summary(model)$coefficient[2, 4], digits = 2)))+
    xlim(-3, 55)+
    geom_text(data = labels, aes(label = Phase, x = Activated - 1, y = Freezing + 3), 
              size = 4, hjust = 1)+
    theme_pubr()+
    labs(x = "Activated Cells (%)", y = "Freezing (%)")

figure5 <- ggarrange(f5a, f5b, labels = c("A", "B"), nrow = 2, ncol = 1)
figure5 <- annotate_figure(figure5, fig.lab = "Figure 5", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure5, filename = "figure 5.pdf", height = 8, width = 5)