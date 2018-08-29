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

# normalize training data, last 2 sessions
late2 <- normalization("z training late2.csv")

# calculate baseline firing rate
firing <- filter(late2, time < 10)
cutoff <- 3 * sd(firing$z)
p <- sum(firing$z >= cutoff)/nrow(firing)

# calculate firing rate of the trace period
late2.firing <- late2 %>%
    filter(time > 25, time < 55) %>%
    group_by(cell) %>%
    summarise(firing = (sum(z >= cutoff) - sum(z <= -1*cutoff))/n())

# subset tone and trace
late2 <- late2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")

# calculate mean z scores of trace and tone
late2$firing <- late2.firing$firing
late2 <- late2 %>%
    mutate(tone.mean = rowMeans(late2[, 2:177]), 
           trace.mean = rowMeans(late2[, 178:529])) %>%
    arrange(desc(trace.mean), desc(tone.mean))
order <- late2$cell

# determine trace cells
late2$type <- ifelse(late2$firing > p, "Trace Cells", "Non-Trace Cells")
late <- late2 %>%
    filter(type == "Trace Cells") %>%
    mutate(Phase = "late", Active = "Active")
late <- late %>%
    group_by(Active, Phase) %>%
    summarise(Number = n())

# normalize training data, first 2 sessions
early2 <- normalization("z training early2.csv")

# calculate firing rate of the trace period
early2.firing <- early2 %>%
    filter(time > 25, time < 55) %>%
    group_by(cell) %>%
    summarise(firing = (sum(z >= cutoff) - sum(z <= -1*cutoff))/n())

# rearrange by late2 order
early2 <- early2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")
early2$firing <- early2.firing$firing
early2 <- early2[match(order, early2$cell),]
early2$type <- late2$type

early2$Active <- ifelse(early2$firing > p, "Active", "Not Active")
early2 <- early2 %>%
    filter(type == "Trace Cells") %>%
    mutate(Phase = "early")
early <- early2 %>%
    group_by(Active, Phase) %>%
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
    # rearrange by late2 order
    temp <- temp %>%
        filter(time > 10, time < 45) %>%
        spread(key = "time", value = "z")
    temp$change <- firing$change
    temp <- temp[match(order, temp$cell),]
    temp$type <- late2$type
    # determine activity
    temp$Active <- "Not Active"
    #temp$activity[temp$change < -1*p2] <- "Inhibited"
    temp$Active[temp$change > p1] <- "Active"
    # select out trace cells
    temp <- temp %>%
        filter(type == "Trace Cells") %>%
        mutate(Phase = paste0("test", i))
    tests.wide <- rbind(tests.wide, temp)
}

tests.wide <- tests.wide %>%
    group_by(Active, Phase) %>%
    summarise(Number = n())

active <- rbind(early, late, tests.wide)
active <- spread(active, key = "Active", value = "Number")
active[is.na(active)] <- 0

# combine activity data and behavior data
correlation <- merge(active, freezing, by = "Phase")
correlation$Active <- correlation$Active/late$Number*100
correlation$Phase <-  rep(c("Training (1&2)", "Training (6&7)", "Test 1",
                            "Test 2", "Test 3", "Test 4"), each = 5)

# calculate coordinates for labels
labels <- correlation %>%
    group_by(Phase) %>%
    summarise(Freezing = mean(Freezing), Active = mean(Active))
labels$Type <- "Test"
labels$Type[5:6] <- "Training"

# calculate correlation
model <- lm(Freezing ~ Active, data = correlation)
summary(model) # R-squared: 0.39, p: 0.0002

# figure 5b correlation plot
f5b <- ggplot(correlation, aes(x = Active, y = Freezing, color = Type))+
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 3) +
    stat_summary(fun.y = mean, geom = "point", size = 1.5)+
    scale_color_lancet(guide = FALSE)+
    annotate("text", x = 9, y = 70, size = 4,
             label = paste("p =", format(summary(model)$coefficient[2, 4], digits = 2)))+
    xlim(-5, 105)+
    stat_smooth(method = lm, color = "orange")+
    theme_pubr()+
    labs(x = "Activated Cells (%)", y = "Freezing (%)")

ggsave(f5b, filename = "slide2-5.pdf", height = 3, width = 4)