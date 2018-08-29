library(tidyverse)
library(ggpubr)
library(ggsci)
library(readxl)
library(zoo)

source("auROC.R")

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# read dF/F data of training, last 2 cycles
late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z training late2.csv"))
    temp <- temp %>%
        mutate(cell = cell + 100*i)
    late2 <- rbind(late2, temp)
}

late2.mean <- late2 %>%
    group_by(time) %>%
    summarise(z = mean(z))
late.tone.mean <- mean(late2.mean$z[late2.mean$time < 10 & late2.mean$time > 5])
late.tone.sd <- sd(late2.mean$z[late2.mean$time < 10 & late2.mean$time > 5])
late2 <- late2 %>%
    mutate(z = (z - late.tone.mean)/late.tone.sd)

late2 <- spread(late2, key = "cell", value = "z")
for(i in 2:ncol(late2)){
    late2[5:(nrow(late2)-4), i] <- rollmean(late2[,i], 9)
}
late2 <- gather(late2, key = "cell", value = "z", -time)

firing <- late2 %>%
    filter(time < 10)
firing$z[firing$z < 15] <- 0
firing$z[firing$z >= 15] <- 1
p <- sum(firing$z == 1)/nrow(firing)

ranking <- late2 %>%
    filter(time > 10, time < 55) %>%
    spread(key = "time", value = "z")

temp <- ranking[, -1]
temp[temp < 15 & temp > -15] <- 0
temp[temp >= 15] <- 1
temp[temp <= -15] <- -1
ranking[, -1] <- temp

ranking <- ranking %>%
    mutate(trace.mean = round(rowMeans(ranking[, 178:529]), 1))
ranking <- filter(ranking, trace.mean > 10*p)

training.late2 <- late2 %>%
    filter(time > 25, time < 55, cell %in% ranking$cell) %>%
    spread(key = "cell", value = "z")

# read z score data of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test1 all.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    test1 <- rbind(test1, temp)
}

test1.mean <- test1 %>%
    group_by(time) %>%
    summarise(z = mean(z))
test1.tone.mean <- mean(test1.mean$z[test1.mean$time < 10 & test1.mean$time > 5])
test1.tone.sd <- sd(test1.mean$z[test1.mean$time < 10 & test1.mean$time > 5])
test1 <- test1 %>%
    mutate(z = (z - test1.tone.mean)/test1.tone.sd)

test1 <- spread(test1, key = "cell", value = "z")
for(i in 2:ncol(test1)){
    test1[5:(nrow(test1)-4), i] <- rollmean(test1[,i], 9)
}
test1 <- gather(test1, key = "cell", value = "z", -time)

test1 <- test1 %>%
    filter(time > 15, time < 45, cell %in% ranking$cell) %>%
    spread(key = "cell", value = "z")

# read z score data of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test2 all.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    test2 <- rbind(test2, temp)
}

test2.mean <- test2 %>%
    group_by(time) %>%
    summarise(z = mean(z))
test2.tone.mean <- mean(test2.mean$z[test2.mean$time < 10 & test2.mean$time > 5])
test2.tone.sd <- sd(test2.mean$z[test2.mean$time < 10 & test2.mean$time > 5])
test2 <- test2 %>%
    mutate(z = (z - test2.tone.mean)/test2.tone.sd)

test2 <- spread(test2, key = "cell", value = "z")
for(i in 2:ncol(test2)){
    test2[5:(nrow(test2)-4), i] <- rollmean(test2[,i], 9)
}
test2 <- gather(test2, key = "cell", value = "z", -time)

test2 <- test2 %>%
    filter(time > 15, time < 45, cell %in% ranking$cell) %>%
    spread(key = "cell", value = "z")

# read z score data of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test3 all.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    test3 <- rbind(test3, temp)
}

test3.mean <- test3 %>%
    group_by(time) %>%
    summarise(z = mean(z))
test3.tone.mean <- mean(test3.mean$z[test3.mean$time < 10 & test3.mean$time > 5])
test3.tone.sd <- sd(test3.mean$z[test3.mean$time < 10 & test3.mean$time > 5])
test3 <- test3 %>%
    mutate(z = (z - test3.tone.mean)/test3.tone.sd)

test3 <- spread(test3, key = "cell", value = "z")
for(i in 2:ncol(test3)){
    test3[5:(nrow(test3)-4), i] <- rollmean(test3[,i], 9)
}
test3 <- gather(test3, key = "cell", value = "z", -time)

test3 <- test3 %>%
    filter(time > 15, time < 45, cell %in% ranking$cell) %>%
    spread(key = "cell", value = "z")

# read z score data of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test4 all.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    test4 <- rbind(test4, temp)
}

test4.mean <- test4 %>%
    group_by(time) %>%
    summarise(z = mean(z))
test4.tone.mean <- mean(test4.mean$z[test4.mean$time < 10 & test4.mean$time > 5])
test4.tone.sd <- sd(test4.mean$z[test4.mean$time < 10 & test4.mean$time > 5])
test4 <- test4 %>%
    mutate(z = (z - test4.tone.mean)/test4.tone.sd)

test4 <- spread(test4, key = "cell", value = "z")
for(i in 2:ncol(test4)){
    test4[5:(nrow(test4)-4), i] <- rollmean(test4[,i], 9)
}
test4 <- gather(test4, key = "cell", value = "z", -time)

test4 <- test4 %>%
    filter(time > 15, time < 45, cell %in% ranking$cell) %>%
    spread(key = "cell", value = "z")


auc <- data.frame(cell = colnames(training.late2)[-1], test1 = NA,
                  test2 = NA, test3 = NA, test4 = NA)
for (i in 2:ncol(training.late2)) {
    auc$test1[i-1] <- auROC(test1[,i], training.late2[,i])
}
for (i in 2:ncol(training.late2)) {
    auc$test2[i-1] <- auROC(test2[,i], training.late2[,i])
}
for (i in 2:ncol(training.late2)) {
    auc$test3[i-1] <- auROC(test3[,i], training.late2[,i])
}
for (i in 2:ncol(training.late2)) {
    auc$test4[i-1] <- auROC(test4[,i], training.late2[,i])
}

auc$mouse <- NA
auc$cell <- as.character(auc$cell)
auc$mouse[auc$cell < 200] <- 1
auc$mouse[auc$cell > 200 & auc$cell < 400] <- 2
auc$mouse[auc$cell > 400 & auc$cell < 500] <- 3
auc$mouse[auc$cell > 500 & auc$cell < 700] <- 4
auc$mouse[auc$cell > 700] <- 5
auc[,2:5] <- (auc[,2:5]-0.5)*2
auc <- gather(auc, key = "test", value = "discrimination", -mouse, -cell)


training <- read_xlsx("behavior scoring.xlsx", sheet = 1)
training[,7:8] <- NULL

late <- filter(training, (Time > 1310 & Time < 1360) | (Time > 1570 & Time < 1620))
late <- late %>%
    gather(key = "mouse", value = "freezing", -Time) %>%
    mutate(mouse = parse_number(mouse)) %>%
    group_by(mouse) %>%
    summarise(late = mean(freezing)/10*100)

test1 <- read_xlsx("behavior scoring.xlsx", sheet = 2, range = "A1:F92")
test1 <- test1 %>%
    filter((Time > 10 & Time < 60) | (Time > 180 & Time < 230) | (Time > 350 & Time < 400) |
               (Time > 520 & Time < 570) | (Time > 690 & Time < 740)) %>%
    gather(key = "mouse", value = "freezing", -Time) %>%
    mutate(mouse = parse_number(mouse)) %>%
    group_by(mouse) %>%
    summarise(test1 = mean(freezing)/10*100)

test2 <- read_xlsx("behavior scoring.xlsx", sheet = 3, range = "A1:F92")
test2 <- test2 %>%
    filter((Time > 10 & Time < 60) | (Time > 180 & Time < 230) | (Time > 350 & Time < 400) |
               (Time > 520 & Time < 570) | (Time > 690 & Time < 740)) %>%
    gather(key = "mouse", value = "freezing", -Time) %>%
    mutate(mouse = parse_number(mouse)) %>%
    group_by(mouse) %>%
    summarise(test2 = mean(freezing)/10*100)

test3 <- read_xlsx("behavior scoring.xlsx", sheet = 4, range = "A1:F92")
test3 <- test3 %>%
    filter((Time > 10 & Time < 60) | (Time > 180 & Time < 230) | (Time > 350 & Time < 400) |
               (Time > 520 & Time < 570) | (Time > 690 & Time < 740)) %>%
    gather(key = "mouse", value = "freezing", -Time) %>%
    mutate(mouse = parse_number(mouse)) %>%
    group_by(mouse) %>%
    summarise(test3 = mean(freezing)/10*100)

test4 <- read_xlsx("behavior scoring.xlsx", sheet = 5, range = "A1:F92")
test4 <- test4 %>%
    filter((Time > 10 & Time < 60) | (Time > 180 & Time < 230) | (Time > 350 & Time < 400) |
               (Time > 520 & Time < 570) | (Time > 690 & Time < 740)) %>%
    gather(key = "mouse", value = "freezing", -Time) %>%
    mutate(mouse = parse_number(mouse)) %>%
    group_by(mouse) %>%
    summarise(test4 = mean(freezing)/10*100)

test1$test1 <- (late$late-test1$test1)/(late$late+test1$test1)
test2$test2 <- (late$late-test2$test2)/(late$late+test2$test2)
test3$test3 <- (late$late-test3$test3)/(late$late+test3$test3)
test4$test4 <- (late$late-test4$test4)/(late$late+test4$test4)
freezing <- cbind(test1, test2, test3, test4)
freezing[, c(3, 5, 7)] <- NULL
freezing <- gather(freezing, key = "test", value = "freezing", -mouse)

correlation <- left_join(auc, freezing, by = c("mouse", "test"))
model <- lm(discrimination ~ freezing, data = correlation)
summary(model)


f6a <- ggplot(correlation, aes(x = freezing, y = discrimination))+
    geom_point(aes(color = test), size = 2, alpha = 0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    geom_vline(xintercept = 0, lty = 2)+
    xlim(-1, 1)+
    ylim(-1, 1)+
    geom_smooth(method = lm, color = "orange")+
    annotate("text", label = "p = 0.0035", x = -0.8, y = 0.9, size = 5)+
    scale_color_aaas(labels = c("Training (Late) vs. Test 1", "Training (Late) vs. Test 2",
                                "Training (Late) vs. Test 3", "Training (Late) vs. Test 4"))+
    labs(x = "Freezing Discrimination Index", y = "Neuron Discrimination Index")+
    theme_pubr()+
    theme(legend.title = element_blank(), legend.position = c(0.2, 0.2),
          legend.text = element_text(size = 8))


correlation24 <- filter(correlation, test %in% c("test2", "test4"))
model <- lm(discrimination ~ freezing, data = correlation24)
summary(model)

pal <- pal_aaas("default")(7)
f6b <- ggplot(correlation24, aes(x = freezing, y = discrimination))+
    geom_point(aes(color = test), size = 2, alpha = 0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    geom_vline(xintercept = 0, lty = 2)+
    xlim(-1, 1)+
    ylim(-1, 1)+
    geom_smooth(method = lm, color = "orange")+
    annotate("text", label = "p = 0.0007", x = -0.8, y = 0.9, size = 5)+
    scale_color_manual(labels = c( "Training (Late) vs. Test 2",
                                "Training (Late) vs. Test 4"), values = pal[c(2, 4)])+
    labs(x = "Freezing Discrimination Index", y = "Neuron Discrimination Index")+
    theme_pubr()+
    theme(legend.title = element_blank(), legend.position = c(0.2, 0.2),
          legend.text = element_text(size = 8))

figure6 <- ggarrange(f6a, f6b, labels = c("A", "B"), nrow = 2, ncol = 1)
figure6 <- annotate_figure(figure6, fig.lab = "Figure 6", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure6, filename = "new figure 6.pdf", height = 8, width = 5)
