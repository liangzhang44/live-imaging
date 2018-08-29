library(tidyverse)
library(ggpubr)
library(ggsci)
library(readxl)
library(zoo)

source("auROC.R")
source("normalization.R")

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

# determine trace cells
tracecells <- late2$cell[late2.firing$firing > p]
training.late2 <- late2 %>%
    gather(key = "time", value = "z", -cell) %>%
    filter(time > 25, time < 55, cell %in% tracecells) %>%
    spread(key = "cell", value = "z")

# read z score data for each test with all trials averaged
for (i in 1:4) {
    assign(paste0("test", i), 
           normalization(paste0("z test", i, " all.csv")) %>%
               filter(time > 15, time < 45, cell %in% tracecells) %>%
               spread(key = "cell", value = "z"))
}

# read z score data for each test and each trial
for (i in 1:4) {
    for (j in 1:5) {
        assign(paste0("test", i, ".trial", j), 
               normalization(paste0("z test", i, " trial", j, ".csv")) %>%
                   filter(time > 15, time < 45, cell %in% tracecells) %>%
                   spread(key = "cell", value = "z"))
    }
}

# calculate area under curve for averaged trials
auc.aver <- data.frame(cell = colnames(training.late2)[-1], test1 = NA,
                  test2 = NA, test3 = NA, test4 = NA)
for (i in 1:4) {
    assign("temp", get(paste0("test", i)))
    for (j in 2:ncol(training.late2)) {
        auc.aver[j-1, i+1] <- auROC(temp[,j], training.late2[,j])
    }
}
auc.aver$mouse <- 1
auc.aver$cell <- as.character(auc.aver$cell)
auc.aver$mouse[auc.aver$cell > 200 & auc.aver$cell < 400] <- 2
auc.aver$mouse[auc.aver$cell > 400 & auc.aver$cell < 500] <- 3
auc.aver$mouse[auc.aver$cell > 500 & auc.aver$cell < 700] <- 4
auc.aver$mouse[auc.aver$cell > 700] <- 5
auc.aver[,2:5] <- (auc.aver[,2:5]-0.5)*2
auc.aver <- auc.aver %>%
    gather(key = "Phase", value = "Discrimination", -mouse, -cell)

# calculate area under curve for all trials
auc.all <- data.frame()
for (i in 1:4) {
    auc <- data.frame(cell = colnames(training.late2)[-1], trial1 = NA,
                      trial2 = NA, trial3 = NA, trial4 = NA, trial5 = NA)
    for (j in 1:5) {
        assign("temp", get(paste0("test", i, ".trial", j)))
        for (k in 2:ncol(training.late2)) {
            auc[k-1, j+1] <- auROC(temp[,k], training.late2[,k])
        }
    }
    auc$mouse <- 1
    auc$cell <- as.character(auc$cell)
    auc$mouse[auc$cell > 200 & auc$cell < 400] <- 2
    auc$mouse[auc$cell > 400 & auc$cell < 500] <- 3
    auc$mouse[auc$cell > 500 & auc$cell < 700] <- 4
    auc$mouse[auc$cell > 700] <- 5
    auc[,2:6] <- (auc[,2:6]-0.5)*2
    auc <- auc %>%
        gather(key = "Trial", value = "Discrimination", -mouse, -cell) %>%
        mutate(Phase = paste0("test", i))
    auc.all <- rbind(auc.all, auc)
}

# read behavior data, last 2 sessions
training <- read_xlsx("behavior scoring.xlsx", sheet = 1)
training[,7:8] <- NULL

late <- filter(training, (Time > 1310 & Time < 1360) | (Time > 1570 & Time < 1620))
late <- late %>%
    gather(key = "mouse", value = "freezing", -Time) %>%
    mutate(mouse = parse_number(mouse)) %>%
    group_by(mouse) %>%
    summarise(Freezing = mean(freezing)/10*100) %>%
    mutate(Phase = "late")

# calculate freezing discrimination index for averaged trials
freezing.aver <- data.frame()
for (i in 1:4) {
    temp <- read_xlsx("behavior scoring.xlsx", sheet = i+1, range = "A1:F92")
    temp <- temp %>%
        filter((Time > 10 & Time < 60) | (Time > 180 & Time < 230) | (Time > 350 & Time < 400) |
                   (Time > 520 & Time < 570) | (Time > 690 & Time < 740)) %>%
        gather(key = "mouse", value = "freezing", -Time) %>%
        mutate(mouse = parse_number(mouse)) %>%
        group_by(mouse) %>%
        summarise(Freezing = mean(freezing)/10*100) %>%
        mutate(Phase = paste0("test", i))
    temp$Freezing <- (late$Freezing - temp$Freezing)/(late$Freezing + temp$Freezing)
    freezing.aver <- rbind(freezing.aver, temp)
}

# calculate freezing discrimination index for all trials
freezing.all <- data.frame()
for (i in 1:4) {
    for (j in 1:5) {
        temp <- read_xlsx("behavior scoring.xlsx", sheet = i+1, range = "A1:F92")
        temp <- temp %>%
            filter((Time > 10+(j-1)*170 & Time < 60+(j-1)*170)) %>%
            gather(key = "mouse", value = "freezing", -Time) %>%
            mutate(mouse = parse_number(mouse)) %>%
            group_by(mouse) %>%
            summarise(Freezing = mean(freezing)/10*100) %>%
            mutate(Trial = paste0("trial", j), Phase = paste0("test", i))
        temp$Freezing <- (late$Freezing - temp$Freezing)/(late$Freezing + temp$Freezing)
        freezing.all <- rbind(freezing.all, temp)
    }
}

# calculate correlation between freezing and activity for averaged trials
correlation.aver <- left_join(auc.aver, freezing.aver, by = c("mouse", "Phase"))
model1 <- lm(Discrimination ~ Freezing, data = correlation.aver)

# calculate correlation between freezing and activity for all trials
correlation.all <- left_join(auc.all, freezing.all, by = c("mouse", "Phase", "Trial"))
model2 <- lm(Discrimination ~ Freezing, data = correlation.all)

# figure 6a correlation for averaged trials
f6a <- ggplot(correlation.aver, aes(x = Freezing, y = Discrimination, color = Phase))+
    geom_point(size = 2, alpha = 0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    geom_vline(xintercept = 0, lty = 2)+
    xlim(-1, 1)+
    ylim(-1, 1)+
    geom_smooth(method = lm, color = "orange")+
    annotate("text", x = -0.8, y = 0.9, size = 4, 
             label = paste("p =", format(summary(model1)$coefficient[2, 4], digits = 2)))+
    scale_color_aaas(labels = c("Training (6&7) vs. Test 1", "Training (6&7) vs. Test 2",
                                "Training (6&7) vs. Test 3", "Training (6&7) vs. Test 4"))+
    labs(x = "Freezing Discrimination Index", y = "Neuron Activity Discrimination Index")+
    theme_pubr()+
    theme(legend.title = element_blank(), legend.position = c(0.2, 0.2),
          legend.text = element_text(size = 8))

# figure 6b correlation for all trials
f6b <- ggplot(correlation.all, aes(x = Freezing, y = Discrimination))+
    geom_point(aes(color = Phase), size = 2, alpha = 0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    geom_vline(xintercept = 0, lty = 2)+
    xlim(-1, 1)+
    ylim(-1, 1)+
    geom_smooth(method = lm, color = "orange")+
    annotate("text", x = -0.8, y = 0.9, size = 4,
             label = paste("p =", format(summary(model2)$coefficient[2, 4], digits = 2)))+
    scale_color_aaas(labels = c("Training (6&7) vs. Test 1", "Training (6&7) vs. Test 2",
                                "Training (6&7) vs. Test 3", "Training (6&7) vs. Test 4"))+
    labs(x = "Freezing Discrimination Index", y = "Neuron Activity Discrimination Index")+
    theme_pubr()+
    theme(legend.title = element_blank(), legend.position = c(0.2, 0.2),
          legend.text = element_text(size = 8))

figure6 <- ggarrange(f6a, f6b, labels = c("A", "B"), nrow = 2, ncol = 1)
figure6 <- annotate_figure(figure6, fig.lab = "Figure 6", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure6, filename = "figure 6.pdf", height = 8, width = 5)