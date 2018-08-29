library(tidyverse)
library(ggpubr)
library(MASS)

source("dF.all.R")

# calculate dF/F for 5 mice
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]
for(i in 1:length(allfolders)){
    dF.all(allfolders[i])
}

# read dF/F data of training, all cycles
training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF training all.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    training <- rbind(training, temp)
}

# plot dF/F data of training, all cycles
training$cell <- factor(training$cell)
ggplot(training, aes(x=time, y=dF, color=cell))+
    geom_smooth(span=0.1, se=FALSE, size=1.5)+
    geom_point(size=0.1)+
    geom_vline(xintercept = 10, color="blue")+
    geom_vline(xintercept = 25, color="blue")+
    geom_vline(xintercept = 55, color="red")+
    facet_wrap(~cell, nrow = 8, scales = "free_y")+
    theme_pubr()+
    labs(x="Time (sec)", y=expression(Delta*"F/F (%)"), title=expression(Delta*"F/F for Training"))+
    theme(legend.position = "none", strip.background = element_blank(),
          strip.text.x = element_blank())

# read dF/F data of training, first 2 cycles
early2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF training early2.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    early2 <- rbind(early2, temp)
}
early2$phase <- "early"

late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/dF training late2.csv"))
    temp <- temp %>%
        gather(key = "cell", value = "dF", -time) %>%
        mutate(cell=parse_number(cell)) %>%
        mutate(cell=cell+100*i) %>%
        mutate(dF=dF*100)
    late2 <- rbind(late2, temp)
}
late2$phase <- "late"

training.all <- rbind(early2, late2)
ggline(training.all, x = "time", y = "dF", add = "mean_se", color = "phase", numeric.x.axis = TRUE)

training.tone <- training.all %>%
    filter(time > 5, time <25) %>%
    mutate(time = time -10.06, z = NA)

training.tone.mean <- training.tone %>%
    group_by(time, phase) %>%
    summarise(dF = mean(dF))

early.base.mean <- mean(training.tone.mean$dF[training.tone.mean$phase == "early" & training.tone.mean$time < 0])
early.base.sd <- sd(training.tone.mean$dF[training.tone.mean$phase == "early" & training.tone.mean$time < 0])
training.tone$z[training.tone$phase == "early"] <- 
    (training.tone$dF[training.tone$phase == "early"] - early.base.mean)/early.base.sd

late.base.mean <- mean(training.tone.mean$dF[training.tone.mean$phase == "late" & training.tone.mean$time < 0])
late.base.sd <- sd(training.tone.mean$dF[training.tone.mean$phase == "late" & training.tone.mean$time < 0])
training.tone$z[training.tone$phase == "late"] <- 
    (training.tone$dF[training.tone$phase == "late"] - late.base.mean)/late.base.sd

ggline(training.tone, x = "time", y = "z", add = "mean_se", color = "phase")+
    stat_compare_means(aes(group = phase), label = "p.signif", method = "t.test",
                       hide.ns = TRUE, label.y = 10, paired = TRUE)+
    labs(x="Time (sec)", y="Z Score")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

training.shock <- training.all %>%
    filter(time > 51) %>%
    mutate(time = time -56, z = NA)

training.shock.mean <- training.shock %>%
    group_by(time, phase) %>%
    summarise(dF = mean(dF))

early.shock.mean <- mean(training.shock.mean$dF[training.shock.mean$phase == "early" & training.shock.mean$time < 0])
early.shock.sd <- sd(training.shock.mean$dF[training.shock.mean$phase == "early" & training.shock.mean$time < 0])
training.shock$z[training.shock$phase == "early"] <- 
    (training.shock$dF[training.shock$phase == "early"] - early.shock.mean)/early.shock.sd

late.shock.mean <- mean(training.shock.mean$dF[training.shock.mean$phase == "late" & training.shock.mean$time < 0])
late.shock.sd <- sd(training.shock.mean$dF[training.shock.mean$phase == "late" & training.shock.mean$time < 0])
training.shock$z[training.shock$phase == "late"] <- 
    (training.shock$dF[training.shock$phase == "late"] - late.shock.mean)/late.shock.sd

ggline(training.shock, x = "time", y = "z", add = "mean_se", color = "phase", numeric.x.axis = TRUE)+
    #stat_compare_means(label = "p.signif", aes(group = phase), hide.ns = TRUE, label.y = 4, paired = TRUE)+
    labs(x="Time (sec)", y="Z Score")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

training.trace <- training.all %>%
    filter(time < 55) %>%
    mutate(z = NA)

training.trace.mean <- training.trace %>%
    group_by(time, phase) %>%
    summarise(dF = mean(dF))

early.trace.mean <- mean(training.trace.mean$dF[training.trace.mean$phase == "early" & training.trace.mean$time < 10])
early.trace.sd <- sd(training.trace.mean$dF[training.trace.mean$phase == "early" & training.trace.mean$time < 10])
training.trace$z[training.trace$phase == "early"] <- 
    (training.trace$dF[training.trace$phase == "early"] - early.trace.mean)/early.trace.sd

late.trace.mean <- mean(training.trace.mean$dF[training.trace.mean$phase == "late" & training.trace.mean$time < 10])
late.trace.sd <- sd(training.trace.mean$dF[training.trace.mean$phase == "late" & training.trace.mean$time < 10])
training.trace$z[training.trace$phase == "late"] <- 
    (training.trace$dF[training.trace$phase == "late"] - late.trace.mean)/late.trace.sd

training.trace.cell <- training.trace %>%
    filter(time > 25) %>%
    group_by(cell, phase) %>%
    summarise(z = mean(z))
training.trace.cell$cell <- as.character(training.trace.cell$cell)

ggline(training.trace.cell, x = "phase", y = "z", group = "cell")+
    stat_compare_means(paired = TRUE, method = "t.test")


training.trace.baseline <- training.all %>%
    filter(time < 10) %>%
    group_by(cell, phase) %>%
    summarise(mean = mean(dF), sd = sd(dF))
training.trace <- training.all %>%
    filter(time > 25, time < 55) %>%
    spread(key = "time", value = "dF")
training.trace[, c(-1,-2)] <- (training.trace[, c(-1,-2)] - training.trace.baseline$mean)/training.trace.baseline$sd
training.trace <- training.trace %>%
    gather(key = "time", value = "z", -cell, -phase) %>%
    group_by(cell, phase) %>%
    summarise(z = mean(z))
ggpaired(training.trace, x = "phase", y = "z", line.color = "gray", line.size = 0.4, group = "cell")+
    stat_compare_means(paired = TRUE)
ggline(training.trace, x = "phase", y = "z", group = "cell", add = "boxplot")

figure2 <- ggarrange(f2a, f2b, labels = c("A", "B"), nrow = 2, ncol = 1)
figure2 <- annotate_figure(figure2, fig.lab = "Figure 2", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure2, filename = "figure 2 new.png", height = 8, width = 6)

# read Z score of training files of 5 mice
training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec training all.csv"))
    temp$cell <- temp$cell+100*i
    training <- rbind(training, temp)
}

# calculate max of z scores after shock
shock <- training %>%
    filter(block >= 55) %>%
    group_by(cell) %>%
    summarise(max = max(z), min = min(z))
shock$z <- ifelse(abs(shock$max)>abs(shock$min), shock$max, shock$min)

# define types based on z >= 3 and z <= -3
type1 <- as.character(shock$cell[shock$z >= 3])
type2 <- as.character(shock$cell[shock$z < 3 & shock$z > -3])
type3 <- as.character(shock$cell[shock$z <= -3])

# read Z score of training, first 2 cycles
training.early2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec training early2.csv"))
    temp$cell <- temp$cell+100*i
    training.early2 <- rbind(training.early2, temp)
}
training.early2 <- rename(training.early2, training_early = z)

# read Z score of training, last 2 cycles
training.late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec training late2.csv"))
    temp$cell <- temp$cell+100*i
    training.late2 <- rbind(training.late2, temp)
}
training.late2 <- rename(training.late2, training_late = z)

# combine tone periods of all phases
early2.late2.shock <- cbind(training.early2[training.early2$block >= 55 & training.early2$block <= 59,], 
                            training.late2[training.late2$block >= 55 & training.late2$block <= 59,])

# calculate percentage of active time
early2.late2.shock[,c(4,5)] <- NULL
temp <- early2.late2.shock[, c(-1, -2)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
early2.late2.shock[, c(-1, -2)] <- temp
early2.late2.shock <- early2.late2.shock %>%
    gather(key = "phase", value = "z", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(pos = sum(z == 1)/n()*100) %>%
    separate(phase, into = c("phase", "cycles")) %>%
    mutate(type = NA)

# assign types
early2.late2.shock$type[early2.late2.shock$cell %in% type1] <- "Type I"
early2.late2.shock$type[early2.late2.shock$cell %in% type2] <- "Type II"
early2.late2.shock$type[early2.late2.shock$cell %in% type3] <- "Type III"
early2.late2.shock$cell <- as.character(early2.late2.shock$cell)

# line plot
temp1 <- spread(early2.late2.shock, key = "cycles", value = "pos")
increased <- temp1$cell[temp1$phase == "training" & temp1$early < temp1$late]
decreased <- temp1$cell[temp1$phase == "training" & temp1$early > temp1$late]
unchanged <- temp1$cell[temp1$phase == "training" & temp1$early == temp1$late]
temp1$trend <- NA
temp1$trend[temp1$cell %in% increased] <- "increased"
temp1$trend[temp1$cell %in% decreased] <- "decreased"
temp1$trend[temp1$cell %in% unchanged] <- "unchanged"
temp1 <- gather(temp1, key = "cycles", value = "z", -cell, -phase, -type, -trend)
ggplot(early2.late2.shock, aes(x = cycles, y = pos, group = cell))+
    geom_line(aes(color = cell))+
    geom_point(size = 2, alpha = 0.2)+
    facet_wrap(~type)+
    theme_pubr()+
    theme(legend.position = "none")


# read Z score of training, first 3 cycles
training.early3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec training early3.csv"))
    temp$cell <- temp$cell+100*i
    training.early3 <- rbind(training.early3, temp)
}
training.early3 <- rename(training.early3, training_early = z)

# read Z score of training, last 3 cycles
training.late3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec training late3.csv"))
    temp$cell <- temp$cell+100*i
    training.late3 <- rbind(training.late3, temp)
}
training.late3 <- rename(training.late3, training_late = z)

# combine tone periods of all phases
early3.late3.shock <- cbind(training.early3[training.early3$block >= 55 & training.early3$block <= 59,], 
                            training.late3[training.late3$block >= 55 & training.late3$block <= 59,])

# calculate percentage of active time
early3.late3.shock[,c(4,5)] <- NULL
temp <- early3.late3.shock[, c(-1, -2)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
early3.late3.shock[, c(-1, -2)] <- temp
early3.late3.shock <- early3.late3.shock %>%
    gather(key = "phase", value = "z", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(pos = sum(z == 1)/n()*100) %>%
    separate(phase, into = c("phase", "cycles")) %>%
    mutate(type = NA)

# assign types
early3.late3.shock$type[early3.late3.shock$cell %in% type1] <- "Type I"
early3.late3.shock$type[early3.late3.shock$cell %in% type2] <- "Type II"
early3.late3.shock$type[early3.late3.shock$cell %in% type3] <- "Type III"
early3.late3.shock$cell <- as.character(early3.late3.shock$cell)

ggplot(early3.late3.shock, aes(x = cycles, y = pos, group = cell))+
    geom_line(aes(color = cell))+
    geom_point(size = 2, alpha = 0.2)+
    facet_wrap(~type)+
    theme_pubr()+
    theme(legend.position = "none")



# read Z score files of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test1 all.csv"))
    temp$cell <- temp$cell+100*i
    test1 <- rbind(test1, temp)
}
test1 <- rename(test1, test1 = z)

# read Z score files of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test2 all.csv"))
    temp$cell <- temp$cell+100*i
    test2 <- rbind(test2, temp)
}
test2 <- rename(test2, test2 = z)

# read Z score files of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test3 all.csv"))
    temp$cell <- temp$cell+100*i
    test3 <- rbind(test3, temp)
}
test3 <- rename(test3, test3 = z)

# read Z score files of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z test4 all.csv"))
    temp$cell <- temp$cell+100*i
    test4 <- rbind(test4, temp)
}
test4 <- rename(test4, test4 = z)

# combine trace periods of all phases
test.trace <- cbind(test1, test2, test3, test4)
test.trace[,c(4,5,7,8,10,11,13,14)] <- NULL
test.trace <- filter(test.trace, time > 15, time < 45)
temp <- test.trace[, c(-1, -2)]
temp[temp < 3] <- 0
temp[temp >= 3] <- 1
test.trace[, c(-1, -2)] <- temp

# calculate probability mass function
p <- sum(test.trace[, c(-1, -2)] == 1)/(nrow(test.trace)*4)
theoretical <- data.frame(total = 0:length(unique(test.trace$time)), 
                          theoretical = dbinom(x = 0:length(unique(test.trace$time)), prob = p, 
                                               size = length(unique(test.trace$time))))
test.trace.sum <- test.trace %>%
    gather(key = "phase", value = "value", -cell, -time) %>%
    group_by(cell, phase) %>%
    summarise(total = sum(value))
test.trace.sum <- test.trace.sum %>%
    group_by(phase, total) %>%
    summarise(count = n()/80) %>%
    spread(key = "phase", value = "count")
distribution <- full_join(theoretical, test.trace.sum, by = "total")

distribution <- gather(distribution, key = "type", value = "probability", -total)
distribution <- distribution %>%
    mutate(time = total/length(unique(test.trace$time))*30,
           block = cut(time, breaks = 0:30, labels = 1:30))
distribution$probability[is.na(distribution$probability)] <- 0
distribution$block <- as.numeric(distribution$block)
distribution$block[is.na(distribution$block)] <- 0
distribution <- distribution %>%
    group_by(block, type) %>%
    summarise(probability = sum(probability)) %>%
    spread(key = "type", value = "probability")

distribution <- distribution[1:5,]
distribution[6, -1] <- 1-colSums(distribution[1:5, -1], na.rm = TRUE)
distribution[6, 1] <- "4+"
distribution <- gather(distribution, key = "type", value = "probability", -block)

distribution$block <- as.character(distribution$block)
ggplot(distribution, aes(x = type, y = probability, fill = block))+
    geom_bar(stat = "identity")+
    scale_fill_brewer(palette = "Pastel1")+
    scale_x_discrete(limits = c("theoretical", "test1", "test2", "test3", "test4"))+
    theme_pubr()


test.trace <- cbind(test1, test2, test3, test4)
test.trace[,c(4,5,7,8,10,11,13,14)] <- NULL
test.trace <- filter(test.trace, block >= 15, block <= 44)
temp <- test.trace[, c(-1, -2)]
temp[temp > -3] <- 0
temp[temp <= -3] <- -1
test.trace[, c(-1, -2)] <- temp

# calculate probability mass function
p <- sum(test.trace[, c(-1, -2)] == -1)/(nrow(test.trace)*4)
theoretical <- data.frame(time = 0:30, theoretical = dbinom(x = 0:30, prob = p, size = 30))
test.trace.sum <- test.trace %>%
    gather(key = "phase", value = "value", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(time = -1*sum(value))
test.trace.sum <- test.trace.sum %>%
    group_by(phase, time) %>%
    summarise(count = n()/80) %>%
    spread(key = "phase", value = "count")
distribution <- full_join(theoretical, test.trace.sum, by = "time")
distribution <- distribution[1:5,]
distribution[6, -1] <- 1-colSums(distribution[1:5, -1], na.rm = TRUE)
distribution[6, 1] <- "4+"
distribution <- gather(distribution, key = "type", value = "probability", -time)
distribution$probability[is.na(distribution$probability)] <- 0
ggplot(distribution, aes(x = type, y = probability, fill = time))+
    geom_bar(stat = "identity")+
    scale_fill_brewer(palette = "Pastel1")+
    scale_x_discrete(limits = c("theoretical", "test1", "test2", "test3", "test4"))+
    theme_pubr()

png("probability of activation.png")
plot(x = 0:30, y = binom1, type = "h", col = "red", xlab = "Time of activation(sec)",
     ylab = "Probability")
dev.off()





# read Z score files of test1, first 2 cycles
test1.early2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test1 early2.csv"))
    temp$cell <- temp$cell+100*i
    test1.early2 <- rbind(test1.early2, temp)
}
test1.early2 <- rename(test1.early2, test1_early = z)

# read Z score files of test1, last 2 cycles
test1.late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test1 late2.csv"))
    temp$cell <- temp$cell+100*i
    test1.late2 <- rbind(test1.late2, temp)
}
test1.late2 <- rename(test1.late2, test1_late = z)

# read Z score files of test2, first 2 cycles
test2.early2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test2 early2.csv"))
    temp$cell <- temp$cell+100*i
    test2.early2 <- rbind(test2.early2, temp)
}
test2.early2 <- rename(test2.early2, test2_early = z)

# read Z score files of test2, last 2 cycles
test2.late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test2 late2.csv"))
    temp$cell <- temp$cell+100*i
    test2.late2 <- rbind(test2.late2, temp)
}
test2.late2 <- rename(test2.late2, test2_late = z)

# read Z score files of test3, first 2 cycles
test3.early2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test3 early2.csv"))
    temp$cell <- temp$cell+100*i
    test3.early2 <- rbind(test3.early2, temp)
}
test3.early2 <- rename(test3.early2, test3_early = z)

# read Z score files of test3, last 2 cycles
test3.late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test3 late2.csv"))
    temp$cell <- temp$cell+100*i
    test3.late2 <- rbind(test3.late2, temp)
}
test3.late2 <- rename(test3.late2, test3_late = z)

# read Z score files of test4, first 2 cycles
test4.early2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test4 early2.csv"))
    temp$cell <- temp$cell+100*i
    test4.early2 <- rbind(test4.early2, temp)
}
test4.early2 <- rename(test4.early2, test4_early = z)

# read Z score files of test4, last 2 cycles
test4.late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test4 late2.csv"))
    temp$cell <- temp$cell+100*i
    test4.late2 <- rbind(test4.late2, temp)
}
test4.late2 <- rename(test4.late2, test4_late = z)


# combine tone periods of all phases
early2.late2.tone1 <- cbind(training.early2[training.early2$block >= 10 & training.early2$block <= 24,], 
                         training.late2[training.late2$block >= 10 & training.late2$block <= 24,])
early2.late2.tone2 <- cbind(test1.early2[test1.early2$block >= 10 & test1.early2$block <= 14,], 
                         test1.late2[test1.late2$block >= 10 & test1.late2$block <= 14,], 
                         test2.early2[test2.early2$block >= 10 & test2.early2$block <= 14,], 
                         test2.late2[test2.late2$block >= 10 & test2.late2$block <= 14,], 
                         test3.early2[test3.early2$block >= 10 & test3.early2$block <= 14,], 
                         test3.late2[test3.late2$block >= 10 & test3.late2$block <= 14,],
                         test4.early2[test4.early2$block >= 10 & test4.early2$block <= 14,], 
                         test4.late2[test4.late2$block >= 10 & test4.late2$block <= 14,])

# calculate percentage of active time
early2.late2.tone1[,c(4,5)] <- NULL
temp <- early2.late2.tone1[, c(-1, -2)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
early2.late2.tone1[, c(-1, -2)] <- temp
early2.late2.tone1 <- early2.late2.tone1 %>%
    gather(key = "phase", value = "z", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(pos = sum(z == 1)/15*100) %>%
    separate(phase, into = c("phase", "cycles")) %>%
    mutate(type = NA)

early2.late2.tone2[,c(4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26)] <- NULL
temp <- early2.late2.tone2[, c(-1, -2)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
early2.late2.tone2[, c(-1, -2)] <- temp
early2.late2.tone2 <- early2.late2.tone2 %>%
    gather(key = "phase", value = "z", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(pos = sum(z == 1)/5*100) %>%
    separate(phase, into = c("phase", "cycles")) %>%
    mutate(type = NA)
early2.late2.tone <- rbind(early2.late2.tone1, early2.late2.tone2)

# assign types
early2.late2.tone$type[early2.late2.tone$cell %in% type1] <- "Type I"
early2.late2.tone$type[early2.late2.tone$cell %in% type2] <- "Type II"
early2.late2.tone$type[early2.late2.tone$cell %in% type3] <- "Type III"
early2.late2.tone$cell <- as.character(early2.late2.tone$cell)
early2.late2.tone$phase <- factor(early2.late2.tone$phase, levels = c("training", "test1", "test2", "test3", "test4"))

# lollipop plot
ggplot(temp1)+
    geom_segment(aes(x = cell, xend = cell, y = early, yend = late, color = trend))+
    geom_point(aes(x = cell, y = early), fill = "black", shape = 21, size = 2)+
    geom_point(aes(x = cell, y = late), fill = "white", shape = 21, size = 2)+
    facet_grid(type~phase)

# line plot
temp1 <- spread(early2.late2.tone, key = "cycles", value = "pos")
increased <- temp1$cell[temp1$phase == "training" & temp1$early < temp1$late]
decreased <- temp1$cell[temp1$phase == "training" & temp1$early > temp1$late]
unchanged <- temp1$cell[temp1$phase == "training" & temp1$early == temp1$late]
temp1$trend <- NA
temp1$trend[temp1$cell %in% increased] <- "increased"
temp1$trend[temp1$cell %in% decreased] <- "decreased"
temp1$trend[temp1$cell %in% unchanged] <- "unchanged"
temp1 <- gather(temp1, key = "cycles", value = "z", -cell, -phase, -type, -trend)
ggplot(temp1, aes(x = cycles, y = z, color = trend, group = cell))+
    geom_line()+
    facet_grid(type~phase)


# combine trace periods of all phases
early2.late2.trace <- cbind(training.early2[training.early2$block >= 25 & training.early2$block <= 54,], 
                         training.late2[training.late2$block >= 25 & training.late2$block <= 54,], 
                         test1.early2[test1.early2$block >= 15 & test1.early2$block <= 44,], 
                         test1.late2[test1.late2$block >= 15 & test1.late2$block <= 44,], 
                         test2.early2[test2.early2$block >= 15 & test2.early2$block <= 44,], 
                         test2.late2[test2.late2$block >= 15 & test2.late2$block <= 44,], 
                         test3.early2[test3.early2$block >= 15 & test3.early2$block <= 44,], 
                         test3.late2[test3.late2$block >= 15 & test3.late2$block <= 44,],
                         test4.early2[test4.early2$block >= 15 & test4.early2$block <= 44,], 
                         test4.late2[test4.late2$block >= 15 & test4.late2$block <= 44,])
early2.late2.trace[,c(4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29)] <- NULL
temp <- early2.late2.trace[, c(-1, -2)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
early2.late2.trace[, c(-1, -2)] <- temp

# calculate percentage of active time
early2.late2.trace <- early2.late2.trace %>%
    gather(key = "phase", value = "z", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(pos = sum(z == 1)/30*100) %>%
    separate(phase, into = c("phase", "cycles")) %>%
    mutate(type = NA)
early2.late2.trace$type[early2.late2.trace$cell %in% type1] <- "Type I"
early2.late2.trace$type[early2.late2.trace$cell %in% type2] <- "Type II"
early2.late2.trace$type[early2.late2.trace$cell %in% type3] <- "Type III"
early2.late2.trace$cell <- as.character(early2.late2.trace$cell)
early2.late2.trace$phase <- factor(early2.late2.trace$phase, levels = c("training", "test1", "test2", "test3", "test4"))

# line plot
early2.late2.trace <- spread(early2.late2.trace, key = "cycles", value = "pos")
increased <- early2.late2.trace$cell[early2.late2.trace$phase == "training" & 
                                         early2.late2.trace$early < early2.late2.trace$late]
decreased <- early2.late2.trace$cell[early2.late2.trace$phase == "training" & 
                                         early2.late2.trace$early > early2.late2.trace$late]
unchanged <- early2.late2.trace$cell[early2.late2.trace$phase == "training" & 
                                         early2.late2.trace$early == early2.late2.trace$late]
early2.late2.trace$trend <- NA
early2.late2.trace$trend[early2.late2.trace$cell %in% increased] <- "increased"
early2.late2.trace$trend[early2.late2.trace$cell %in% decreased] <- "decreased"
early2.late2.trace$trend[early2.late2.trace$cell %in% unchanged] <- "unchanged"
early2.late2.trace <- gather(early2.late2.trace, key = "cycles", value = "z", -cell, -phase, -type, -trend)

a <- ggplot(early2.late2.trace[early2.late2.trace$trend == "increased",], aes(x = cycles, y = z, group = cell))+
    geom_line(color = "red")+
    geom_point(size = 2, alpha = 0.4)+
    facet_grid(type~phase)+
    theme_pubr()
b <- ggplot(early2.late2.trace[early2.late2.trace$trend == "decreased",], aes(x = cycles, y = z, group = cell))+
    geom_line(color = "green")+
    geom_point(size = 2, alpha = 0.4)+
    facet_grid(type~phase)+
    theme_pubr()
c <- ggplot(early2.late2.trace[early2.late2.trace$trend == "unchanged",], aes(x = cycles, y = z, group = cell))+
    geom_line(color = "blue")+
    geom_point(size = 2, alpha = 0.4)+
    scale_y_continuous(limits = c(0, 100))+
    facet_grid(type~phase)+
    theme_pubr()
fig <- ggarrange(a, b, c, labels = c("A", "B", "C"), ncol = 1, nrow = 3)
ggsave(fig, filename = "early2 vs late2.png", height = 12, width = 10)





# read Z score files of test1, first 3 cycles
test1.early3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test1 early3.csv"))
    temp$cell <- temp$cell+100*i
    test1.early3 <- rbind(test1.early3, temp)
}
test1.early3 <- rename(test1.early3, test1_early = z)

# read Z score files of test1, last 3 cycles
test1.late3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test1 late3.csv"))
    temp$cell <- temp$cell+100*i
    test1.late3 <- rbind(test1.late3, temp)
}
test1.late3 <- rename(test1.late3, test1_late = z)

# read Z score files of test2, first 3 cycles
test2.early3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test2 early3.csv"))
    temp$cell <- temp$cell+100*i
    test2.early3 <- rbind(test2.early3, temp)
}
test2.early3 <- rename(test2.early3, test2_early = z)

# read Z score files of test2, last 3 cycles
test2.late3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test2 late3.csv"))
    temp$cell <- temp$cell+100*i
    test2.late3 <- rbind(test2.late3, temp)
}
test2.late3 <- rename(test2.late3, test2_late = z)

# read Z score files of test3, first 3 cycles
test3.early3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test3 early3.csv"))
    temp$cell <- temp$cell+100*i
    test3.early3 <- rbind(test3.early3, temp)
}
test3.early3 <- rename(test3.early3, test3_early = z)

# read Z score files of test3, last 3 cycles
test3.late3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test3 late3.csv"))
    temp$cell <- temp$cell+100*i
    test3.late3 <- rbind(test3.late3, temp)
}
test3.late3 <- rename(test3.late3, test3_late = z)

# read Z score files of test4, first 3 cycles
test4.early3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test4 early3.csv"))
    temp$cell <- temp$cell+100*i
    test4.early3 <- rbind(test4.early3, temp)
}
test4.early3 <- rename(test4.early3, test4_early = z)

# read Z score files of test4, last 2 cycles
test4.late3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test4 late3.csv"))
    temp$cell <- temp$cell+100*i
    test4.late3 <- rbind(test4.late3, temp)
}
test4.late3 <- rename(test4.late3, test4_late = z)


# combine tone periods of all phases
early3.late3.tone1 <- cbind(training.early3[training.early3$block >= 10 & training.early3$block <= 24,], 
                          training.late3[training.late3$block >= 10 & training.late3$block <= 24,])
early3.late3.tone2 <- cbind(test1.early3[test1.early3$block >= 10 & test1.early3$block <= 14,], 
                          test1.late3[test1.late3$block >= 10 & test1.late3$block <= 14,], 
                          test2.early3[test2.early3$block >= 10 & test2.early3$block <= 14,], 
                          test2.late3[test2.late3$block >= 10 & test2.late3$block <= 14,], 
                          test3.early3[test3.early3$block >= 10 & test3.early3$block <= 14,], 
                          test3.late3[test3.late3$block >= 10 & test3.late3$block <= 14,],
                          test4.early3[test4.early3$block >= 10 & test4.early3$block <= 14,], 
                          test4.late3[test4.late3$block >= 10 & test4.late3$block <= 14,])
early3.late3.tone1[,c(4,5)] <- NULL
temp <- early3.late3.tone1[, c(-1, -2)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
early3.late3.tone1[, c(-1, -2)] <- temp
early3.late3.tone1 <- early3.late3.tone1 %>%
    gather(key = "phase", value = "z", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(pos = sum(z == 1)/15*100) %>%
    separate(phase, into = c("phase", "cycles")) %>%
    mutate(type = NA)

early3.late3.tone2[,c(4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26)] <- NULL
temp <- early3.late3.tone2[, c(-1, -2)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
early3.late3.tone2[, c(-1, -2)] <- temp
early3.late3.tone2 <- early3.late3.tone2 %>%
    gather(key = "phase", value = "z", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(pos = sum(z == 1)/5*100) %>%
    separate(phase, into = c("phase", "cycles")) %>%
    mutate(type = NA)
early3.late3.tone <- rbind(early3.late3.tone1, early3.late3.tone2)
# calculate percentage of active time

early3.late3.tone$type[early3.late3.tone$cell %in% type1] <- "Type I"
early3.late3.tone$type[early3.late3.tone$cell %in% type2] <- "Type II"
early3.late3.tone$type[early3.late3.tone$cell %in% type3] <- "Type III"
early3.late3.tone$cell <- as.character(early3.late3.tone$cell)
early3.late3.tone$phase <- factor(early3.late3.tone$phase, levels = c("training", "test1", "test2", "test3", "test4"))

# lollipop plot
ggplot(temp1)+
    geom_segment(aes(x = cell, xend = cell, y = early, yend = late, color = trend))+
    geom_point(aes(x = cell, y = early), fill = "black", shape = 21, size = 2)+
    geom_point(aes(x = cell, y = late), fill = "white", shape = 21, size = 2)+
    facet_grid(type~phase)

# line plot
temp1 <- spread(early3.late3.tone, key = "cycles", value = "pos")
increased <- temp1$cell[temp1$phase == "training" & temp1$early < temp1$late]
decreased <- temp1$cell[temp1$phase == "training" & temp1$early > temp1$late]
unchanged <- temp1$cell[temp1$phase == "training" & temp1$early == temp1$late]
temp1$trend <- NA
temp1$trend[temp1$cell %in% increased] <- "increased"
temp1$trend[temp1$cell %in% decreased] <- "decreased"
temp1$trend[temp1$cell %in% unchanged] <- "unchanged"
temp1 <- gather(temp1, key = "cycles", value = "z", -cell, -phase, -type, -trend)
ggplot(temp1, aes(x = cycles, y = z, color = trend, group = cell))+
    geom_line()+
    facet_grid(type~phase)


# combine trace periods of all phases
early3.late3.trace <- cbind(training.early3[training.early3$block >= 25 & training.early3$block <= 54,], 
                          training.late3[training.late3$block >= 25 & training.late3$block <= 54,], 
                          test1.early3[test1.early3$block >= 15 & test1.early3$block <= 44,], 
                          test1.late3[test1.late3$block >= 15 & test1.late3$block <= 44,], 
                          test2.early3[test2.early3$block >= 15 & test2.early3$block <= 44,], 
                          test2.late3[test2.late3$block >= 15 & test2.late3$block <= 44,], 
                          test3.early3[test3.early3$block >= 15 & test3.early3$block <= 44,], 
                          test3.late3[test3.late3$block >= 15 & test3.late3$block <= 44,],
                          test4.early3[test4.early3$block >= 15 & test4.early3$block <= 44,], 
                          test4.late3[test4.late3$block >= 15 & test4.late3$block <= 44,])
early3.late3.trace[,c(4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29)] <- NULL
temp <- early3.late3.trace[, c(-1, -2)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
early3.late3.trace[, c(-1, -2)] <- temp

# calculate percentage of active time
early3.late3.trace <- early3.late3.trace %>%
    gather(key = "phase", value = "z", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(pos = sum(z == 1)/30*100) %>%
    separate(phase, into = c("phase", "cycles")) %>%
    mutate(type = NA)
early3.late3.trace$type[early3.late3.trace$cell %in% type1] <- "Type I"
early3.late3.trace$type[early3.late3.trace$cell %in% type2] <- "Type II"
early3.late3.trace$type[early3.late3.trace$cell %in% type3] <- "Type III"
early3.late3.trace$cell <- as.character(early.late.trace$cell)
early3.late3.trace$phase <- factor(early3.late3.trace$phase, levels = c("training", "test1", "test2", "test3", "test4"))

# line plot
early3.late3.trace <- spread(early3.late3.trace, key = "cycles", value = "pos")
increased <- early3.late3.trace$cell[early3.late3.trace$phase == "training" & 
                                         early3.late3.trace$early < early3.late3.trace$late]
decreased <- early3.late3.trace$cell[early3.late3.trace$phase == "training" & 
                                         early3.late3.trace$early > early3.late3.trace$late]
unchanged <- early3.late3.trace$cell[early3.late3.trace$phase == "training" & 
                                         early3.late3.trace$early == early3.late3.trace$late]
early3.late3.trace$trend <- NA
early3.late3.trace$trend[early3.late3.trace$cell %in% increased] <- "increased"
early3.late3.trace$trend[early3.late3.trace$cell %in% decreased] <- "decreased"
early3.late3.trace$trend[early3.late3.trace$cell %in% unchanged] <- "unchanged"
early3.late3.trace <- gather(early3.late3.trace, key = "cycles", value = "z", -cell, -phase, -type, -trend)

a <- ggplot(early3.late3.trace[early3.late3.trace$trend == "increased",], aes(x = cycles, y = z, group = cell))+
    geom_line(color = "red")+
    geom_point(size = 2, alpha = 0.4)+
    facet_grid(type~phase)+
    theme_pubr()
b <- ggplot(early3.late3.trace[early3.late3.trace$trend == "decreased",], aes(x = cycles, y = z, group = cell))+
    geom_line(color = "green")+
    geom_point(size = 2, alpha = 0.4)+
    facet_grid(type~phase)+
    theme_pubr()
c <- ggplot(early3.late3.trace[early3.late3.trace$trend == "unchanged",], aes(x = cycles, y = z, group = cell))+
    geom_line(color = "blue")+
    geom_point(size = 2, alpha = 0.4)+
    scale_y_continuous(limits = c(0, 100))+
    facet_grid(type~phase)+
    theme_pubr()
fig <- ggarrange(a, b, c, labels = c("A", "B", "C"), ncol = 1, nrow = 3)
ggsave(fig, filename = "early3 vs late3.png", height = 12, width = 10)



