library(tidyverse)
library(ggpubr)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(EBImage)

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# read Z score of tone files of 5 mice
z.training <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z training all.csv"))
    temp$cell <- temp$cell+100*i
    z.training <- rbind(z.training, temp)
}

shock <- z.training %>%
    filter(time > 55, time < 60) %>%
    group_by(cell) %>%
    summarise(max = max(z), min = min(z))
shock$z <- ifelse(abs(shock$max)>abs(shock$min), shock$max, shock$min)

type1 <- as.character(shock$cell[shock$z >= 3])
type2 <- as.character(shock$cell[shock$z < 3 & shock$z > -3])
type3 <- as.character(shock$cell[shock$z <= -3])

training <- z.training %>%
    spread(key = "time", value = "z") %>%
    mutate(type = NA)
training$type[training$cell %in% type1] <- "Type I"
training$type[training$cell %in% type2] <- "Type II"
training$type[training$cell %in% type3] <- "Type III"
training$cell <- as.character(training$cell)

temp <- training[, c(-1, -765)]
temp[temp < 3 & temp > -3] <- 0
temp[temp >= 3] <- 1
temp[temp <= -3] <- -1
training[, c(-1, -765)] <- temp
training <- training %>%
    mutate(tone.sum = rowSums(training[, 120:295]), trace.sum = rowSums(training[, 296:647])) %>%
    arrange(desc(tone.sum), desc(trace.sum))

# figure 4a
col <- colorRamp2(c(1, 0, -1), brewer.pal(3, "RdYlBu"))
png("new figure 4a.png", height = 1200, width = 600)
hm1 <- Heatmap(training[,120:295],col = col, name = "Training",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Tone", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = training$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 174), labels=c("10", "15", "20", "25"), 
                                         side="bottom"), gp=gpar(fontsize=18)),
               heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           labels_gp=gpar(fontsize=18), at=c(-1,0,1),
                                           grid_height = unit(6, "mm"), grid_width = unit(6, "mm")))
hm2 <- Heatmap(training[,296:647],col = col, name = "Training",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Trace", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = training$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 60, 118, 177, 236, 295, 351), labels=c("26", "30", "35", "40", "45", "50", "55"), 
                                         side="bottom"), gp=gpar(fontsize=18)))
               #heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           #labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm3 <- Heatmap(training[,648:706],col = col, name = "Training",
               cluster_columns = FALSE,cluster_rows = FALSE,
               column_title = "Shock", column_title_gp = gpar(fontsize = 20),
               column_title_side = "bottom",
               show_row_names = FALSE, show_column_names = FALSE,
               row_title_gp = gpar(fontsize = 18),
               split = training$type, gap = unit(3, "mm"),
               bottom_annotation = columnAnnotation(
                   link=column_anno_link(at=c(1, 59), labels=c("56", "60"), 
                                         side="bottom"), gp=gpar(fontsize=18)))
               #heatmap_legend_param = list(title_gp=gpar(fontsize=18), color_bar = "discrete",
                                           #labels_gp=gpar(fontsize=18), at=c(-1,0,1)))
hm1+hm2+hm3
dev.off()

img <- readImage("new figure 4a.png")
df <- data.frame()
f4a <- ggplot(df)+
    geom_blank()+
    background_image(img)

# read Z score files of test1
test1 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test1 all.csv"))
    temp$cell <- temp$cell+100*i
    test1 <- rbind(test1, temp)
}
test1 <- rename(test1, test1 = z)

# read Z score files of test2
test2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test2 all.csv"))
    temp$cell <- temp$cell+100*i
    test2 <- rbind(test2, temp)
}
test2 <- rename(test2, test2 = z)

# read Z score files of test3
test3 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test3 all.csv"))
    temp$cell <- temp$cell+100*i
    test3 <- rbind(test3, temp)
}
test3 <- rename(test3, test3 = z)

# read Z score files of test4
test4 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z 1sec test4 all.csv"))
    temp$cell <- temp$cell+100*i
    test4 <- rbind(test4, temp)
}
test4 <- rename(test4, test4 = z)

# combine trace periods of all phases
test.trace <- cbind(test1, test2, test3, test4)
test.trace[,c(4,5,7,8,10,11,13,14)] <- NULL
test.trace <- test.trace %>%
    filter(block >= 15, block <= 44) %>%
    rename(`Test 1` = test1, `Test 2` = test2, `Test 3` = test3, `Test 4` = test4)
temp <- test.trace[, c(-1, -2)]
temp[temp < 3] <- 0
temp[temp >= 3] <- 1
test.trace[, c(-1, -2)] <- temp

# calculate probability mass function
p <- sum(test.trace[, c(-1, -2)] == 1)/(nrow(test.trace)*4) # p=0.064
theoretical <- data.frame(time = 0:30, Theoretical = dbinom(x = 0:30, prob = p, size = 30))
test.trace.sum <- test.trace %>%
    gather(key = "phase", value = "value", -cell, -block) %>%
    group_by(cell, phase) %>%
    summarise(time = sum(value))
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

# figure 4b
f4b <- ggplot(distribution, aes(x = type, y = probability, fill = time))+
    geom_bar(stat = "identity")+
    scale_fill_brewer(palette = "Set3")+
    scale_x_discrete(limits = c("Theoretical", "Test 1", "Test 2", "Test 3", "Test 4"))+
    labs(x = "Phase", y = "Probability", fill = "Time (sec)")+
    theme_pubr()

# figure 4c
test.trace <- cbind(test1, test2, test3, test4)
test.trace[,c(4,5,7,8,10,11,13,14)] <- NULL
test.trace <- test.trace %>%
    filter(block >= 15, block <= 44) %>%
    rename(`Test 1` = test1, `Test 2` = test2, `Test 3` = test3, `Test 4` = test4)
temp <- test.trace[, c(-1, -2)]
temp[temp > -3] <- 0
temp[temp <= -3] <- -1
test.trace[, c(-1, -2)] <- temp

# calculate probability mass function
p <- sum(test.trace[, c(-1, -2)] == -1)/(nrow(test.trace)*4) # p=0.035
theoretical <- data.frame(time = 0:30, Theoretical = dbinom(x = 0:30, prob = p, size = 30))
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
f4c <- ggplot(distribution, aes(x = type, y = probability, fill = time))+
    geom_bar(stat = "identity")+
    scale_fill_brewer(palette = "Set3")+
    scale_x_discrete(limits = c("Theoretical", "Test 1", "Test 2", "Test 3", "Test 4"))+
    labs(x = "Phase", y = "Probability", fill = "Time (sec)")+
    theme_pubr()

figure4 <- ggarrange(f4a, ggarrange(f4b, f4c, labels = c("B", "C"), nrow = 2),
                     labels = c("A", ""))
figure4 <- annotate_figure(figure4, fig.lab = "Figure 4", fig.lab.face = "bold",
                           fig.lab.size = 14, top = text_grob(""))
ggsave(figure4, filename = "new figure 4.png", height = 9, width = 9)



ranking <- training.late2 %>%
    select(cell, 178:529) %>%
    gather(key = "time", value = "z", -cell) %>%
    group_by(cell) %>%
    summarise(percent = sum(z == 1)/n()*100)
ranking <- ranking %>%
    group_by(percent) %>%
    summarise(count = n()/80)
hist(ranking$pos)
p <- sum(training.late2[, 178:529] == 1)/(ncol(training.late2[, 178:529])*nrow(training.late2)) # p=0.27
theoretical <- data.frame(percent = (0:ncol(training.late2[, 178:529]))/ncol(training.late2[, 178:529])*100, 
                          Theoretical = dbinom(x = 0:ncol(training.late2[, 178:529]), prob = p, 
                                               size = ncol(training.late2[, 178:529])))
plot(theoretical$time, theoretical$Theoretical)
distribution <- full_join(theoretical, ranking, by = "percent")
distribution <- gather(distribution, key = "type", value = "frequency", -percent)
distribution$frequency[is.na(distribution$frequency)] <- 0
ggplot(distribution, aes(x = percent, y = frequency, color = type))+
    geom_point()+
    geom_line()