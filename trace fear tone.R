library(scales)
library(tidyverse)
library(readxl)
library(VennDiagram)
setwd("E:/Live imaging")
source("E:/Live imaging/cusum.z.tone515.R")

# process all trace fear extinction folders for 5 mice
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]
for(i in 1:length(allfolders)){
    cusum.z.tone515(allfolders[i])
}
# load z score files of 5 mice
all.tone <- data.frame()
for(i in 1:length(allfolders)){
    tone <- read.csv(paste0("./", allfolders[i], "/z score 515.csv"))
    tone$cell <- tone$cell+100*i
    all.tone <- rbind(all.tone, tone)
}

# process all trace fear extinction folders with control cycles for 5 mice
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]
for(i in 1:length(allfolders)){
    cusum.z.control(allfolders[i])
}

# load z score files of 5 mice
z.training <- data.frame()
for(i in 1:length(allfolders)){
    training <- read.csv(paste0("./", allfolders[i], "/z training cycles.csv"))
    training$cell <- training$cell+100*i
    z.training <- rbind(z.training, tone)
}

# process trace fear conditioning for 2 mice
cusum.z.tone515("20160211 trace fear conditioning")
cusum.z.tone515("20160211 trace fear conditioning 2")
cusum.z.tone515("20160302 trace fear conditioning")
cusum.z.tone515("20160302 trace fear conditioning 2")
cusum.z.tone515("20160302 trace fear conditioning 3")

# load all z score files of 2 mice
exp6.1<-read.csv('./20160211 trace fear conditioning/z score 515.csv')
exp6.1$cell<-exp6.1$cell+600
exp6.2<-read.csv('./20160211 trace fear conditioning 2/z score 515.csv')
exp6.2$cell<-exp6.2$cell+612
exp7.1<-read.csv('./20160302 trace fear conditioning/z score 515.csv')
exp7.1$cell<-exp7.1$cell+700
exp7.2<-read.csv('./20160302 trace fear conditioning 2/z score 515.csv')
exp7.2$cell<-exp7.2$cell+712
exp7.3<-read.csv('./20160302 trace fear conditioning 3/z score 515.csv')
exp7.3$cell<-exp7.3$cell+700

# bind all z scores
all.tone0 <- rbind(exp6.1, exp6.2, exp7.1, exp7.2, exp7.3)
all.tone0 <- rbind(exp6.1, exp6.2)
all.tone0 <- all.tone0[,1:4]
all.tone0 <- all.tone0[complete.cases(all.tone0),]

all.tone <- all.tone[complete.cases(all.tone),]
test1 <- all.tone[all.tone[,2]>3&all.tone[,3]<3&all.tone[,4]>3,]
test2 <- all.tone[all.tone[,2]>3&all.tone[,3]>3&all.tone[,4]<3,]
test3 <- all.tone[all.tone[,2]<3&all.tone[,3]>3&all.tone[,4]>3,]

all.tone.5 <- all.tone
all.tone.5[, -1] <- all.tone.5[, -1] > 3
all.tone.5$count <- apply(all.tone.5[, -1], 1, sum)
counts <- table(all.tone.5$count)
m <- mean(all.tone.5$count)
p <- m/(ncol(all.tone.5)-2)
pbinom(tail(counts, 1)[[1]]-1, nrow(all.tone.5), p^(ncol(all.tone.5)-2), lower.tail = FALSE)

all.tone.4 <- all.tone[, -6]
all.tone.4[, -1] <- all.tone.4[, -1] > 3
all.tone.4$count <- apply(all.tone.4[, -1], 1, sum)
counts <- table(all.tone.4$count)
m <- mean(all.tone.4$count)
p <- m/(ncol(all.tone.4)-2)
pbinom(tail(counts, 1)[[1]]-1, nrow(all.tone.4), p^(ncol(all.tone.4)-2), lower.tail = FALSE)

all.tone.3 <- all.tone[, -(5:6)]
all.tone.3[, -1] <- all.tone.3[, -1] > 3
all.tone.3$count <- apply(all.tone.3[, -1], 1, sum)
counts <- table(all.tone.3$count)
m <- mean(all.tone.3$count)
p <- m/(ncol(all.tone.3)-2)
pbinom(tail(counts, 1)[[1]]-1, nrow(all.tone.4), p^(ncol(all.tone.3)-2), lower.tail = FALSE)

all.tone0[, -1] <- ifelse(all.tone0[,-1] > 3, 1, 0)
all.tone0$count <- apply(all.tone0[, -1], 1, sum)
counts <- table(all.tone0$count)
m <- mean(all.tone0$count)
p <- m/(ncol(all.tone0)-2)
pbinom(tail(counts, 1)[[1]]-1, nrow(all.tone0), p^(ncol(all.tone0)-2), lower.tail = FALSE)


more.tone <- rbind(all.tone.3, all.tone0)
counts <- table(more.tone$count)
m <- mean(more.tone$count)
p <- m/(ncol(more.tone)-2)
pbinom(tail(counts, 1)[[1]]-1, nrow(more.tone), p^(ncol(more.tone)-2), lower.tail = FALSE)

all.tone.pos.train <- all.tone[all.tone[,2]>3,-(5:6)]
all.tone.pos.train[, -1] <- ifelse(all.tone.pos.train[,-1] > 3, 1, 0)
all.tone.pos.train$count <- apply(all.tone.pos.train[, -1], 1, sum)
counts <- table(all.tone.pos.train$count)
m <- mean(all.tone.pos.train$count)
p <- m/(ncol(all.tone.pos.train)-2)
pbinom(tail(counts, 1)[[1]]-1, nrow(all.tone.pos.train), p^(ncol(all.tone.pos.train)-2), lower.tail = FALSE)


# make h clusters and reorder all cells
row.order<-hclust(dist(all.exp[,2:4]))$order
all.exp.new<-all.exp[row.order,]

# generate tidy data frame and phase and cell columns into factors
all.exp.tidy<-gather(all.exp.new, key = "phase", value = "z", -cell)
all.exp.tidy$phase<-factor(all.exp.tidy$phase, levels = unique(all.exp.tidy$phase))
all.exp.tidy$cell<-factor(all.exp.tidy$cell, levels = unique(all.exp.tidy$cell))
# rescale z scores
all.exp.heatmap<-mutate(all.exp.tidy, z=ifelse(all.exp.tidy$z > 3, 3, all.exp.tidy$z))
all.exp.heatmap$z<-ifelse(all.exp.heatmap$z < -3, -3, all.exp.heatmap$z)

# make a heat map
ggplot(data = all.exp.heatmap, aes(x=phase, y=cell, fill=z))+
    geom_tile()+theme_minimal()+
    scale_fill_gradient2(low = muted("blue"), high = muted("red"), mid = "white", 
                         midpoint = 0, limit = c(-4,4))
ggsave("heatmap_all.png", width = 4, height = 8)
write.csv(all.exp, "z score all.csv", row.names = FALSE)

# categorize cells based on z scores
all.exp$act<-ifelse(all.exp$training>3&all.exp$test1>3&all.exp$test2>3, 'strong', "inactive")
all.exp$act<-ifelse(all.exp$training>3&all.exp$test1>3&
                        all.exp$test2>2&all.exp$test2<3, 'moderate', all.exp$act)
all.exp$act<-ifelse(all.exp$training>3&all.exp$test2>3&
                        all.exp$test1>2&all.exp$test1<3, 'moderate', all.exp$act)
all.exp$act<-ifelse(all.exp$training>2&all.exp$training<3&
                        all.exp$test1>3&all.exp$test2>3, 'moderate', all.exp$act)
all.exp.act<-gather(all.exp, key = "phase", value = "z", -cell, -act)
all.exp.act$phase<-factor(all.exp.act$phase, levels = unique(all.exp.act$phase))

# plot line chart
ggplot(data = all.exp.act, aes(x=phase, y=z, group=cell, color=act))+
    geom_point(size=2)+
    geom_line(lwd=1.2, aes(linetype=act))+
    scale_color_manual(values = c('grey', 'pink', 'red'))+
    scale_linetype_manual(values=c("dashed", "solid", "solid"))+
    scale_y_continuous(limits = c(-20, 20))+
    geom_hline(yintercept = 3, lty=2)+
    geom_hline(yintercept = -3, lty=2)
ggsave("line_all.png")


files <- list.files("20161122 trace fear extinction 2/28", full.names = TRUE)
fileone <- read.csv(files[1])
dat <- data.frame(time=fileone[,2],cycle1=NA,cycle2=NA,
                  cycle3=NA,cycle4=NA,cycle5=NA,cycle6=NA,cycle7=NA)

for(j in 1:length(files)){
    f <- read.csv(files[j])
    
    # subset one column of data for model fitting
    thesub <- f[,c(2,4)]
    names(thesub) <- c('time','signal')
    
    # fit one phase decay curve with self-start
    d_model <- try(nls(signal ~ SSasymp(time, Asym, r0, lrc), data = thesub), silent = TRUE)
    
    # discard data if not fitted
    if(class(d_model) == 'nls'){
        dat[,j+1] <- resid(d_model)
    } else {
        dat[,j+1] <- NA
    }
}
dat.plot <- gather(dat, key = "cycle", value = "residual", -time)
ggplot(dat.plot, aes(x=time, y=residual, color=cycle))+
    geom_smooth(span=0.05, se=FALSE)+
    facet_grid(cycle~.)+
    geom_vline(xintercept = 10, lty=2)+
    geom_vline(xintercept = 55, lty=2)

fired.shock <- list(Shock = all.shock$cell[all.shock$shock>3], Train.tone = all.tone$cell[all.tone$training>3])
venn.diagram(fired, filename = "venn shock.png", imagetype = "png",
             col = "black",
             lty = "dotted",
             lwd = 4,
             fill = c("green", "red"),
             alpha = 0.50,
             cex = 1.5,
             fontfamily = "serif",
             fontface = "bold",
             cat.col = c("darkgreen", "darkred"),
             cat.cex = 1.5,
             cat.fontfamily = "serif",
             cat.default.pos = "text")

fired.tone <- list(Train.tone = all.tone$cell[all.tone$training>3],
                   Test1.tone = all.tone$cell[all.tone$test1>3], 
                   Test2.tone = all.tone$cell[all.tone$test2>3],
                   Test3.tone = all.tone$cell[all.tone$test3>3], 
                   Test4.tone = all.tone$cell[all.tone$test4>3])
venn.diagram(fired.tone, filename = "venn tone 5.png", imagetype = "png",
             col = "black",
             lty = "dotted",
             lwd = 4,
             fill = c("cornflowerblue", "green", "yellow", "darkorchid1", "red"),
             alpha = 0.50,
             cex = 1.5,
             fontfamily = "serif",
             fontface = "bold",
             cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4", "darkred"),
             cat.cex = 1.5,
             cat.fontfamily = "serif",
             cat.default.pos = "text",
             cat.dist = -0.01)

fired.tone <- list(Train.tone = all.tone$cell[all.tone$training>3],
                   Test1.tone = all.tone$cell[all.tone$test1>3], 
                   Test2.tone = all.tone$cell[all.tone$test2>3],
                   Test3.tone = all.tone$cell[all.tone$test3>3])
venn.diagram(fired.tone, filename = "venn tone 4.png", imagetype = "png",
             col = "black",
             lty = "dotted",
             lwd = 4,
             fill = c("cornflowerblue", "green", "yellow", "darkorchid1"),
             alpha = 0.50,
             cex = 1.5,
             fontfamily = "serif",
             fontface = "bold",
             cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4"),
             cat.cex = 1.5,
             cat.fontfamily = "serif",
             cat.default.pos = "text",
             cat.dist = -0.01)

fired.tone <- list(Train.tone = all.tone$cell[all.tone$training>3],
                   Test1.tone = all.tone$cell[all.tone$test1>3], 
                   Test2.tone = all.tone$cell[all.tone$test2>3])
venn.diagram(fired.tone, filename = "venn tone 3.png", imagetype = "png",
             col = "black",
             lty = "dotted",
             lwd = 4,
             fill = c("cornflowerblue", "green", "yellow"),
             alpha = 0.50,
             cex = 1.5,
             fontfamily = "serif",
             fontface = "bold",
             cat.col = c("darkblue", "darkgreen", "orange"),
             cat.cex = 1.5,
             cat.fontfamily = "serif",
             cat.default.pos = "text",
             cat.dist = -0.01)


all.scaled <- column_to_rownames(all.tone, var = "cell")
all.scaled <- t(scale(all.scaled))
row_dend = hclust(dist(all.scaled)) # row clustering
col_dend = hclust(dist(t(all.scaled))) # column clustering
Heatmap(all.scaled, 
        name = "Z score", #title of legend
        column_title = "Cells", row_title = "Phases", # Text size for row names
        cluster_rows = color_branches(row_dend, k = 2),
        cluster_columns = color_branches(col_dend, k = 6))

densityHeatmap(t(all.scaled))

d.time <- d.all %>%
    gather(key = "cycle", value = "residual", -time, -cell) %>%
    spread(key = "time", value = "residual")
col <- colorRamp2(c(-3000, 0, 3000), brewer.pal(3, "RdBu"))
Heatmap(d.time[,-(1:2)],col = col, name = "Training",
        cluster_columns = FALSE,cluster_rows = FALSE,
        column_title = "Time", column_title_gp = gpar(fontsize = 26),
        column_title_side = "bottom",
        column_names_gp = gpar(fontsize = 22),
        show_row_names = FALSE,
        split = d.time$cycle, gap = unit(3, "mm"))

d.tidy <- gather(d.all, key = "cycle", value = "residual", -time, -cell)
d.tidy$cell <- as.factor(d.tidy$cell)
ggplot(d.tidy, aes(x=time, y=cell, fill=residual))+
    geom_raster()+
    facet_grid(cycle~.)+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", limits=c(-3000, 3000))+
    annotate("rect", xmin = 10, xmax = 25, ymin = 0, ymax = 12, alpha = .2, fill="blue")+
    annotate("rect", xmin = 55, xmax = 56, ymin = 0, ymax = 12, alpha = .2, fill="red")
