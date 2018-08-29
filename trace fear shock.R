library(scales)
library(tidyverse)
setwd("E:/Live imaging")
source("E:/Live imaging/cusum.z.shock5060.R")

# process all trace fear extinction folders
allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]
for(i in 1:length(allfolders)){
    cusum.z.shock5060(allfolders[i])
}

# load z score files of 5 mice
all.shock <- data.frame()
for(i in 1:length(allfolders)){
    shock <- read.csv(paste0("./", allfolders[i], "/z score 5060.csv"))
    shock$cell <- shock$cell+100*i
    all.shock <- rbind(all.shock, shock)
}

# bind all z scores
all.exp <- merge(all.shock, all.tone, by = "cell")

# make h clusters and reorder all cells
shock.pos <- all.exp[all.exp[,2]>3&all.exp[,3]>3&all.exp[,4]>3&all.exp[, 5]>3,]
shock.neg <- all.exp[all.exp$shock<0,]

row.order<-hclust(dist(shock.pos[,2:3]))$order
shock.pos.new<-shock.pos[row.order,]

# generate tidy data frame and phase and cell columns into factors
all.exp.tidy<-gather(shock.pos.new, key = "phase", value = "z", -cell)
all.exp.tidy$phase<-factor(all.exp.tidy$phase, levels = unique(all.exp.tidy$phase))
all.exp.tidy$cell<-factor(all.exp.tidy$cell, levels = unique(all.exp.tidy$cell))
# rescale z scores
all.exp.heatmap<-mutate(all.exp.tidy, z=ifelse(all.exp.tidy$z > 3, 3, all.exp.tidy$z))
all.exp.heatmap$z<-ifelse(all.exp.heatmap$z < -3, -3, all.exp.heatmap$z)

# make a heat map
ggplot(data = all.exp.heatmap, aes(x=phase, y=cell, fill=z))+
    geom_tile()+theme_minimal()+
    scale_fill_gradient2(low = muted("blue"), high = muted("red"), mid = "white", 
                         midpoint = 0, limit = c(-3,3))
ggsave("heatmap5060.png", width = 4, height = 8)
write.csv(all.exp, "z score 5060.csv", row.names = FALSE)

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
ggsave("line5060.png")