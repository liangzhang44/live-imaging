library(ReporteRs)
library(readxl)
library(tidyverse)
library(ggpubr)
library(ggsci)
library(EBImage)
library(zoo)
library(ez)
library(multcomp)

# Create a PowerPoint document
options( "ReporteRs-fontsize" = 14, 
         "ReporteRs-default-font" = "Arial")
doc = pptx()

# Slide 1 : Title
doc <- addSlide(doc, "Title Slide")
doc <- addTitle(doc,"ANOVA and post hoc tests in R")
doc <- addSubtitle(doc, 
"Liang Zhang
4-13-2018")

# Slide 2 : Packages
doc <- addSlide(doc, "Comparison")
doc <- addTitle(doc, "Packages: tidyverse, ggpubr, ez, multcomp")
doc <- addParagraph(doc, "tidyverse")
doc <- addParagraph(doc, value = c("tidyr", "dplyr", "readr", "ggplot2"),
                    par.properties =  parProperties(list.style = 'unordered'))
doc <- addParagraph(doc, "ggpubr")
doc <- addParagraph(doc, value = c("ggscatter()", "ggbarplot()", "ggboxplot()", "...",
                                           "stat_compare_means()", "compare_means()", "theme_pubr"),
                    par.properties =  parProperties(list.style = 'unordered'))

# Slide 3: BrdU barplot
E3brdu <- read_xlsx("20171229 BrdU NeuN counting.xlsx", sheet = 1, na = "NA")
E3neun <- read_xlsx("20171229 BrdU NeuN counting.xlsx", sheet = 2, na = "NA")
E4brdu <- read_xlsx("20171229 BrdU NeuN counting.xlsx", sheet = 3, na = "NA")
E4neun <- read_xlsx("20171229 BrdU NeuN counting.xlsx", sheet = 4, na = "NA")

E3brdu <- E3brdu %>%
    mutate(total=rowSums(E3brdu[,-1])*8) %>%
    dplyr::select(mouse, total) %>%
    rename(E3BrdU = total)
E3neun <- E3neun %>%
    mutate(total=rowSums(E3neun[,-1])*8) %>%
    dplyr::select(mouse, total) %>%
    rename(E3NeuN = total)
E4brdu <- E4brdu %>%
    mutate(total=rowSums(E4brdu[,-1])*8) %>%
    dplyr::select(mouse, total) %>%
    rename(E4BrdU = total)
E4neun <- E4neun %>%
    mutate(total=rowSums(E4neun[,-1])*8) %>%
    dplyr::select(mouse, total) %>%
    rename(E4NeuN = total)

males <- merge(merge(E3brdu, E3neun, by = "mouse"), merge(E4brdu, E4neun, by = "mouse"),
               by = "mouse")

males$treatment <- ifelse(males$mouse %in% c("Cd1", "Cd2", "Cd3", "Cd4", "Cd5"),
                          "Cd", "Control")
males <- gather(males, key = "genotype", value = "number", -mouse, -treatment)
brdu <- males[males$genotype %in% c("E3BrdU", "E4BrdU"),]
brdu$genotype <- ifelse(brdu$genotype == "E3BrdU", "ApoE3", "ApoE4")
brdu$treatment <- factor(brdu$treatment, levels = c("Control", "Cd"))

plot1 <- ggbarplot(data = brdu, x = "genotype", y = "number", color = "treatment", palette = "jco",
               add = "mean_se", position = position_dodge(0.8))+
    stat_compare_means(aes(group=treatment), label = "p.signif",
                       method = "t.test", hide.ns = TRUE)+
    labs(x = NULL, y = "BrdU+ cells per DG", color = "")

doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Example 1")
doc <- addPlot(doc, print, x = plot1)
code1 ='ggbarplot(data = brdu, x = "genotype", 
                   y = "number", color = "treatment", 
                   palette = "jco", add = "mean_se", 
                   position = position_dodge(0.8)) +
    stat_compare_means(aes(group = treatment), 
                       label = "p.signif",
                       method = "t.test", 
                       hide.ns = TRUE) +
    labs(x = NULL, 
         y = "BrdU+ cells per DG", 
         color = "")'
doc <- addRScript(doc, text = code1)

# Slide 4: Example 2 diagram
df <- data.frame(
    left = c(seq(1, length.out = 7, by = 2), seq(46, length.out = 5, by = 2),
             seq(67, length.out = 5, by = 2), seq(88, length.out = 5, by = 2),
             seq(109, length.out = 5, by = 2)),
    right = c(seq(2, length.out = 7, by = 2), seq(47, length.out = 5, by = 2),
              seq(68, length.out = 5, by = 2), seq(89, length.out = 5, by = 2),
              seq(110, length.out = 5, by = 2)),
    up = rep(2, 27),
    down = rep(-2, 27)
)

f1a <- ggplot(df, aes(xmin=left, xmax=right, ymin=down, ymax=up))+
    ylim(-8, 8)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = seq(55, length.out = 3, by = 21), xend = seq(67, length.out = 3, by = 21), y = 0, yend = 0)+
    annotate("text", x = 30, y = 1, label = "3 hours", color = "blue", size = 4)+
    annotate("text", x = seq(61, length.out = 3, by = 21), y = 1, label = "1 hour", color = "blue", size = 4)+
    annotate("text", x = 7.5, y = -3.5, label = "Conditioning", size = 5)+
    annotate("text", x = 81, y = 3.5, label = "Retrieval/Extinction", size = 5)+
    annotate("text", x = 7.5, y = -2.5, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = 2.5, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = 2.5, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = 2.5, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = 2.5, label = "Test 4", size = 4)+
    annotate("rect", xmin = 0, xmax = 97.5, ymin = 5.5, ymax = 7, color = "black", fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 15, xmax = 37.5, ymin = 5.5, ymax = 7, alpha = 1, fill = "blue")+
    annotate("segment", x = 82.5, xend = 82.5, y = 5.5, yend = 7, size = 1.5, color = "red")+
    annotate("text", x = 26, y = 7.5, label = "Tone", color = "blue", size = 4)+
    annotate("text", x = 82.5, y = 7.5, label = "Shock", color = "red", size = 4)+
    annotate("text", x = 60, y = 7.5, label = "Trace", size = 4)+
    annotate("text", x = 7.5, y = 6.25, label = "10 sec", size = 4)+
    annotate("text", x = 26, y = 6.25, label = "15 sec", size = 4, color = "white")+
    annotate("text", x = 60, y = 6.25, label = "30 sec", size = 4)+
    annotate("text", x = 90, y = 6.25, label = "10 sec", size = 4)+
    annotate("segment", x = 0, xend = 3, y = 5.5, yend = 2.5, lty = 2)+
    annotate("segment", x = 4, xend = 97.5, y = 2.5, yend = 5.5, lty = 2)+
    annotate("segment", x = 3, xend = 3, y = 2, yend = 2.5, lty = 1)+
    annotate("segment", x = 4, xend = 4, y = 2, yend = 2.5, lty = 1)+
    annotate("rect", xmin = 51.5, xmax = 119, ymin = -7, ymax = -5.5, color = "black", fill = "yellow", alpha = 0.4)+
    annotate("rect", xmin = 66.5, xmax = 74, ymin = -7, ymax = -5.5, alpha = 1, fill = "blue")+
    annotate("text", x = 70.25, y = -7.5, label = "Tone", color = "blue", size = 4)+
    annotate("text", x = 96.5, y = -7.5, label = "Trace", size = 4)+
    annotate("text", x = 59, y = -6.25, label = "10 sec", size = 4)+
    annotate("text", x = 70.25, y = -6.25, label = "5 sec", size = 4, color = "white")+
    annotate("text", x = 96.5, y = -6.25, label = "30 sec", size = 4)+
    annotate("segment", x = 51.5, xend = 115, y = -5.5, yend = -2.5, lty = 2)+
    annotate("segment", x = 116, xend = 119, y = -2.5, yend = -5.5, lty = 2)+
    annotate("segment", x = 115, xend = 115, y = -2, yend = -2.5, lty = 1)+
    annotate("segment", x = 116, xend = 116, y = -2, yend = -2.5, lty = 1)

doc <- addSlide(doc, "Title and Content")
doc <- addTitle(doc, "Example 2: Trace Fear Conditioning")
doc <- addPlot(doc, print, x = f1a)

# Slide 5: Example 2 raw data
img2a <- readImage("new figure 2a.png")
f2a <- ggplot()+
    geom_blank()+
    background_image(img2a)

# insets
df <- data.frame(
    left = c(seq(1, length.out = 7, by = 2), seq(46, length.out = 5, by = 2),
             seq(67, length.out = 5, by = 2), seq(88, length.out = 5, by = 2),
             seq(109, length.out = 5, by = 2)),
    right = c(seq(2, length.out = 7, by = 2), seq(47, length.out = 5, by = 2),
              seq(68, length.out = 5, by = 2), seq(89, length.out = 5, by = 2),
              seq(110, length.out = 5, by = 2)),
    up = rep(2, 27),
    down = rep(-2, 27)
)

diagram1 <- ggplot(df, aes(xmin = left, xmax = right, ymin = down, ymax = up))+
    ylim(-6, 10)+
    xlim(-20, 120)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("segment", x = 3.5, xend = 3.5, y = 6, yend = 2.5, arrow = arrow(length = unit(0.1, "inches")))+
    annotate("text", x = 10, y = 7, label = "Session 2", size = 5)+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = 55, xend = 67, y = 0, yend = 0)+
    annotate("segment", x = 76, xend = 88, y = 0, yend = 0)+
    annotate("segment", x = 97, xend = 109, y = 0, yend = 0)+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 4)

# figure 2b
cycle2 <- read.csv("./20161122 trace fear extinction 2/28/timelapse_28_0002.csv")
cycle2 <- cycle2[,c(2, 6, 7)]
colnames(cycle2) <- c("Time", "Cell4", "Cell5")

cycle2[3:(dim(cycle2)[1]-2),2] <- rollmean(cycle2[,2],5)
cycle2[3:(dim(cycle2)[1]-2),3] <- rollmean(cycle2[,3],5)
cycle2 <- cycle2[3:(dim(cycle2)[1]-2),]

temp1 <- filter(cycle2, Time < 10 | Time > 63)
model1 <- try(nls(Cell4 ~ SSasymp(Time, Asym, r0, lrc), data = temp1), silent = TRUE)
model2 <- try(nls(Cell5 ~ SSasymp(Time, Asym, r0, lrc), data = temp1), silent = TRUE)

curve1 <- cycle2
curve1$Cell4 <- predict(model1, list(Time = cycle2$Time))
curve1$Cell5 <- predict(model2, list(Time = cycle2$Time))

cycle2 <- gather(cycle2, key = "Cell", value = "Fluorescence", -Time)
curve1 <- gather(curve1, key = "Cell", value = "Fluorescence", -Time)

f2b2 <- ggplot(cycle2, aes(Time, Fluorescence, color = Cell))+
    geom_smooth(span = 0.05, se = FALSE)+
    geom_line(data = curve1, aes(Time, Fluorescence), lty = 2, lwd = 1.5)+
    scale_y_continuous(limits = c(340, 410))+
    annotate("rect", xmin = 10, xmax = 25, ymin = -Inf, ymax = 400, fill = "blue", alpha = 0.4)+
    annotate("rect", xmin = 55.5, xmax = 56.5, ymin = -Inf, ymax = 400, fill = "red", alpha = 0.4)+
    annotate("text", x = 17.5, y = 403, label = "Tone", color = "blue", size = 5)+
    annotate("text", x = 56, y = 403, label = "Shock", color = "red", size = 5)+
    annotate("text", x = 62, y = 370, label = "Cell a", color = "darkblue", size = 5)+
    annotate("text", x = 62, y = 346, label = "Cell b", color = "red", size = 5)+
    annotate("text", x = 32.5, y = 410, label = "Training (Session 2)", size = 6)+
    annotate("segment", x = 10.5, xend = 17.5, y = 389, yend = 389,
             arrow = arrow(length = unit(0.05, "inches"), ends = "both", angle = 90))+
    scale_color_aaas(guide = FALSE)+
    labs(x = "Time (sec)", y = "Fluorescence (AU)")+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))+
    theme_pubr()

f2b <- ggarrange(diagram1, f2b2, ncol = 1, nrow = 2, heights = c(1, 3))

doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc,"Example 2: Raw Data")
doc <- addPlot(doc, print, x = f2a)
doc <- addPlot(doc, print, x = f2b)

# Slide 6: population mean
pal <- pal_jama("default")(7)
diagram3 <- ggplot(df, aes(xmin=left, xmax=right, ymin=down, ymax=up))+
    ylim(-6, 10)+
    xlim(-70, 170)+
    theme_void()+
    geom_rect(colour = "black", fill = c(rep("green", 7), rep("yellow", 20)))+
    annotate("segment", x = 1.5, xend = 1.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.1, "inches")), color = pal[1])+
    annotate("segment", x = 3.5, xend = 3.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.1, "inches")), color = pal[1])+
    annotate("segment", x = 11.5, xend = 11.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.1, "inches")), color = pal[2])+
    annotate("segment", x = 13.5, xend = 13.5, y = 5.5, yend = 2.5, 
             arrow = arrow(length = unit(0.1, "inches")), color = pal[2])+
    annotate("segment", x = 14, xend = 46, y = 0, yend = 0)+
    annotate("segment", x = 55, xend = 67, y = 0, yend = 0)+
    annotate("segment", x = 76, xend = 88, y = 0, yend = 0)+
    annotate("segment", x = 97, xend = 109, y = 0, yend = 0)+
    annotate("text", x = 2.5, y = 7.5, label = "1&2", size = 5, color = pal[1])+
    annotate("text", x = 12.5, y = 7.5, label = "6&7", size = 5, color = pal[2])+
    annotate("text", x = 7.5, y = -4, label = "Training", size = 4)+
    annotate("text", x = 50.5, y = -4, label = "Test 1", size = 4)+
    annotate("text", x = 71.5, y = -4, label = "Test 2", size = 4)+
    annotate("text", x = 92.5, y = -4, label = "Test 3", size = 4)+
    annotate("text", x = 113.5, y = -4, label = "Test 4", size = 4)

allfolders <- list.files()
allfolders <- allfolders[grep("extinction", allfolders)]

# read dF/F data of training, first 2 cycles
early2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z training early2.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    early2 <- rbind(early2, temp)
}

early2.mean <- early2 %>%
    group_by(time) %>%
    summarise(z = mean(z))
early.tone.mean <- mean(early2.mean$z[early2.mean$time < 10 & early2.mean$time > 5])
early.tone.sd <- sd(early2.mean$z[early2.mean$time < 10 & early2.mean$time > 5])
early2.tone <- early2 %>%
    mutate(z = (z - early.tone.mean)/early.tone.sd,phase = "Training (Sessions 1&2)")

# read dF/F data of training, last 2 cycles
late2 <- data.frame()
for(i in 1:length(allfolders)){
    temp <- read.csv(paste0("./", allfolders[i], "/z training late2.csv"))
    temp <- temp %>%
        mutate(cell=cell+100*i)
    late2 <- rbind(late2, temp)
}

late2.mean <- late2 %>%
    group_by(time) %>%
    summarise(z = mean(z))
late.tone.mean <- mean(late2.mean$z[late2.mean$time < 10 & late2.mean$time > 5])
late.tone.sd <- sd(late2.mean$z[late2.mean$time < 10 & late2.mean$time > 5])
late2.tone <- late2 %>%
    mutate(z = (z - late.tone.mean)/late.tone.sd, phase = "Training (Sessions 6&7)")

training.tone <- rbind(early2.tone, late2.tone)

f2d2 <- ggline(training.tone, x = "time", y = "z", add = "mean_se", color = "phase", 
               numeric.x.axis = TRUE, size = 0.2)+
    annotate("rect", xmin = 0, xmax = 65, ymin = -13, ymax = -11, color = "black", fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -13, ymax = -11, alpha = 1, fill = "blue")+
    annotate("rect", xmin = 55, xmax = 56, ymin = -13, ymax = -11, alpha = 0.5, fill = "red")+
    annotate("text", x = 17.5, y = -12, label = "Tone", color = "white", size = 4)+
    annotate("text", x = 55.5, y = -12, label = "Shock", color = "red", size = 4)+
    annotate("text", x = 40, y = -12, label = "Trace", size = 4)+
    annotate("text", x = 40, y = -2, label = "Training (Sessions 1&2)", size = 5, color = pal[1])+
    annotate("text", x = 40, y = 13, label = "Training (Sessions 6&7)", size = 5, color = pal[2])+
    geom_vline(xintercept = 10, lty = 2, color = "blue")+
    geom_vline(xintercept = 25, lty = 2, color = "blue")+
    geom_vline(xintercept = 55, lty = 2, color = "red")+
    geom_vline(xintercept = 56, lty = 2, color = "red")+
    labs(x = "Time (sec)", y= "Z Score")+
    scale_color_jama(guide = FALSE)+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          legend.title = element_blank(), legend.text = element_text(size = 14))

f2d <- ggarrange(diagram3, f2d2, ncol = 1, nrow = 2, heights = c(1, 3))

doc <- addSlide(doc, "Title and Content")
doc <- addTitle(doc, "Example 2: Population Means")
doc <- addPlot(doc, print, x = f2d)

# Slide 7: Two-way ANOVA
#tone.aov <- ezANOVA(data = training.tone, dv = z, wid = cell, within_full = time,
 #                   between = phase, type = 2, return_aov = TRUE)
tone.aov$ANOVA
summary(glht(tone.aov$aov,linfct=mcp(phase="Tukey")))

doc <- addSlide(doc, "Content with Caption")
doc <- addTitle(doc, "Example 2: Two-way ANOVA with Repeated Measures")
code2 <- 'tone.aov <- ezANOVA(
data = training.tone, 
dv = z, 
wid = cell, 
within_full = time,
between = phase, 
type = 2, 
return_aov = TRUE)

tone.aov$ANOVA

summary(glht(tone.aov$aov, linfct = mcp(phase = "Tukey")))'

doc <- addImage(doc, "ANOVA results.png")
doc <- addRScript(doc, text = code2)

# Slide 8: post hoc test
doc <- addSlide(doc, "Title and Content")
doc <- addTitle(doc, "Post hoc Tests")
code3 <- 'pvalues <- compare_means(z ~ phase, data = training.tone, 
group.by = "time",
paired = TRUE, 
method = "t.test", 
p.adjust.method = "BH")

pvalues <- filter(pvalues, p.adj < 0.05)'
doc <- addRScript(doc, text = code3)

# Slide 9: table
pvalues <- compare_means(z ~ phase, data = training.tone, group.by = "time",
                          paired = TRUE, method = "t.test", p.adjust.method = "BH")
pvalues <- filter(pvalues, p.adj < 0.05)
doc <- addSlide(doc, "Title and Content")
doc <- addTitle(doc, "Selected Results of Post hoc Tests")
doc <- addFlexTable(doc, vanilla.table(pvalues[1:7,]))

# Slide 10: Populaiton means with p values
f2d2 <- ggline(training.tone, x = "time", y = "z", add = "mean_se", color = "phase", 
               numeric.x.axis = TRUE, size = 0.2)+
    annotate("rect", xmin = 0, xmax = 65, ymin = -13, ymax = -11, color = "black", fill = "green", alpha = 0.4)+
    annotate("rect", xmin = 10, xmax = 25, ymin = -13, ymax = -11, alpha = 1, fill = "blue")+
    annotate("rect", xmin = 55, xmax = 56, ymin = -13, ymax = -11, alpha = 0.5, fill = "red")+
    annotate("text", x = 17.5, y = -12, label = "Tone", color = "white", size = 4)+
    annotate("text", x = 55.5, y = -12, label = "Shock", color = "red", size = 4)+
    annotate("text", x = 40, y = -12, label = "Trace", size = 4)+
    annotate("text", x = 40, y = -2, label = "Training (Sessions 1&2)", size = 5, color = pal[1])+
    annotate("text", x = 40, y = 13, label = "Training (Sessions 6&7)", size = 5, color = pal[2])+
    geom_vline(xintercept = 10, lty = 2, color = "blue")+
    geom_vline(xintercept = 25, lty = 2, color = "blue")+
    geom_vline(xintercept = 55, lty = 2, color = "red")+
    geom_vline(xintercept = 56, lty = 2, color = "red")+
    annotate("text", x = pvalues$time, y = 14, label = "*", size = 5)+
    labs(x = "Time (sec)", y= "Z Score")+
    scale_color_jama(guide = FALSE)+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
          legend.title = element_blank(), legend.text = element_text(size = 14))

f2d <- ggarrange(diagram3, f2d2, ncol = 1, nrow = 2, heights = c(1, 3))

doc <- addSlide(doc, "Title and Content")
doc <- addTitle(doc, "Example 2: Population Means with P Values")
doc <- addPlot(doc, print, x = f2d)

# write the document 
writeDoc(doc, "20180413 for Julia lab.pptx" )
