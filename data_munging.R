require(foreign)
require(dplyr)
require(metafor)
require(ggplot2)
require(gridExtra)
require(xtable)


data1 <- read.dta(file = "cdma_datacollapsed1.dta")
data1 <- subset(data1, rand == "Randomized") ## Randomized only
data1 <- subset(data1, timing == 2) ## Post-tx only

studies <- c("Alexander & Parsons, 1973", "Aultman-Bettridge, 2007", "Barton et al., 1985 Replication II", 
             "Barton et al., 1985 Replication III", "Borduin et al., 1990", "Borduin et al., 1995", "Borduin et al., 2009",
             "Coatsworth et al., 2001", "Dennis et al., 2004",  "Friedman 1989",  
             "Gordon et al., 1988", "Henggeler et al., 1986",
             "Henggeler et al., 1992", "Henggeler et al., 1997", "Henggler et al., 1999", "Leschied & Cunningham, 2002",
             "Letourneau et al., 2009", "Liddle et al., 2001", "Liddle et al., 2004", "Liddle et al., 2008",      
             "Nickel, Luely, et al., 2006", "Nickel, Muehlbacher, et al., 2006", "Ogden & Halliday-Boykins, 2004",
             "Rowland et al., 2005", "Santisteban et al., 2003", "Sundell et al., 2008", "Szapocznik et al., 1989",
             "Timmons-Mitchell et al., 2006",  "Waldron et al., 2001", "Overall")

data1$studyid <- factor(data1$studyid, levels = studies)

data1 <- data1[,c('studyid', 'txtype', 'comptype',  'meastype', 'd', 'v')]

head(data1)
table(data1$timing)

head(data1)



data2 <- subset(data1, meastype == "Criminal Deliquency")
data3 <- subset(data2, comptype == "TAU")
data3$es_type <- "study"

temp <- rma.uni(yi = d, vi = v, data = data3, method = "REML", slab = studyid)
forest(temp)

temp$b[1,1]
temp$yi

data3$studyid <- factor(data3$studyid, levels(data3$studyid)[c(30:1)])

overall_data <- as.data.frame(rbind(t(c(NA, NA, NA, NA, NA, NA, NA)),
                                    t(c(1, NA, NA, NA, temp$b[1,1], (temp$se)^2, 1))))

colnames(overall_data) <- colnames(data3)
overall_data$studyid[2] <- "Overall"
overall_data$es_type[2] <- "Overall"
overall_data$ordering <- c(0, -1)
overall_data

data3$ordering <- as.numeric(data3$studyid)
data3 <- data3[order(-data3$ordering), ]
data3


data3 <- rbind(data3, overall_data)
data3$studyid2 <- rev(seq(1:length(data3$studyid)))
data3 <- subset(data3, ordering != 0)
data3

data3$ll <- data3$d - (1.96 * sqrt(data3$v))
data3$ul <- data3$d + (1.96 * sqrt(data3$v))




plotSettings <- theme(legend.position = "none", axis.line.y = element_blank(), 
                      axis.ticks.y = element_blank(), 
                      plot.margin = unit(c(0, 0, 0, 0), "npc"), 
                      panel.margin = unit(c(-10, -10, -10, -10), "npc"), 
                      rect = element_blank(), 
                      axis.line = element_line(colour = "black", size = 0.3, linetype = 1), 
                      axis.title.x = element_blank(), 
                      axis.title.y = element_blank(), 
                      panel.grid.major = element_line(color = "red"), 
                      axis.text.y = element_blank())

# you can comment the next string for debug
plotSettings <- plotSettings + theme(panel.grid = element_blank(), 
                                     axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
                                     panel.grid.major = element_blank())

plotSettings2 <- theme(axis.ticks.x = element_blank(), 
                       axis.line.x = element_blank(), 
                       axis.text.x = element_text(color = "white"), 
                       axis.title.x = element_text(color = "white"))



mainPart <- ggplot(data3, aes(x = studyid2, y = d, 
                              ymin = d - (1.96 * sqrt(v)), 
                              ymax = d + (1.96 * sqrt(v)), 
                              shape = es_type)) +
  geom_segment(aes(x = 0, xend = max(data3$studyid2) + 1, 
                   y = 0, yend = 0), linetype = 3) +
  geom_errorbar(width = .2) + 
  geom_point(aes(size = 1/v, fill = data3$es_type)) + 
  coord_flip() +
  geom_vline(aes(xintercept = min(data3$studyid2) + 1), linetype = 5) +
  geom_vline(aes(xintercept = max(data3$studyid2) + 1), linetype = 1) +
  scale_size_continuous(range = c(3,6)) +
  scale_x_continuous(limits = c(0, max(data3$studyid2) + 2), expand = c(0, 0)) +
  scale_y_continuous(name = NULL, limits = c(-3,3), expand = c(0,0)) + 
  scale_shape_manual(values = c(22, 23)) + 
  annotate("text", x = max(data3$studyid2) + 1.2, y=0, 
           label='paste("Effect size (",italic("d"),") with 95% CI")', 
           parse=T, hjust = 0.5) +
  xlab(NULL) +
  ylab(NULL) +
  plotSettings
mainPart

# ystart & yend are arbitrary. [0, 1] is
# convinient for setting relative coordinates of# columns 
ystart = 0
yend = 1

startd_label <- .42  # point to start plot

studyList <- ggplot(data3, aes(x = studyid2, y = 0)) + 
  annotate("text", label = data3$studyid,
           x = data3$studyid2, y = 1, hjust = 1) +
  coord_flip() +
  scale_y_continuous(limits = c(ystart, yend), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, max(data3$studyid2) + 2), expand = c(0,0)) +
  scale_size_continuous(range = c(3,6)) +
  geom_vline(aes(xintercept = min(data3$studyid2) + 1), linetype = 5) +
  geom_vline(aes(xintercept = max(data3$studyid2) + 1), linetype = 1) +
  annotate("text", label = c("Study"), x = max(data3$studyid2) + 1.2, 
           y = 1, hjust = 1) +
  xlab(NULL) +
  ylab(NULL) +
  plotSettings + plotSettings2
studyList
        
dataList <- ggplot(data3, aes(x = studyid2, y = 0)) +
  annotate("text", label = round(data3$d, 2), x = data3$studyid2, y = startd_label, hjust = .5) +
  annotate("text", label = paste("[",round(data3$ll, 2),",",round(data3$ul, 2),"]", sep = ""),
           x = data3$studyid2, y = startd_label + .04, hjust = 0) +
  coord_flip() +
  scale_y_continuous(limits = c(.4, .6), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, max(data3$studyid2) + 2), expand = c(0,0)) +
  scale_size_continuous(range = c(3,6)) +
  geom_vline(aes(xintercept = min(data3$studyid2) + 1), linetype = 5) +
  geom_vline(aes(xintercept = max(data3$studyid2) + 1), linetype = 1) +
  annotate("text", x = max(data3$studyid2) + 1.2, y=startd_label, 
           label='paste(italic("d"))', parse=T, hjust = .5) +
  annotate("text", x = max(data3$studyid2) + 1.2, y=startd_label + .04, 
           label=c('95% CI'), hjust = 0) +
  xlab(NULL) +
  ylab(NULL) +
  plotSettings + plotSettings2
dataList


grid.arrange(ggplotGrob(studyList),
             ggplotGrob(mainPart),
             ggplotGrob(dataList),
             nrow = 1, ncol = 3, 
             widths = c(.5, .75, .5))



# 
# 
# dataList <- ggplot(data3, aes(x = studyid2, y = 0)) +
#   annotate("text", label = round(data3$d, 2), x = data3$studyid2, y = startd_label, hjust = 0) +
#   annotate("text", label = round(data3$ll, 2), x = data3$studyid2, y = startd_label + .04, hjust = 0) +
#   annotate("text", label = ",", x = data3$studyid2, y = startd_label + .07, hjust = 1) +
#   annotate("text", label = round(data3$ul, 2), x = data3$studyid2, y = startd_label + .075, hjust = 0) +
#   coord_flip() +
#   scale_y_continuous(limits = c(.4, .6), expand = c(0,0)) +
#   scale_x_continuous(limits = c(0, max(data3$studyid2) + 2), expand = c(0,0)) +
#   scale_size_continuous(range = c(3,6)) +
#   geom_vline(aes(xintercept = min(data3$studyid2) + 1), linetype = 5) +
#   geom_vline(aes(xintercept = max(data3$studyid2) + 1), linetype = 1) +
#   annotate("text", x = max(data3$studyid2) + 1.2, y=startd_label + .01, 
#            label='paste(italic("d"))', parse=T, hjust = .5) +
#   annotate("text", x = max(data3$studyid2) + 1.2, y=startd_label + .07, 
#            label=c('95% CI'), hjust = .5) +
#   xlab(NULL) +
#   ylab(NULL) +
#   plotSettings + plotSettings2
# dataList



