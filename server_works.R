require(shiny)
require(foreign)
require(metafor)
require(dplyr)
require(ggplot2)
require(gridExtra)
require(xtable)
require(BH)

## Read in the data  =======================
data1 <- read.dta(file = "cdma_datacollapsed1.dta")

## Fix a typo
tmp <- levels(data1$meastype)
tmp[2] <- "Criminal Delinquency"
levels(data1$meastype) <- tmp


load('measures.RData')
complete_data <- read.dta(file = "cdma_datacleaned.dta")


### Data Munging ============================
data1 <- subset(data1, rand == "Randomized") ## Randomized only
data1 <- subset(data1, timing == 2) ## Post-tx only

### Reorder the levels of studyid factor
studies <- c("Alexander & Parsons, 1973", "Aultman-Bettridge, 2007", 
             "Barton et al., 1985 Replication II", 
             "Barton et al., 1985 Replication III", 
             "Borduin et al., 1990", "Borduin et al., 1995", 
             "Borduin et al., 2009","Coatsworth et al., 2001", 
             "Dennis et al., 2004",  "Friedman 1989",  
             "Gordon et al., 1988", "Henggeler et al., 1986",
             "Henggeler et al., 1992", "Henggeler et al., 1997", 
             "Henggler et al., 1999", "Leschied & Cunningham, 2002",
             "Letourneau et al., 2009", "Liddle et al., 2001", 
             "Liddle et al., 2004", "Liddle et al., 2008",      
             "Nickel, Luely, et al., 2006", 
             "Nickel, Muehlbacher, et al., 2006", 
             "Ogden & Halliday-Boykins, 2004",
             "Rowland et al., 2005", "Santisteban et al., 2003", 
             "Sundell et al., 2008", "Szapocznik et al., 1989",
             "Timmons-Mitchell et al., 2006",  "Waldron et al., 2001", 
             "Overall")

data1$studyid <- factor(data1$studyid, levels = studies)

### Subset the data so that we include the correct data in the full dataset
complete_data <- subset(complete_data, rand == "Randomized") ## Randomized only
complete_data <- subset(complete_data, timing == 2) ## Post-tx only

### reduce number of variables
data1 <- data1[,c('studyid', 'txtype', 'comptype',  'meastype', 'd', 'v')]
complete_data <- complete_data[ , c("studyid", "txtype", "comptype", "measure", 
                                    "d", "v", "n1", "n2")]
complete_data$studyid <- factor(complete_data$studyid, levels = studies)
complete_data <- arrange(complete_data, studyid)
colnames(complete_data) <- c("Study ID", "Tx Type", "Comparison Type", "Measure", 
                             "d", "v", "Tx N", "Comp N")

## Source the Plotsettings ======================
source("plotsettings.R")

## Set some values for the plots ====================

### These are values for helping set the text location in the 2nd and 3rd plots

#### ystart & yend are arbitrary. [0, 1] is
#### convinient for setting relative coordinates of# columns 
ystart = 0
yend = 1
startd_label <- .42  # point to start plot 

shinyServer(function(input, output) {
  
  datasetInput_compgroup <- reactive({
    switch(input$compgroup,
           "Control" = subset(data1, comptype == "Control"),
           "TAU" = subset(data1, comptype == "TAU"),
           "Alternative" = subset(data1, comptype == "Alternative")
    )  
  })  
  
  datasetInput_1 <- reactive ({
    switch(input$outcome,
           "All" = datasetInput_compgroup() %.% group_by(studyid, comptype) %.% 
             summarise(txtype = "All", meastype = "All", 
                       d = mean(d), v = mean(v)),
           "Recidivism; getting arrested" = subset(datasetInput_compgroup(), 
                                                   meastype == "Recidivism; getting arrested"),
           "Criminal Delinquency" = subset(datasetInput_compgroup(), 
                                           meastype == "Criminal Delinquency"),
           "Substance Use" = subset(datasetInput_compgroup(), 
                                    meastype == "Substance Use"),
           "Internalizing Behaviors" = subset(datasetInput_compgroup(), 
                                              meastype == "Internalizing Behaviors"),
           "Externalizing Behaviors" = subset(datasetInput_compgroup(), 
                                              meastype == "Externalizing Behaviors"), 
           "Family Measures" = subset(datasetInput_compgroup(), 
                                      meastype == "Family Measures"),
           "School Measures" = subset(datasetInput_compgroup(), 
                                      meastype == "School Measures"),
           "Peer" = subset(datasetInput_compgroup(), meastype == "Peer"),
           "Other" = subset(datasetInput_compgroup(), meastype == "Other")
    )
  })
  
  output$mytable <- renderDataTable({
    if (input$whichData == 'analysis_data') {
      dataset <- datasetInput_1()
      dataset <- dataset[, c('studyid', 'txtype', 'comptype', 'meastype', 'd', 'v')]
      dataset
    }
    else {
      dataset <- complete_data
      dataset
    }
  }, options = list(aLengthMenu = c(5, 10, 15, iDisplayLength = 20))
  )
  
  output$ma_summary <- renderPrint({
    ma_res <- rma.uni(yi = d, vi = v, data = datasetInput_1(), method = "REML", slab = studyid)
    summary(ma_res)
  })
  
  ### Note. I use the <<- assignment operator to make sure ggplot can find the data. It won't work
  ### without them in this function. I don't understand why.
  ### see http://stackoverflow.com/questions/17468082/shiny-app-ggplot-cant-find-data
  
  output$ggforest <- renderPlot({
    ### Set data2 to the reactive dataset 
    data2 <<- datasetInput_1()
    data2$es_type <<- "study"
    
    ### Reorder the factor levels for Studyid
    data2$studyid <<- factor(data2$studyid, levels(data2$studyid)[c(30:1)])
    
    ### sort the active dataset by studyid (numerically, reverse order)
    data2$ordering <<- as.numeric(data2$studyid)
    data2 <<- data2[order(-data2$ordering), ]
    
    ## Perform the meta-analysis ========================
    ma_res <<- rma.uni(yi = d, vi = v, data = data2, method = "REML", slab = studyid)
    
    
    ### Create data representing the overall results. These will
    ### be used in the forest plot
    ### The empty space is used to make some of the plotting prep easier
    overall_data <<- as.data.frame(rbind(t(c(NA, NA, NA, NA, NA, NA, NA, NA)),
                                         t(c(1, NA, NA, NA, ma_res$b[1,1], (ma_res$se)^2, 1, 1))))
    
    colnames(overall_data) <<- colnames(data2)
    overall_data$studyid[2] <<- "Overall" ### for the name in the plot (left column)
    overall_data$es_type[2] <<- "Overall" ### for the symbol type in the plot
    overall_data$ordering <<- c(0, -1)
    
    
    ### combine the data2 and overall_data
    ### create a new variable studyid which is just sequence of numbers
    ### studyid2 is for use in the plots
    data3 <<- rbind(data2, overall_data)
    data3$studyid2 <<- rev(seq(1:length(data3$studyid)))
    data3 <<- subset(data3, ordering != 0) ### drop the empty row
    
    ### create 95% confidence limits
    data3$ll <<- data3$d - (1.96 * sqrt(data3$v))
    data3$ul <<- data3$d + (1.96 * sqrt(data3$v))
    
    
    ## Plots =============================
    
    ### Main Part of Forest Plot ========================
    
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
      annotate("text", x = max(data3$studyid2) + 1.3, y=0, 
               label='paste("Effect size (",italic("d"),") with 95% CI")', 
               parse=T, hjust = 0.5, size = 6) +
      xlab(NULL) +
      ylab(NULL) +
      plotSettings
    
    ### Study listing ================================
    
    studyList <- ggplot(data3, aes(x = studyid2, y = 0)) + 
      annotate("text", label = data3$studyid,
               x = data3$studyid2, y = 1, hjust = 1) +
      coord_flip() +
      scale_y_continuous(limits = c(ystart, yend), expand = c(0,0)) +
      scale_x_continuous(limits = c(0, max(data3$studyid2) + 2), expand = c(0,0)) +
      scale_size_continuous(range = c(3,6)) +
      geom_vline(aes(xintercept = min(data3$studyid2) + 1), linetype = 5) +
      geom_vline(aes(xintercept = max(data3$studyid2) + 1), linetype = 1) +
      annotate("text", label = c("Study"), x = max(data3$studyid2) + 1.3, 
               y = 1, hjust = 1, size = 6) +
      xlab(NULL) +
      ylab(NULL) +
      plotSettings + plotSettings2
    
    ### Data List ====================================
    
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
      annotate("text", x = max(data3$studyid2) + 1.3, y=startd_label, 
               label='paste(italic("d"))', parse=T, hjust = .5, size = 6) +
      annotate("text", x = max(data3$studyid2) + 1.3, y=startd_label + .04, 
               label=c('95% CI'), hjust = 0, size = 6) +
      xlab(NULL) +
      ylab(NULL) +
      plotSettings + plotSettings2
    
    ### Bring the plots together ========================
    
    print(grid.arrange(ggplotGrob(studyList),
                       ggplotGrob(mainPart),
                       ggplotGrob(dataList),
                       nrow = 1, ncol = 3, 
                       widths = c(.5, .75, .5)))
  })
  
  output$results_table <- renderTable({
    ma_res <- rma.uni(yi = d, vi = v, data = datasetInput_1(), method = "REML", slab = studyid)
    mat <- matrix(c(round(ma_res$b[1,1],2), round(ma_res$tau2, 2), round(ma_res$I2, 2), ma_res$method, 
                    round(ma_res$fit.stats$REML[3], 2), round(ma_res$fit.stats$REML[4], 2)),
                  nrow = 6, ncol = 1)
    rownames(mat) <- c('Mean ES', 'Between Studies Variance', 'I-squared', 'Estimator', 'AIC', 'BIC')
    colnames(mat) <- 'Parameter'
    xtable(mat)
  })
  
  output$measure_table <- renderTable ({
    xtable(measures)
  }, include.rownames=FALSE, digits = 0)
  
})