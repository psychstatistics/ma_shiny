# Plot settings for the ggplot forest plot ------------------------

### TODO: Figure out for sure what each of these things do. It works
### for now but could use some understanding

plotSettings <- theme(legend.position = "none", 
                      axis.line.y = element_blank(), 
                      axis.ticks.y = element_blank(), 
                      plot.margin = unit(c(0, 0, 0, 0), "npc"), 
                      panel.margin = unit(c(-10, -10, -10, -10), "npc"), 
                      rect = element_blank(), 
                      axis.line = element_line(colour = "black", size = 0.3, linetype = 1), 
                      axis.title.x = element_blank(), 
                      axis.title.y = element_blank(), 
                      panel.grid.major = element_line(color = "red"), 
                      axis.text.y = element_blank(),
                      text = element_text(size = 20, colour = "black"))

# you can comment the next string for debug
plotSettings <- plotSettings + theme(panel.grid = element_blank(), 
                                     axis.text.y = element_blank(), 
                                     axis.ticks.y = element_blank(), 
                                     panel.grid.major = element_blank())

plotSettings2 <- theme(axis.ticks.x = element_blank(), 
                       axis.line.x = element_blank(), 
                       axis.text.x = element_text(color = "white"), 
                       axis.title.x = element_text(color = "white"))