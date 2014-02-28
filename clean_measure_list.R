library(stringr)

text <- readLines("measure_list.txt")
text <- str_trim(text, side = "both") ## function from stringr package
meastemp <- strsplit(text, "[[:space:]][-][[:space:]]")
measures <- matrix(0, nrow = 276, ncol = 2)

for (i in 1:276) {
  for (j in 1:2) {
    measures[i, j] <- meastemp[[i]][j]  
  }
}
  
measures <- as.data.frame(measures)

colnames(measures) <- c("Measure Number", "Measure Name")
### Need to use the as.character because of the factor levels
### see: http://stackoverflow.com/questions/4931545/converting-string-to-numeric
measures[,1] <- round(as.numeric(as.character(measures[,1])), digits = 0)
head(measures)


save(measures, file = 'measures.RData')