
# A function for computing the random effects meta-analysis
# and placing the results within a dataset that has the same
# columns as the reactive dataset

an_results <- function(dataset, es, var_es, studyid) {
  ## Perform the meta-analysis
  ma_res <<- rma.uni(yi = es, vi = var_es, 
                     data = dataset, method = "REML", 
                     slab = studyid)
  ### Create data representing the overall results. These will
  ### be used in the forest plot after we attach to the study data
  ### The missing values are used to make some of the plotting prep easier
  overall_data <<- as.data.frame(rbind(t(c(NA, NA, NA, NA, NA, NA, NA, NA)),
                                      t(c(1, NA, NA, NA, ma_res$b[1,1], (ma_res$se)^2, 1, 1)),
                                      t(c(1, NA, NA, NA, ma_res$b[1,1], (ma_res$se)^2, 1, 1))))
  colnames(overall_data) <<- colnames(dataset) ### name the columns
  overall_data$studyid[2] <<- "Overall" ### for the name in the plot (left column)
  overall_data$es_type[2] <<- "Overall" ### for the symbol type in the plot
  overall_data$studyid[3] <<- "Prediction Interval" ### for the name in the plot (left column)
  overall_data$es_type[3] <<- "Prediction Interval" ### for the symbol type in the plot
  overall_data$ordering <<- c(0, -1, -2)
  return(overall_data)
}


