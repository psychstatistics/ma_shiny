

# A function for combining study data and the analysis results from metafor.
# It also does some prep for the plotting.

combine_data <- function(studydata = data2, resultsdata = overall_data) {
  ### combine the studydata (data2) and resultsdata (overall_data)
  ### create a new variable studyid which is just sequence of numbers
  ### studyid2 is for use in the plots
  data3 <<- rbind(studydata, resultsdata)
  data3$studyid2 <<- rev(seq(1:length(data3$studyid)))
  data3 <<- subset(data3, ordering != 0) ### drop the empty row  
  ### create 95% confidence limits and Prediction interval
  data3$ll <- ifelse(data3$studyid == "Prediction Interval", predict(ma_res)$cr.lb,  data3$d - (1.96 * sqrt(data3$v)))
  data3$ul <- ifelse(data3$studyid == "Prediction Interval", predict(ma_res)$cr.ub,  data3$d + (1.96 * sqrt(data3$v)))  
  return(data3)
}




