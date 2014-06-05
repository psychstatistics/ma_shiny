

## A function for preparing the reactive dataset
## to be passed to metafor (rma.uni())
ma_prep <- function(dataset = datasetInput_1()) {
  ### Set ma_input to the reactive dataset 
  ma_input <<- dataset
  ma_input$es_type <<- "study"
  
  ### How many levels does the input dataset have?
  factor_levels <<- length(levels(ma_input$studyid))
  
  ### Reorder the factor levels for Studyid
  ma_input$studyid <<- factor(ma_input$studyid, 
                              levels(ma_input$studyid)[c(factor_levels:1)])
  
  ### sort the active dataset by studyid (numerically, reverse order)
  ma_input$ordering <<- as.numeric(ma_input$studyid)
  ma_input <<- ma_input[order(-ma_input$ordering), ]
  return(ma_input)
} 
