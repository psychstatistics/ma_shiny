
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Psychotherapy Meta-Analysis"),
  
  ## in the list, the first is the name to appear on the app and after the = is the value in the data
  
  sidebarPanel(
    selectInput("outcome", "Outcome Measure:",
                list("All" = "All",
                     "Recidivism; getting arrested" = "Recidivism; getting arrested",
                     "Criminal Deliquency" = "Criminal Deliquency",    ### Need to change the variable b/c spelled wrong
                     "Substance Use" = "Substance Use",
                     "Internalizing Behaviors" = "Internalizing Behaviors",
                     "Externalizing Behaviors" = "Externalizing Behaviors", 
                     "Family Measures" = "Family Measures",
                     "School Measures" = "School Measures",
                     "Peer" = "Peer",
                     "Other" = "Other")),
    selectInput("compgroup", "Comparison Group:",
                list("Control" = "Control",
                     "TAU" = "TAU",
                     "Alternative" = "Alternative")),
    helpText("Select an outcome measure category as well as a comparison group category. The default for the Outcome Measure is 'All', which aggregates across all measures within a study. The default for Comparison Type is 'Control'. Thus, the initial results you see on the right are the meta-analysis results for all outcome measures comparing a family therapy to control.")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Meta-Analysis", 
        h4("Forest Plot"),
        plotOutput("ggforest"),
        helpText(""),
        helpText("Forest plot presenting data from each study as well as the overall, random effects aggregate estimate."),
        helpText(""),
        h4("Key Statistics"),
        tableOutput("results_table"),
        helpText(""),
        helpText("Key parameter estimates from a random effects meta-analysis using REML estimation. All analyses were performed using the metafor package available for R. Additional information from the analysis is available on the 'Metafor Output' tab.")),
      tabPanel("Metafor Output", verbatimTextOutput("ma_summary")),
      tabPanel("Data", dataTableOutput('mytable'))  
      )
    )

))
  
  
#, plotOutput("forest")