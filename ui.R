

library(shiny)
library(shinythemes)


shinyUI(fluidPage(
  theme=shinytheme("cerulean"),
  
  navbarPage(
    title="Meta-Analysis of Diagnostic Accuracy Studies with Several Cutpoints",
    id="nav",
    
    tabPanel("About", value="about",helpText( tags$b(h3("About ")),"Diagnostic accuracy tests may
                                          be based on an ordinal or continuous biomarker or an ordinal score together
                                          with a cutoff. The decision whether the target condition is present or not
                                          (positive or negative test result) depends on whether the observed value is
                                          above or below the cutoff. Sensitivity and specificity of the test depend on
                                          the chosen cutoff and vary with the cutoff. In meta-analysis of diagnostic
                                          accuracy studies, results are often reported for multiple cutoffs within a
                                          study, and the cutoffs may differ between studies. The multiple cutoffs
                                          model creates a link between the range of cutoffs and the respective pairs
                                          of sensitivity and specificity and thus allows identifying cutoffs at which
                                          the test is likely to perform best",a(href="https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-016-0196-1", target="_blank", "Gerta[2016]."), 
                                             "This app is based on the R package",  a(href="https://github.com/guido-s/diagmeta", target="_blank", "diagmeta v0.1-0"),".",
                                             br(),
                                             tags$b(h4("Note :")), "When using the app make sure that the variable names in the file
                                          perfectly correspond to the arguments of the function. The variable names in the file should be ", tags$strong("TP"), " for true positive, ", tags$strong("FP"), " for false positive,
                                          ", tags$strong("TN"), " for true negative, ", tags$strong("FN"), " for false negative", tags$strong("studlab"), " for study label and",tags$strong("cutoff"), " for cutoffs"
                                             
                                           )
             
    ),
    tabPanel("IPD2Diag", 
             sidebarLayout(
               sidebarPanel(
                 helpText(tags$b("Enter individual participant data (comma delimited) :")),
                 br(),
                 textInput('studlab', 'study labels', "1,2,3,4,5")
                 ,
                 textInput('value', 'individual patients measurements', "20,18.5,30,12,15")
                 ,
                 textInput('status', 'individuals status', "0,0,0,1,1")
                 ,
                 
                 
                 actionButton("submit4", "submit"),
                 br(),
                 br(),
                 helpText("In the Transformed data tab, these above IPD data is simulated and transformed to
                          data set by ", tags$code("IPD2diag"), "function, that fits into", tags$code("diagmeta"), "function."
                 ),
                 
                 
                 actionButton("submit5", "Transform"),
                 p("click on the transform button to view transformed IPD data")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("DataFrame", tableOutput("dframe")),
                   tabPanel("Transformed Data", tableOutput("dframe1"), downloadButton("downloadData", "Download"))
                   
                 ))
             )
    ),
    tabPanel("diagmeta", value = "diagmeta", 
             sidebarLayout(
               sidebarPanel(
                 a(href ="https://github.com/kolampally/data-required-for-app/blob/master/DataBrookeLevis.csv",
                   target = "_blank", "Example dataset"),
                 br(),
                 br(),
                 fileInput("file", "Choose CSV File", accept = c(
                   "read.csv", "read.table", "read.mtv",  "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv2")),
                 tags$hr(),
                 checkboxInput("header", "Header", TRUE),
                 
                 
                 selectInput("distr", 
                             "type of distribution",
                             choices = c("logistic", "normal"), TRUE),
                 selectInput("model", "select the model",
                             choices = c("Different Intercepts and Common Slopes (DICS)" = "DICS",
                                         
                                         "Common Intercept (CI)" ="CI",
                                         "Different Intercepts (DI)" = "DI",
                                         "Common Slopes (CS)" = "CS",
                                         "Different Slopes (DS)" = "DS",
                                         "Common Intercept and Common Slopes (CICS)" = "CICS",
                                         "Different Intercepts and Different Slopes (DIDS)" = "DIDS",
                                         "Common Intercept and Different Slopes (CIDS)" = "CIDS"), TRUE),
                 selectInput("method.weights", "weighting method",
                             choices = c("inverse variance (invvar)" = "invvar",  "sample size (size)" = "size",
                                         "no weighting (equal)" = "equal"), TRUE),
                 
                 checkboxInput("equalvar", "assume equal variances for both groups", FALSE),
                 
                 checkboxInput("log.cutoff", "use log transformation for diagnostic marker", FALSE),
                 sliderInput("lambda",
                             "select lambda",
                             min =0.1,
                             max = 1,
                             value = 0.5),
                 
                 sliderInput("level","level used for confidence intervals", min =0.1,max = 1,value = 0.95),
                 sliderInput("incr",
                             "select continuity correction",
                             min =0.1,
                             max = 1,
                             value = 0.5),
                 numericInput("n.iter", "number of iterations", value = 1000 ),
                 actionButton("submit", "submit"),
                 p("click on the submit button to apply the settings")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Summary",verbatimTextOutput("result")),
                   tabPanel("Detailed results",verbatimTextOutput("result1"))
              
                 )
               )
             )),
    tabPanel("plots", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("which", "select the plot", choices = c( "regression plot(reg)" = "reg",
                                                                      "Cumulative distribution plot(cdf)" = "cdf",
                                                                      "survival plot" = "survival", "Youden plot" = "Youden",
                                                                      "Receiver Operating Characteristic plot(ROC)" =  "ROC",
                                                                      "Summary receiver operating characteristic plot(SROC)" = "SROC",
                                                                      "Density plot" = "density"),
                             multiple = TRUE),              
                 checkboxInput("ci", "confidence intervals for reg, cdf, survival and youden", FALSE),
                 checkboxInput("ciSens", "confidence intervals for sensitivity, given the specificity in SROC plot", FALSE),
                 checkboxInput("ciSpec", "confidence intervals for specificity, given the sensitivity in SROC plot", FALSE),
                 checkboxInput("mark.optcut", "Marking optimal cutoff on SROC plot", FALSE),
                 checkboxInput("mark.cutpoints", "Marking cutoffs on SROC plot", FALSE),
                 checkboxInput("ellipse", "Confidence ellipse on SROC plot", FALSE),
                 checkboxInput("points", "points on ROC", FALSE),
                 checkboxInput("lines", "polygonal lines connecting points belonging to the same study of plots reg, cdf, survival and Youden", FALSE),
                 
                 checkboxInput("rlines", "regression lines or curves for plots reg, cdf, survival and Youden", FALSE),
                 
                 checkboxInput("line.optcut","vertical line at optimal cutoff for cdf, survival, youden and density", FALSE),
                 
                 radioButtons("col.points", "type of color", choices = c("rainbow", "topo","heat",
                                                                         "terrain","cm","gray" ,"black")),
                 selectInput("shading", "shading and hatching confidence regions in SROC plot", choices = c( "none","shade","hatch"),TRUE),
                 radioButtons("tplot" , "downloading plot as png or pdf", choices = c("png", "pdf")),
                 actionButton("submit1", "show"),
                 p("click on the show button to see the graphs")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("plot", 
                            plotOutput("plot"),
                            downloadButton(outputId = "down", "Download the plot"))
                   
                 )
               )
             )),
    tabPanel("values", 
             sidebarLayout(
               sidebarPanel(
                 helpText(tags$b("Extract values from a", tags$code("diagmeta"), "object :")),
                 br(),
                 sliderInput("pr",
                             "Select prevalance",
                             min =0.0,
                             max = 1,
                             value = 0.5),
                 numericInput("cut", "enter cutoff value", value = 25),
                
                 actionButton("submit2", "submit")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("values", verbatimTextOutput("value"))
                   
                 )
               )
             )),
    
    tabPanel("tabulate Sens Spec", 
             sidebarLayout(
               sidebarPanel(
                 helpText(tags$b("Tabulating sensitivity and specificity for ranges of cutoffs from a", tags$code("diagmeta"), "object :")),
                 br(),
          
                 textInput('cutoffs', 'enter cutoff values (comma delimited)', "0,1,2")
               ,
               
                 
                 actionButton("submit3", "submit")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("tabulate", verbatimTextOutput("tab"))
                   
                 ))
               )
             )

    
    )

  )
  
)
