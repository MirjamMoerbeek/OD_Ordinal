library(shinythemes)
library(plotly)
navbarPage(theme = shinytheme("cerulean"),"Optimal treatment allocation ordinal regression ",
           tabPanel("J = 3 categories",
                    sidebarLayout(
                      sidebarPanel(
                                   h4("A priori estimates of model parameters: response probabilities and effect size"),
                                   numericInput("p31", label = h5("p(Y=1)"), value = 0.167,min=0,max=1,width='50%'),
                                   numericInput("p32", label = h5("p(Y=2)"), value = 0.500,min=0,max=1,width='50%'),                    
                                   numericInput("p33", label = h5("p(Y=3)"), value = 0.333,min=0,max=1,width='50%'),
                                   numericInput("OR3", label = h5("Odds Ratio"), value = 1.68,min=0,max=100,width='50%'),
                                   h4("Specify ratio of costs in the intervention condition to costs in the control."),
                                   numericInput("ratio3", label = h5("Cost Ratio"), value = 5,min=0,max=100,width='50%'),
                                   h4("Specify parameters for statistical test"),
                                   numericInput("alpha3", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                                   selectInput("test3", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                                   submitButton("Submit")
                                  ),
                      mainPanel(h3("Response probability per category"),
                                plotOutput("plot3a", width = 400, height = 300),
                                fluidRow(
                                  column(4,
                                         h3("Efficiency graph"),
                                           plotlyOutput("plot3b", width = 400, height = 350),
                                           h5("This graph shows the efficiency for any proportion as compared to the optimal proportion."),
                                           textOutput("OD.3.b1"),
                                           textOutput("OD.3.b2")
                                          )
                                  ,
                                  column(4,
                                         
                                         h3("Robustness graph"),
                                         plotlyOutput("plot3c", width = 400, height = 350),
                                         h5("This graph shows how well the optimal design for the prior estimate of the odds ratio performs if the population value of the odds ratio is different.")
                                  ),
                                  column(4,
                                         h3("Power graph"),
                                         plotlyOutput("plot3d", width = 400, height = 350),
                                         h5("This graph shows the relation between budget and power. The cost are scaled such that the cost in the control condition are equal to 1.")
                                  )
                      ))
                    )
           ),
           
           
           tabPanel("J = 4 categories",
                    sidebarLayout(
                      sidebarPanel(
                                   h4("A priori estimates of model parameters: response probabilities and effect size"),
                                   numericInput("p41", label = h5("p(Y=1)"), value = 0.1,min=0,max=1,width='50%'),
                                   numericInput("p42", label = h5("p(Y=2)"), value = 0.3,min=0,max=1,width='50%'),                    
                                   numericInput("p43", label = h5("p(Y=3)"), value = 0.4,min=0,max=1,width='50%'),
                                   numericInput("p44", label = h5("p(Y=4)"), value = 0.2,min=0,max=1,width='50%'),
                                   numericInput("OR4", label = h5("Odds Ratio"), value = 1.68,min=0,max=100,width='50%'),
                                   h4("Specify ratio of costs in the intervention condition to costs in the control."),
                                   numericInput("ratio4", label = h5("Cost Ratio"), value = 5,min=0,max=100,width='50%'),
                                   h4("Specify parameters for statistical test"),
                                   numericInput("alpha4", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                                   selectInput("test4", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                                   submitButton("Submit")
                      ),
                      mainPanel(h3("Response probability per category"),
                                plotOutput("plot4a", width = 400, height = 300),
                                fluidRow(
                                  column(4,
                                         h3("Efficiency graph"),     
                                         plotlyOutput("plot4b", width = 400, height = 350), 
                                         h5("This graph shows the efficiency for any proportion as compared to the optimal proportion."),
                                         textOutput("OD.4.b1"),
                                         textOutput("OD.4.b2")
                                  )
                                  ,
                                  column(4,
                                         h3("Robustness graph"),
                                         plotlyOutput("plot4c", width = 400, height = 350),
                                         h5("This graph shows how well the optimal design for the prior estimate of the odds ratio performs if the population value of the odds ratio is different.")
                                  ),
                                  column(4,
                                         h3("Power graph"),
                                         plotlyOutput("plot4d", width = 400, height = 350),
                                         h5("This graph shows the relation between budget and power. The cost are scaled such that the cost in the control condition are equal to 1.")
                                  ) 
                                )
                      )
                    )
           ),

           tabPanel("J = 5 categories",
                    sidebarLayout(
                      sidebarPanel(
                                  h4("A priori estimates of model parameters: response probabilities and effect size"),
                                  numericInput("p51", label = h5("p(Y=1)"), value = 0.067,min=0,max=1,width='50%'),
                                  numericInput("p52", label = h5("p(Y=2)"), value = 0.2,min=0,max=1,width='50%'),                    
                                  numericInput("p53", label = h5("p(Y=3)"), value = 0.333,min=0,max=1,width='50%'),                   
                                  numericInput("p54", label = h5("p(Y=4)"), value = 0.267,min=0,max=1,width='50%'),
                                  numericInput("p55", label = h5("p(Y=5)"), value = 0.133,min=0,max=1,width='50%'),
                                  numericInput("OR5", label = h5("Odds Ratio"), value = 1.68,min=0,max=100,width='50%'),
                                  h4("Specify ratio of costs in the intervention condition to costs in the control."),
                                  numericInput("ratio5", label = h5("Cost Ratio"), value = 5,min=0,max=100,width='50%'),
                                  h4("Specify parameters for statistical test"),
                                  numericInput("alpha5", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                                  selectInput("test5", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                                  submitButton("Submit")
                                ),
                      mainPanel(h3("Response probability per category"),
                                plotOutput("plot5a", width = 400, height = 300),
                                fluidRow(
                                    column(4,
                                    h3("Efficiency graph")  ,
                                    plotlyOutput("plot5b", width = 400, height = 350), 
                                    h5("This graph shows the efficiency for any proportion as compared to the optimal proportion."),
                                    textOutput("OD.5.b1"),
                                    textOutput("OD.5.b2")
                                    )  
                                
                          ,
                          column(4,
                                 h3("Robustness graph"),
                                 plotlyOutput("plot5c", width = 400, height = 350),
                                 h5("This graph shows how well the optimal design for the prior estimate of the odds ratio performs if the population value of the odds ratio is different.")
                          ),
                          column(4,
                                 h3("Power graph"),
                                 plotlyOutput("plot5d", width = 400, height = 350),
                                 h5("This graph shows the relation between budget and power. The cost are scaled such that the cost in the control condition are equal to 1.")
                          ) 
                          
                                )
                      )
                    )
             ),
           
           tabPanel("J = 6 categories",
                    sidebarLayout(
                      sidebarPanel(
                                  h4("A priori estimates of model parameters: response probabilities and effect size"),
                                   numericInput("p61", label = h5("p(Y=1)"), value = 0.048,min=0,max=1,width='50%'),
                                   numericInput("p62", label = h5("p(Y=2)"), value = 0.143,min=0,max=1,width='50%'),                    
                                   numericInput("p63", label = h5("p(Y=3)"), value = 0.238,min=0,max=1,width='50%'),                   
                                   numericInput("p64", label = h5("p(Y=4)"), value = 0.286,min=0,max=1,width='50%'),
                                   numericInput("p65", label = h5("p(Y=5)"), value = 0.190,min=0,max=1,width='50%'),
                                   numericInput("p66", label = h5("p(Y=6)"), value = 0.095,min=0,max=1,width='50%'),
                                   numericInput("OR6", label = h5("Odds Ratio"), value = 1.68,min=0,max=100,width='50%'),
                                  h4("Specify ratio of costs in the intervention condition to costs in the control."),
                                  numericInput("ratio6", label = h5("Cost Ratio"), value = 5,min=0,max=100,width='50%'),
                                  h4("Specify parameters for statistical test"),
                                  numericInput("alpha6", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                                   selectInput("test6", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                                   submitButton("Submit")
                      ),
                      mainPanel(h3("Response probability per category"),
                                plotOutput("plot6a", width = 400, height = 300),
                                fluidRow(
                                  column(4,
                                         h3("Efficiency graph")  ,
                                         plotlyOutput("plot6b", width = 400, height = 350), 
                                         h5("This graph shows the efficiency for any proportion as compared to the optimal proportion."),
                                         textOutput("OD.6.b1"),
                                         textOutput("OD.6.b2")
                                  )  
                                  
                                  ,
                                  column(4,
                                         h3("Robustness graph"),
                                         plotlyOutput("plot6c", width = 400, height = 350),
                                         h5("This graph shows how well the optimal design for the prior estimate of the odds ratio performs if the population value of the odds ratio is different.")
                                  ),
                                  column(4,
                                         h3("Power graph"),
                                         plotlyOutput("plot6d", width = 400, height = 350),
                                         h5("This graph shows the relation between budget and power. The cost are scaled such that the cost in the control condition are equal to 1.")
                                  )
                                  
                                )
                      )
                    )
           ),
           
           tabPanel("J = 7 categories",
                    sidebarLayout(
                      sidebarPanel(
                        h4("A priori estimates of model parameters: response probabilities and effect size"),
                    numericInput("p71", label = h5("p(Y=1)"), value = 0.036,min=0,max=1,width='50%'),
                    numericInput("p72", label = h5("p(Y=2)"), value = 0.107,min=0,max=1,width='50%'),
                    numericInput("p73", label = h5("p(Y=3)"), value = 0.179,min=0,max=1,width='50%'),
                    numericInput("p74", label = h5("p(Y=4)"), value = 0.250,min=0,max=1,width='50%'),
                    numericInput("p75", label = h5("p(Y=5)"), value = 0.214,min=0,max=1,width='50%'),
                    numericInput("p76", label = h5("p(Y=6)"), value = 0.144,min=0,max=1,width='50%'),
                    numericInput("p77", label = h5("p(Y=7)"), value = 0.070,min=0,max=1,width='50%'),
                    numericInput("OR7", label = h5("Odds Ratio"), value = 1.67,min=0,max=100,width='50%'),
                    h4("Specify ratio of costs in the intervention condition to costs in the control."),
                    numericInput("ratio7", label = h5("Cost Ratio"), value = 5,min=0,max=100,width='50%'),
                    h4("Specify parameters for statistical test"),
                    numericInput("alpha7", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                    selectInput("test7", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                    submitButton("Submit")
                      ),
                    mainPanel(
                       h3("Response probability per category"),
                      plotOutput("plot7a", width = 400, height = 300),
                      fluidRow(
                        column(4,
                               h3("Efficiency graph") ,
                               plotlyOutput("plot7b", width = 400, height = 350), 
                               h5("This graph shows the efficiency for any proportion as compared to the optimal proportion."),
                               textOutput("OD.7.b1"),
                               textOutput("OD.7.b2")
                               )  
                        
                        ,
                        column(4,
                               h3("Robustness graph"),
                               plotlyOutput("plot7c", width = 400, height = 350),
                               h5("This graph shows how well the optimal design for the prior estimate of the odds ratio performs if the population value of the odds ratio is different.")
                        ),
                        column(4,
                               h3("Power graph"),
                               plotlyOutput("plot7d", width = 400, height = 350),
                               h5("This graph shows the relation between budget and power. The cost are scaled such that the cost in the control condition are equal to 1.")
                        )
                              ))
                    
                   
                  )
            )

)
