library(shinythemes)
library(plotly)
navbarPage(theme = shinytheme("cerulean"),"Optimal treatment allocation ordinal regression ",
           tabPanel("J = 3 categories",
                    sidebarLayout(
                      sidebarPanel(h3("Input"),
                                 #  h5("Give probability for each category in the control condition"),
                                   numericInput("p31", label = h5("p(Y=1)"), value = 0.167,min=0,max=1,width='50%'),
                                   numericInput("p32", label = h5("p(Y=2)"), value = 0.500,min=0,max=1,width='50%'),                    
                                   numericInput("p33", label = h5("p(Y=3)"), value = 0.333,min=0,max=1,width='50%'),
                                   numericInput("OR3", label = h5("Odds Ratio"), value = 1.68,min=0,max=100,width='50%'),
                                   numericInput("ratio3", label = h5("Cost Ratio"), value = 2,min=0,max=100,width='50%'),
                                   numericInput("alpha3", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                                   numericInput("power3", label = h5("Requested power"), value = 0.8,min=0,max=1,width='50%'),
                                   selectInput("test3", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                                   submitButton("Submit")
                                  ),
                      mainPanel(h3("Response probability per category"),
                                plotOutput("plot3a", width = 400, height = 300),
                                fluidRow(
                                    column(5,
                                           h3("Results for D-optimality"),     
                                          plotlyOutput("plot3b1", width = 400, height = 350), 
                                          br(),br(),
                                          textOutput("OD.3.D1"),
                                          textOutput("OD.3.D2")
                                          )
                                  ,
                                  column(4,
                                         h3("Results for c-optimality"),
                                          plotlyOutput("plot3b2", width = 400, height = 350),
                                         br(),br(),
                                          textOutput("OD.3.c1"),
                                          textOutput("OD.3.c2"),
                                          textOutput("OD.3.c3")
                                         )
                                        )
                      )
                    )
           ),
           
           
           tabPanel("J = 4 categories",
                    sidebarLayout(
                      sidebarPanel(h3("Input"),
                                   numericInput("p41", label = h5("p(Y=1)"), value = 0.1,min=0,max=1,width='50%'),
                                   numericInput("p42", label = h5("p(Y=2)"), value = 0.3,min=0,max=1,width='50%'),                    
                                   numericInput("p43", label = h5("p(Y=3)"), value = 0.4,min=0,max=1,width='50%'),
                                   numericInput("p44", label = h5("p(Y=4)"), value = 0.2,min=0,max=1,width='50%'),
                                   numericInput("OR4", label = h5("Odds Ratio"), value = 1.68,min=0,max=100,width='50%'),
                                   numericInput("ratio4", label = h5("Cost Ratio"), value = 2,min=0,max=100,width='50%'),
                                   numericInput("alpha4", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                                   numericInput("power4", label = h5("Requested power"), value = 0.8,min=0,max=1,width='50%'),
                                   selectInput("test4", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                                   submitButton("Submit")
                      ),
                      mainPanel(h3("Response probability per category"),
                                plotOutput("plot4a", width = 400, height = 300),
                                fluidRow(
                                  column(5,
                                         h3("Results for D-optimality"),     
                                         plotlyOutput("plot4b1", width = 400, height = 350), 
                                         br(),br(),     
                                         textOutput("OD.4.D1"),
                                         textOutput("OD.4.D2")
                                  )
                                  ,
                                  column(4,
                                         h3("Results for c-optimality"),
                                         plotlyOutput("plot4b2", width = 400, height = 350), 
                                         br(),br(),
                                         textOutput("OD.4.c1"),
                                         textOutput("OD.4.c2"),
                                         textOutput("OD.4.c3")
                                  )
                                )
                      )
                    )
           ),

           tabPanel("J = 5 categories",
                    sidebarLayout(
                      sidebarPanel(h3("Probabilities for control"),
                                  numericInput("p51", label = h5("p(Y=1)"), value = 0.067,min=0,max=1,width='50%'),
                                  numericInput("p52", label = h5("p(Y=2)"), value = 0.2,min=0,max=1,width='50%'),                    
                                  numericInput("p53", label = h5("p(Y=3)"), value = 0.333,min=0,max=1,width='50%'),                   
                                  numericInput("p54", label = h5("p(Y=4)"), value = 0.267,min=0,max=1,width='50%'),
                                  numericInput("p55", label = h5("p(Y=5)"), value = 0.133,min=0,max=1,width='50%'),
                                  numericInput("OR5", label = h5("Odds Ratio"), value = 1.68,min=0,max=100,width='50%'),
                                  numericInput("ratio5", label = h5("Cost Ratio"), value = 2,min=0,max=100,width='50%'),
                                  numericInput("alpha5", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                                  numericInput("power5", label = h5("Requested power"), value = 0.8,min=0,max=1,width='50%'),
                                  selectInput("test5", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                                  submitButton("Submit")
                                ),
                      mainPanel(h3("Response probability per category"),
                                plotOutput("plot5a", width = 400, height = 300),
                                fluidRow(
                                    column(5,
                                    h3("Results for D-optimality")  ,
                                    plotlyOutput("plot5b1", width = 400, height = 350), 
                                    br(),br(),
                                    textOutput("OD.5.D1"),
                                    textOutput("OD.5.D2")
                                    )  
                                
                          ,
                                  column(4,
                                  h3("Results for c-optimality"),
                                  plotlyOutput("plot5b2", width = 400, height = 350), 
                                  br(),br(),
                                  textOutput("OD.5.c1"),
                                  textOutput("OD.5.c2"),
                                  textOutput("OD.5.c3")
                                  )
                          
                                )
                      )
                    )
             ),
           
           tabPanel("J = 6 categories",
                    sidebarLayout(
                      sidebarPanel(h3("Probabilities for control"),
                                   numericInput("p61", label = h5("p(Y=1)"), value = 0.048,min=0,max=1,width='50%'),
                                   numericInput("p62", label = h5("p(Y=2)"), value = 0.143,min=0,max=1,width='50%'),                    
                                   numericInput("p63", label = h5("p(Y=3)"), value = 0.238,min=0,max=1,width='50%'),                   
                                   numericInput("p64", label = h5("p(Y=4)"), value = 0.286,min=0,max=1,width='50%'),
                                   numericInput("p65", label = h5("p(Y=5)"), value = 0.190,min=0,max=1,width='50%'),
                                   numericInput("p66", label = h5("p(Y=6)"), value = 0.095,min=0,max=1,width='50%'),
                                   numericInput("OR6", label = h5("Odds Ratio"), value = 1.68,min=0,max=100,width='50%'),
                                   numericInput("ratio6", label = h5("Cost Ratio"), value = 2,min=0,max=100,width='50%'),
                                   numericInput("alpha6", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                                   numericInput("power6", label = h5("Requested power"), value = 0.8,min=0,max=1,width='50%'),
                                   selectInput("test6", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                                   submitButton("Submit")
                      ),
                      mainPanel(h3("Response probability per category"),
                                plotOutput("plot6a", width = 400, height = 300),
                                fluidRow(
                                  column(5,
                                         h3("Results for D-optimality")  ,
                                         plotlyOutput("plot6b1", width = 400, height = 350), 
                                         br(),br(),
                                         textOutput("OD.6.D1"),
                                         textOutput("OD.6.D2")
                                  )  
                                  
                                  ,
                                  column(4,
                                         h3("Results for c-optimality"),
                                         plotlyOutput("plot6b2", width = 400, height = 350), 
                                         br(),br(),
                                         textOutput("OD.6.c1"),
                                         textOutput("OD.6.c2"),
                                         textOutput("OD.6.c3")
                                  )
                                  
                                )
                      )
                    )
           ),
           
           tabPanel("J = 7 categories",
                    sidebarLayout(
                      sidebarPanel(h3("Probabilities for control"),
                    numericInput("p71", label = h5("p(Y=1)"), value = 0.036,min=0,max=1,width='50%'),
                    numericInput("p72", label = h5("p(Y=2)"), value = 0.107,min=0,max=1,width='50%'),
                    numericInput("p73", label = h5("p(Y=3)"), value = 0.179,min=0,max=1,width='50%'),
                    numericInput("p74", label = h5("p(Y=4)"), value = 0.250,min=0,max=1,width='50%'),
                    numericInput("p75", label = h5("p(Y=5)"), value = 0.214,min=0,max=1,width='50%'),
                    numericInput("p76", label = h5("p(Y=6)"), value = 0.144,min=0,max=1,width='50%'),
                    numericInput("p77", label = h5("p(Y=7)"), value = 0.070,min=0,max=1,width='50%'),
                    numericInput("OR7", label = h5("Odds Ratio"), value = 1.67,min=0,max=100,width='50%'),
                    numericInput("ratio7", label = h5("Cost Ratio"), value = 2,min=0,max=100,width='50%'),
                    numericInput("alpha7", label = h5("Type I error rate"), value = 0.05,min=0,max=1,width='50%'),
                    numericInput("power7", label = h5("Requested power"), value = 0.8,min=0,max=1,width='50%'),
                    selectInput("test7", label = "Type of test", choices = list("One-sided" = 1, "Two-sided" = 2), selected = 1,width='50%'),
                    submitButton("Submit")
                      ),
                    mainPanel(
                       h3("Response probability per category"),
                      plotOutput("plot7a", width = 400, height = 300),
                      fluidRow(
                        column(5,
                               h3("Results for D-optimality") ,
                               plotlyOutput("plot7b1", width = 400, height = 350), 
                               br(),br(),
                               textOutput("OD.7.D1"),
                               textOutput("OD.7.D2")
                               )  
                        
                        ,
                        column(4,
                               h3("Results for c-optimality"),
                               plotlyOutput("plot7b2", width = 400, height = 350), 
                               br(),br(),
                               textOutput("OD.7.c1"),
                               textOutput("OD.7.c2"),
                               textOutput("OD.7.c3")
                               )
                              ))
                    
                   
                  )
            )

)