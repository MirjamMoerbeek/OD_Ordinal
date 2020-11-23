library(shiny)
source("functions.R")

function(input, output, session) {

######################################################################################################################
######################################################################################################################
######################################################################################################################
### 3 categories
######################################################################################################################
######################################################################################################################
######################################################################################################################
  
################################################################################################################################
### barplots probabilities 
################################################################################################################################  
  output$plot3a <- renderPlot({
    
    validate(
      need(input$p31+input$p32+input$p33==1, 'Input error: probabilities must sum to 1'),
      need(input$p31>0&input$p31<1, 'Input error: p1 not in interval [0,1]'),
      need(input$p32>0&input$p32<1, 'Input error: p2 not in interval [0,1]'),
      need(input$p33>0&input$p33<1, 'Input error: p3 not in interval [0,1]')
    )
    
    p.c=c(input$p31,input$p32,input$p33)
    beta=log(input$OR3)
    cp.c=cumsum(p.c)
    cp.c=cp.c[cp.c<1]
    logit.cp.c=log(cp.c/(1-cp.c))
    logit.cp.i=logit.cp.c+beta
    cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
    p.i=rep(0,3)				### response probabilities intervention
    p.i[1]=cp.i[1]
    p.i[2]=cp.i[2]-cp.i[1]
    p.i[3]=1-cp.i[2]
    
    control=p.c
    intervention=p.i
    barplot(cbind(control,intervention),beside=TRUE,ylim=c(0,1),ylab="probability",legend=c(1,2,3))
      })
  
################################################################################################################################
### plot RE for D-optimality
################################################################################################################################  
  output$plot3b1 <- renderPlotly({
    output=f.OD3(input$p31,input$p32,input$p33,input$OR3,input$ratio3)
    output=as.data.frame(output)
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- row.names(output)
  
          plot_ly(output, x = ~output[,1], y = ~output[,2], key = ~key,mode='lines',type="scatter") %>%
          layout(dragmode = "select",
             xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
             yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
  })
  
  
################################################################################################################################
### plot RE for c-optimality
################################################################################################################################  
  output$plot3b2 <- renderPlotly({
    output=f.OD3(input$p31,input$p32,input$p33,input$OR3,input$ratio3)
    output=as.data.frame(output)
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- row.names(output)
    

    plot_ly(output, x = ~output[,1], y = ~output[,3], key = ~key,mode='lines',type="scatter") %>%
      layout(dragmode = "select",
             xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
             yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
  })
  
   
################################################################################################################################
### text output optimal design for D-optimality
################################################################################################################################  
   output$OD.3.D1 <- renderText({ 
    output=f.OD3(input$p31,input$p32,input$p33,input$OR3,input$ratio3)
    opt.prop=which(output[,2]==1)
        paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
  })
  
################################################################################################################################
### text output RE balanced design for c-optimality
################################################################################################################################  
   output$OD.3.D2 <- renderText({ 
     output=f.OD3(input$p31,input$p32,input$p33,input$OR3,input$ratio3)
     paste("Relative efficiency of balanced design: ", round(output[50,2],2))
   })

################################################################################################################################
### text output optimal design for c-optimality
################################################################################################################################  
   output$OD.3.c1 <- renderText({ 
     output=f.OD3(input$p31,input$p32,input$p33,input$OR3,input$ratio3)
     opt.prop=which(output[,3]==1)
     paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
   })
   
################################################################################################################################
### text output RE balanced design c-optimality
################################################################################################################################  
   output$OD.3.c2 <- renderText({ 
     output=f.OD3(input$p31,input$p32,input$p33,input$OR3,input$ratio3)
     paste("Relative efficiency of balanced design: ", round(output[50,3],2))
   })
   
   
   
######################################################################################################################
######################################################################################################################
######################################################################################################################
### 4 categories
######################################################################################################################
######################################################################################################################
######################################################################################################################
   
################################################################################################################################
### barplots probabilities 
################################################################################################################################  
   
   output$plot4a <- renderPlot({
     
     validate(
       need(input$p41+input$p42+input$p43+input$p44==1, 'Input error: probabilities must sum to 1'),
       need(input$p41>0&input$p41<1, 'Input error: p1 not in interval [0,1]'),
       need(input$p42>0&input$p42<1, 'Input error: p2 not in interval [0,1]'),
       need(input$p43>0&input$p43<1, 'Input error: p3 not in interval [0,1]'),
       need(input$p44>0&input$p44<1, 'Input error: p4 not in interval [0,1]')
     )
     
     p.c=c(input$p41,input$p42,input$p43,input$p44)
     beta=log(input$OR4)
     cp.c=cumsum(p.c)
     cp.c=cp.c[cp.c<1]
     logit.cp.c=log(cp.c/(1-cp.c))
     logit.cp.i=logit.cp.c+beta
     cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
     p.i=rep(0,3)				### response probabilities intervention
     p.i[1]=cp.i[1]
     p.i[2]=cp.i[2]-cp.i[1]
     p.i[3]=cp.i[3]-cp.i[2]
     p.i[4]=1-cp.i[3]
     control=p.c
     intervention=p.i
     barplot(cbind(control,intervention),beside=TRUE,ylim=c(0,1),ylab="probability",legend=c(1,2,3,4))
   })
   
################################################################################################################################
### plot RE for D-optimality
################################################################################################################################  
   output$plot4b1 <- renderPlotly({
     output=f.OD4(input$p41,input$p42,input$p43,input$p44,input$OR4,input$ratio4)

     output=as.data.frame(output)
     # use the key aesthetic/argument to help uniquely identify selected observations
     key <- row.names(output)
     
     plot_ly(output, x = ~output[,1], y = ~output[,2], key = ~key,mode='lines',type="scatter") %>%
       layout(dragmode = "select",
              xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
              yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
     
   })
   
################################################################################################################################
### plot RE for c-optimality
################################################################################################################################  
   output$plot4b2 <- renderPlotly({
     output=f.OD4(input$p41,input$p42,input$p43,input$p44,input$OR4,input$ratio4)
     
     output=as.data.frame(output)
     # use the key aesthetic/argument to help uniquely identify selected observations
     key <- row.names(output)
     
     plot_ly(output, x = ~output[,1], y = ~output[,3], key = ~key,mode='lines',type="scatter") %>%
       layout(dragmode = "select",
              xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
              yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
     
   })
   
################################################################################################################################
### text output optimal design for D-optimality
################################################################################################################################  
   output$OD.4.D1 <- renderText({ 
     output=f.OD4(input$p41,input$p42,input$p43,input$p44,input$OR4,input$ratio4)
     opt.prop=which(output[,2]==1)
     paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
   })
   
################################################################################################################################
### text output RE balanced design for c-optimality
################################################################################################################################  
   output$OD.4.D2 <- renderText({ 
     output=f.OD4(input$p41,input$p42,input$p43,input$p44,input$OR4,input$ratio4)
     paste("Relative efficiency of balanced design: ", round(output[50,2],2))
   })
   
################################################################################################################################
### text output optimal design for c-optimality
################################################################################################################################  
   output$OD.4.c1 <- renderText({ 
     output=f.OD4(input$p41,input$p42,input$p43,input$p44,input$OR4,input$ratio4)
     opt.prop=which(output[,3]==1)
     paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
   })
   
################################################################################################################################
### text output RE balanced design c-optimality
################################################################################################################################  
   output$OD.4.c2 <- renderText({ 
     output=f.OD4(input$p41,input$p42,input$p43,input$p44,input$OR4,input$ratio4)
     paste("Relative efficiency of balanced design: ", round(output[50,3],2))
   })
   

   
######################################################################################################################
######################################################################################################################
######################################################################################################################
### 5 categories
######################################################################################################################
######################################################################################################################
######################################################################################################################

################################################################################################################################
### barplots probabilities 
################################################################################################################################  
   
   output$plot5a <- renderPlot({
     
     validate(
       need(input$p51+input$p52+input$p53+input$p54+input$p55==1, 'Input error: probabilities must sum to 1'),
       need(input$p51>0&input$p51<1, 'Input error: p1 not in interval [0,1]'),
       need(input$p52>0&input$p52<1, 'Input error: p2 not in interval [0,1]'),
       need(input$p53>0&input$p53<1, 'Input error: p3 not in interval [0,1]'),
       need(input$p54>0&input$p54<1, 'Input error: p4 not in interval [0,1]'),
       need(input$p55>0&input$p55<1, 'Input error: p5 not in interval [0,1]')
     )
     
     p.c=c(input$p51,input$p52,input$p53,input$p54,input$p55)
     beta=log(input$OR5)
     cp.c=cumsum(p.c)
     cp.c=cp.c[cp.c<1]
     logit.cp.c=log(cp.c/(1-cp.c))
     logit.cp.i=logit.cp.c+beta
     cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
     p.i=rep(0,3)				### response probabilities intervention
     p.i[1]=cp.i[1]
     p.i[2]=cp.i[2]-cp.i[1]
     p.i[3]=cp.i[3]-cp.i[2]
     p.i[4]=cp.i[4]-cp.i[3]
     p.i[5]=1-cp.i[4]
     control=p.c
     intervention=p.i
     barplot(cbind(control,intervention),beside=TRUE,ylim=c(0,1),ylab="probability",legend=c(1,2,3,4,5))
   })
   
################################################################################################################################
### plot RE for D-optimality
################################################################################################################################  
   output$plot5b1 <- renderPlotly({
     output=f.OD5(input$p51,input$p52,input$p53,input$p54,input$p55,input$OR5,input$ratio5)

     output=as.data.frame(output)
     # use the key aesthetic/argument to help uniquely identify selected observations
     key <- row.names(output)
     
     plot_ly(output, x = ~output[,1], y = ~output[,2], key = ~key,mode='lines',type="scatter") %>%
       layout(dragmode = "select",
              xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
              yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
     
   })
   
################################################################################################################################
### plot RE for c-optimality
################################################################################################################################  
   output$plot5b2 <- renderPlotly({
     output=f.OD5(input$p51,input$p52,input$p53,input$p54,input$p55,input$OR5,input$ratio5)

     output=as.data.frame(output)
     # use the key aesthetic/argument to help uniquely identify selected observations
     key <- row.names(output)
     
     plot_ly(output, x = ~output[,1], y = ~output[,3], key = ~key,mode='lines',type="scatter") %>%
       layout(dragmode = "select",
              xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
              yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
     
   })

################################################################################################################################
### text output optimal design for D-optimality
################################################################################################################################  
   output$OD.5.D1 <- renderText({ 
     output=f.OD5(input$p51,input$p52,input$p53,input$p54,input$p55,input$OR5,input$ratio5)
     opt.prop=which(output[,2]==1)
     paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
   })
   
################################################################################################################################
### text output RE balanced design for c-optimality
################################################################################################################################  
   output$OD.5.D2 <- renderText({ 
     output=f.OD5(input$p51,input$p52,input$p53,input$p54,input$p55,input$OR5,input$ratio5)
     paste("Relative efficiency of balanced design: ", round(output[50,2],2))
   })
   
################################################################################################################################
### text output optimal design for c-optimality
################################################################################################################################  
   output$OD.5.c1 <- renderText({ 
     output=f.OD5(input$p51,input$p52,input$p53,input$p54,input$p55,input$OR5,input$ratio5)
     opt.prop=which(output[,3]==1)
     paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
   })
   
################################################################################################################################
### text output RE balanced design c-optimality
################################################################################################################################  
   output$OD.5.c2 <- renderText({ 
     output=f.OD5(input$p51,input$p52,input$p53,input$p54,input$p55,input$OR5,input$ratio5)
     paste("Relative efficiency of balanced design: ", round(output[50,3],2))
   })
   
   
   
######################################################################################################################
######################################################################################################################
######################################################################################################################
### 6 categories
######################################################################################################################
######################################################################################################################
######################################################################################################################
   
################################################################################################################################
### barplots probabilities 
################################################################################################################################  
   
   output$plot6a <- renderPlot({

     validate(
       need(input$p61+input$p62+input$p63+input$p64+input$p65+input$p66==1, 'Input error: probabilities must sum to 1'),
       need(input$p61>0&input$p61<1, 'Input error: p1 not in interval [0,1]'),
       need(input$p62>0&input$p62<1, 'Input error: p2 not in interval [0,1]'),
       need(input$p63>0&input$p63<1, 'Input error: p3 not in interval [0,1]'),
       need(input$p64>0&input$p64<1, 'Input error: p4 not in interval [0,1]'),
       need(input$p65>0&input$p65<1, 'Input error: p5 not in interval [0,1]'),
       need(input$p66>0&input$p66<1, 'Input error: p6 not in interval [0,1]')
     )
     
     p.c=c(input$p61,input$p62,input$p63,input$p64,input$p65,input$p66)
     beta=log(input$OR6)
     cp.c=cumsum(p.c)
     cp.c=cp.c[cp.c<1]
     logit.cp.c=log(cp.c/(1-cp.c))
     logit.cp.i=logit.cp.c+beta
     cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
     p.i=rep(0,3)				### response probabilities intervention
     p.i[1]=cp.i[1]
     p.i[2]=cp.i[2]-cp.i[1]
     p.i[3]=cp.i[3]-cp.i[2]
     p.i[4]=cp.i[4]-cp.i[3]
     p.i[5]=cp.i[5]-cp.i[4]
     p.i[6]=1-cp.i[5]
     control=p.c
     intervention=p.i
     barplot(cbind(control,intervention),beside=TRUE,ylim=c(0,1),ylab="probability",legend=c(1,2,3,4,5,6))
   })
   
################################################################################################################################
### plot RE for D-optimality
################################################################################################################################  
   output$plot6b1 <- renderPlotly({
     output=f.OD6(input$p61,input$p62,input$p63,input$p64,input$p65,input$p66,input$OR6,input$ratio6)
     output=as.data.frame(output)
     # use the key aesthetic/argument to help uniquely identify selected observations
     key <- row.names(output)
     
     plot_ly(output, x = ~output[,1], y = ~output[,2], key = ~key,mode='lines',type="scatter") %>%
       layout(dragmode = "select",
              xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
              yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
     
   })
   
################################################################################################################################
### plot RE for c-optimality
################################################################################################################################  
   output$plot6b2 <- renderPlotly({
     output=f.OD6(input$p61,input$p62,input$p63,input$p64,input$p65,input$p66,input$OR6,input$ratio6)

     output=as.data.frame(output)
     # use the key aesthetic/argument to help uniquely identify selected observations
     key <- row.names(output)
     
     plot_ly(output, x = ~output[,1], y = ~output[,3], key = ~key,mode='lines',type="scatter") %>%
       layout(dragmode = "select",
              xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
              yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
     
   })
   
################################################################################################################################
### text output optimal design for D-optimality
################################################################################################################################  
   output$OD.6.D1 <- renderText({ 
     output=f.OD6(input$p61,input$p62,input$p63,input$p64,input$p65,input$p66,input$OR6,input$ratio6)
     opt.prop=which(output[,2]==1)
     paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
   })
   
################################################################################################################################
### text output RE balanced design for c-optimality
################################################################################################################################  
   output$OD.6.D2 <- renderText({ 
     output=f.OD6(input$p61,input$p62,input$p63,input$p64,input$p65,input$p66,input$OR6,input$ratio6)
     paste("Relative efficiency of balanced design: ", round(output[50,2],2))
   })
   
################################################################################################################################
### text output optimal design for c-optimality
################################################################################################################################  
   output$OD.6.c1 <- renderText({ 
     output=f.OD6(input$p61,input$p62,input$p63,input$p64,input$p65,input$p66,input$OR6,input$ratio6)
     opt.prop=which(output[,3]==1)
     paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
   })
   
################################################################################################################################
### text output RE balanced design c-optimality
################################################################################################################################  
   output$OD.6.c2 <- renderText({ 
     output=f.OD6(input$p61,input$p62,input$p63,input$p64,input$p65,input$p66,input$OR6,input$ratio6)
     paste("Relative efficiency of balanced design: ", round(output[50,3],2))
   })
 
   
######################################################################################################################
######################################################################################################################
######################################################################################################################
### 7 categories
######################################################################################################################
######################################################################################################################
######################################################################################################################
   
################################################################################################################################
### barplots probabilities 
################################################################################################################################  
   
   output$plot7a <- renderPlot({

     validate(
       need(input$p71+input$p72+input$p73+input$p74+input$p75+input$p76+input$p77==1, 'Input error: probabilities must sum to 1'),
       need(input$p71>0&input$p71<1, 'Input error: p1 not in interval [0,1]'),
       need(input$p72>0&input$p72<1, 'Input error: p2 not in interval [0,1]'),
       need(input$p73>0&input$p73<1, 'Input error: p3 not in interval [0,1]'),
       need(input$p74>0&input$p74<1, 'Input error: p4 not in interval [0,1]'),
       need(input$p75>0&input$p75<1, 'Input error: p5 not in interval [0,1]'),
       need(input$p76>0&input$p76<1, 'Input error: p6 not in interval [0,1]'),
       need(input$p77>0&input$p77<1, 'Input error: p7 not in interval [0,1]')
     )
     
     p.c=c(input$p71,input$p72,input$p73,input$p74,input$p75,input$p76,input$p77)
     beta=log(input$OR7)
     cp.c=cumsum(p.c)
     cp.c=cp.c[cp.c<1]
     logit.cp.c=log(cp.c/(1-cp.c))
     logit.cp.i=logit.cp.c+beta
     cp.i=exp(logit.cp.i)/(1+exp(logit.cp.i))
     p.i=rep(0,3)				### response probabilities intervention
     p.i[1]=cp.i[1]
     p.i[2]=cp.i[2]-cp.i[1]
     p.i[3]=cp.i[3]-cp.i[2]
     p.i[4]=cp.i[4]-cp.i[3]
     p.i[5]=cp.i[5]-cp.i[4]
     p.i[6]=cp.i[6]-cp.i[5]
     p.i[7]=1-cp.i[6]
     control=p.c
     intervention=p.i
     barplot(cbind(control,intervention),beside=TRUE,ylim=c(0,1),ylab="probability",legend=c(1,2,3,4,5,6,7))
   })
   
################################################################################################################################
### plot RE for D-optimality
################################################################################################################################  
   output$plot7b1 <- renderPlotly({
     output=f.OD7(input$p71,input$p72,input$p73,input$p74,input$p75,input$p76,input$p77,input$OR7,input$ratio7)

     output=as.data.frame(output)
     # use the key aesthetic/argument to help uniquely identify selected observations
     key <- row.names(output)
     
     plot_ly(output, x = ~output[,1], y = ~output[,2], key = ~key,mode='lines',type="scatter") %>%
       layout(dragmode = "select",
              xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
              yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
     
   })

################################################################################################################################
### plot RE for c-optimality
################################################################################################################################  
   output$plot7b2 <- renderPlotly({
     output=f.OD7(input$p71,input$p72,input$p73,input$p74,input$p75,input$p76,input$p77,input$OR7,input$ratio7)
     
     output=as.data.frame(output)
     # use the key aesthetic/argument to help uniquely identify selected observations
     key <- row.names(output)
     
     plot_ly(output, x = ~output[,1], y = ~output[,3], key = ~key,mode='lines',type="scatter") %>%
       layout(dragmode = "select",
              xaxis = list(title = "proportion subjects in intervention condition", range = c(0, 1), showgrid = F,zeroline=TRUE,showline = TRUE),
              yaxis = list(title = "Relative Efficiency", range=c(0,1.05),showgrid = T,zeroline=TRUE,showline = TRUE))
     
   })
   
################################################################################################################################
### text output optimal design for D-optimality
################################################################################################################################  
   output$OD.7.D1 <- renderText({ 
     output=f.OD7(input$p71,input$p72,input$p73,input$p74,input$p75,input$p76,input$p77,input$OR7,input$ratio7)
     opt.prop=which(output[,2]==1)
     paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
   })
   
################################################################################################################################
### text output RE balanced design for c-optimality
################################################################################################################################  
   output$OD.7.D2 <- renderText({ 
     output=f.OD7(input$p71,input$p72,input$p73,input$p74,input$p75,input$p76,input$p77,input$OR7,input$ratio7)
     paste("Relative efficiency of balanced design: ", round(output[50,2],2))
   })
   
################################################################################################################################
### text output optimal design for c-optimality
################################################################################################################################  
   output$OD.7.c1 <- renderText({ 
     output=f.OD7(input$p71,input$p72,input$p73,input$p74,input$p75,input$p76,input$p77,input$OR7,input$ratio7)
     opt.prop=which(output[,3]==1)
     paste("Optimal proportion subjects in intervention: ", output[opt.prop,1])
   })
   
################################################################################################################################
### text output RE balanced design c-optimality
################################################################################################################################  
   output$OD.7.c2 <- renderText({ 
     output=f.OD7(input$p71,input$p72,input$p73,input$p74,input$p75,input$p76,input$p77,input$OR7,input$ratio7)
     paste("Relative efficiency of balanced design: ", round(output[50,3],2))
   })
   

}

