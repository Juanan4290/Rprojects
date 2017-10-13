library(shiny); library(ggplot2);

shinyServer(function(input, output) {
    
    selectedData = reactive({
        concreteAll[,c(input$xcol,input$ycol,input$group)]
    })
    
    modelFit=reactive({
        x=concreteAll[,input$xcol]
        y=concreteAll[,input$ycol]
        model=lm(y ~ x)
    })
    
    output$plot <- renderPlot({
    ggplot(data=selectedData(),aes(x=selectedData()[,1],
                                   y=selectedData()[,2],
                                   col=selectedData()[,3]))+
            geom_point()+xlab(input$xcol)+ylab(input$ycol)+
            labs(col=input$group)+
        if(input$model==T){
            geom_smooth(method='lm',col="red",se=T)
        }

  })
  
})
