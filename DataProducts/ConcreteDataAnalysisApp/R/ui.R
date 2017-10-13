library(shiny); library(fastR); data(concreteAll)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Concrete Data Analysis"),
  
  # Sidebar with X and Y variables to plot
  sidebarLayout(
    sidebarPanel(
        selectInput("xcol", "X variable to analysis:", selected = "cement", names(concreteAll)),
        selectInput("ycol", "Y variable to analysis:", selected = "strength",names(concreteAll)),
        selectInput("group", "Grouping by:", selected = "water", names(concreteAll)),
        checkboxInput("model","Fit Linear Model",value=F)
    ),
    
    # Show a plot with the selected variables
    mainPanel(
       plotOutput("plot")
    )
  )
))
