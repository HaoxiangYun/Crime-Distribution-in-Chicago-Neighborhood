#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui = shinyUI(
fluidPage(
    
    # Application title
    titlePanel("Crime and Socio-Economic Trends"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("slider",
                        "Community Area Numbers:",
                        min = 1,
                        max = 77,
                        value =c(1,77) ),
            selectInput(inputId = "select",
                        label = h3("Select What to Plot against Crime"),
                        choices = list("Infant Mortality Rate" = 1,
                                       "Income per Capita" = 2,
                                       "Birth Rate" = 3,
                                       "Unemployment" = 4),
                        selected = 1),
            submitButton("Apply Changes")
            
        ),
        
        
        mainPanel(tabsetPanel(
            tabPanel("About", textOutput("about")),
            tabPanel("Plot",plotOutput("distPlot")),
            tabPanel("Community Area Map of Chicago",tags$img(src = "https://www.housingstudies.org/media/filer_public/2013/05/21/chicago_comm_areas_ref_color.jpg",height = "500",width="500")),
            tabPanel("Crime Density WordCloud",plotOutput("wordcloud")),
            tabPanel("Summary", tableOutput("summary")),
            tabPanel("Linear Regression", plotOutput("reg"))
            
        )
           
        )
    )
)
)


server = function(input, output) {
    active_dataset1 = reactive({   # Reactive
        if(input$select == 3) {
            yvar = "Birth.Rate"
        } else if (input$select == 2) {
            yvar = "Per.Capita.Income"
        } else if (input$select == 1){
            yvar = "Infant.Mortality.Rate"
        } else{
            yvar = "Unemployment"
        }
        plot_versus_crime(yvar,input$slider[1],input$slider[2])
    }) # close: reactive()
    
    active_dataset2 = reactive({   # Reactive
        if(input$select == 3) {
            yvar = "Birth.Rate"
        } else if (input$select == 2) {
            yvar = "Per.Capita.Income"
        } else if (input$select == 1){
            yvar = "Infant.Mortality.Rate"
        } else{
            yvar = "Unemployment"
        }
        linear_reg(yvar,input$slider[1],input$slider[2])
    })
    output$distPlot = renderPlot({
        active_dataset1()
    })
    
    output$wordcloud = renderPlot({
        wordcloud(words = ncrimes_health$Community.Area.Name,freq = ncrimes_health$Number_of_Crimes,random.order = FALSE, scale = c(3,.2), random.color = TRUE, colors = c(colors()))
    })
    output$about = renderText({
        about_text
    })
    output$summary = renderTable({
        summary
    })
    output$reg = renderPlot({
        active_dataset2()
    })
}

# Run the application 



shinyApp(ui = ui, server = server)

