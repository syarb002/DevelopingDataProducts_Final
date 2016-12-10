library(shiny)

ui <- fluidPage(
        verticalLayout(
   
         titlePanel("Distribution of Letters in a String"),
         
         headerPanel(
            h5("Ever wonder how many times a certain letter appears in a string of text?  Probably not, but this app will take your 
            string and generate a plot showing the number of times each letter appears.")
            ),
         
          tags$br(),
          
          fluidRow(column(width = 4,      
            wellPanel(
              h6("Enter as much text as you want into the box below.  Then use the slider to designate how many of the top letters 
              you would like to have plotted, and click the Plot It button to generate the graph."),
              tags$br(),
              textAreaInput("text", "Type Here!", height = "250px"),
              sliderInput("topn", "Show me the top...", min = 1, max = 15, value = 10),
              actionButton("goPlot", "Plot it!")
           )),
           
           mainPanel(
             plotOutput("plot")
           )
         )
       ),
      h6("Sean Yarborough, Developing Data Products, December 12, 2016")
    )



server <- function(input, output) {
  
  observeEvent(input$goPlot, {
    letterDist <- data.table(sort(table(strsplit(toupper(input$text), split = "")), TRUE))[1:input$topn] %>%
      filter(V1 %in% LETTERS)
    output$plot <- renderPlot({
      ggplot(letterDist, aes(reorder(V1, -N), N)) + geom_bar(stat = "identity", fill = "blue") +
        xlab("letter") +
        ylab("frequency of use")
    })
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

