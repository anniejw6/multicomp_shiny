library(shiny)

# UI
ui <- fluidPage(
  
    titlePanel("Super-Simple Multiple Comparisons"),
    sidebarLayout(
      sidebarPanel(
        p(""),
        
        numericInput(inputId = "num_treat", 
                     label = "Number of Treatments", 
                     value = 1),
        
        numericInput(inputId = "num_subgroup", 
                     label = "Number of Subgroups", 
                     value = 3),
        
        numericInput(inputId = "num_bins", 
                     label = "Number of Bins Per Subgroup", 
                     value = 3),
        
        numericInput(inputId = "num_dv", 
                     label = "Number of Outcomes", 
                     value = 3)
        
      ),
      mainPanel(
        p(textOutput('comparison')),
        tableOutput('pval')
      )
    )
  
)

# Backend
server <- function(input, output) {
  
  num_comp <- reactive({
    input$num_treat * input$num_subgroup *
    input$num_bins * input$num_dv
  })
  
  output$comparison <- renderText({
    # Calculate number of comparisions
    sprintf('You are making %s comparisons!', num_comp())

  })
  
  output$pval <- renderTable({
    
    p_vals <- c(0.1, 0.05, 0.001, 0.0001)
    comps <- sapply(
      p_vals, function(x) p.adjust(x, method = 'holm', n = num_comp()))
    
    data.frame(`Original P-Value` = round(p_vals, 4),
               `Adjusted P-Value` = round(comps, 4)
               )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

