#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

df = read.csv('data/LoanStats_2016Q1.csv', skip = 1)
df_select <- df[,c("loan_amnt", "term", "int_rate", "installment", "grade","home_ownership", "annual_inc", "verification_status", "loan_status", "pymnt_plan", "purpose", 
                   "zip_code", "addr_state" )]

df_select["new_loan_status"] <- NA
df_select$new_loan_status <- ifelse((df_select$loan_status == 'Current')|(df_select$loan_status == 'Fully Paid'), 'Good Loan', 'Bad Loan')
df_select$new_int_rate = as.numeric(gsub("%", "", df_select$int_rate))

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Lending Club Loan Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "grade",label = "Select Your Grade",choices = c("A", "B", "C", "D", "E", "F", "G"),selected = "A"),
      #selectInput(inputId = "home_ownership", label = "Select the Home Ownership Status", choices = c("MORTGAGE", "OWN", "RENT"), selected = "MORTGAGE"),
      sliderInput(inputId = "annual_inc", "Select the annual income range", min = 0, max = 150000, value = 15000)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "plot1")),
        tabPanel("Plot", plotOutput(outputId = "plot2")),
        tabPanel("Plot", plotOutput(outputId = "plot3")),
        tabPanel("Summary", verbatimTextOutput(outputId = "summary"))
      )
    )
  )
)
server <- function(input, output){
  
  output$plot1 <- renderPlot(
    {
      df_select1 = df_select[(df_select$grade == input$grade) & (df_select$annual_inc <= input$annual_inc) , ]
      ggplot(data=df_select1, aes(x=loan_status, y=loan_amnt, fill=loan_status)) + 
        geom_bar(stat="identity") + 
        labs(title="Loan amount vs Loan Status")
    }
  )
  output$plot2 <- renderPlot(
    {
      df_select1 = df_select[(df_select$grade == input$grade) & (df_select$annual_inc <= input$annual_inc) , ]
      ggplot(data=df_select1, aes(x=new_loan_status, y=loan_amnt, fill=new_loan_status)) + 
        geom_bar(stat="identity") + 
        labs(title="Loan amount vs Bi-Category Loan Status")
    }
  )
  output$plot3 <- renderPlot(
    {
      df_select1 = df_select[(df_select$grade == input$grade) & (df_select$annual_inc <= input$annual_inc) , ]
      ggplot(data=df_select, aes(x=verification_status, y=new_int_rate, fill=df_select$verification_status)) + 
        geom_bar(stat="identity") + 
        labs(title="Interest Rate vs Verification Status")
    }
  )
  output$summary <- renderPrint({
    df_select1 = df_select[(df_select$grade == input$grade) & (df_select$annual_inc <= input$annual_inc) , ]
    summary(df_select1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
