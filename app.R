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
        tabPanel("Project Description", htmlOutput("output1")),
        tabPanel("Amount vs Status", plotOutput(outputId = "plot1")),
        tabPanel("Good Loan vs Bad Loan", plotOutput(outputId = "plot2")),
        tabPanel("Interest Rate vs Verification Status", plotOutput(outputId = "plot3")),
        tabPanel("Descriptive Summary", verbatimTextOutput(outputId = "summary"))
      )
    )
  )
)
server <- function(input, output){
  
 output$output1 <- renderUI({
   para1 <- "This project predicts the loan status for Lending Club to process the loan approval pre-check system. We have choosen 
   12 most significant columns from the dataset for data exploration and machine learning models. The final models were trained 
   on Lending Club 2016 Quarter 1 dataset and tested on Quarter 2 datset. "
   para2 <- "Lending club is the world's largest peer-to-peer lending platform which uses technology to create a credit
   marketplace at a lower cost than traditional bank loan programs. By connecting borrowers and investors
   directly and allowing them to invest in and borrow from each other, it avoids the cost and complexity of
   the banking system and passes the savings on to borrowers in the form of lower rates and to investors
   in the form of solid returns. The entire process is online and relies on technology to promote affordability
   of the credit over availability of the credit. "
   para3 <- "<b>Data Dictionary</b>"
   para4 <- "<b>Loan Amount</b> - The listed amount of the loan applied for by the borrower."
   para5 <- "<b>Grade</b> - Grade field is the grade assigned to each loan by Lending Club. It reflects how likely the loan is to be paid
   off and is determined based on the creditworthiness of the borrower. A being the best."
   para6 <- "<b>Interest Rate</b> - Interest Rate is the rate of interest on the loan."
   para7 <- "<b>Annual Income</b> - Applicants enter their annual income during the preliminary application process."
   para8 <- "<b>Verification Status</b> - Indicates whether the co-borrower’s joint income has been verified by Lending Club."
   para9 <- "<b>Loan Status</b> - The term loan status means the current status of the borrower. There are 8 different categories a
borrower can be placed under which are the following: charged off, current, default, fully paid, in grace period, issued, last (16-30 days), late (31-120 days)."
 HTML(paste(para1, para2, para3, para4, para5, para6, para7, para8, para9, sep = '<br/>'))
 })
  
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
      ggplot(data=df_select, aes(x=verification_status, y=new_int_rate, fill=verification_status)) + 
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
