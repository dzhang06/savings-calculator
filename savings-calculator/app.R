#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Savings Calculator"),
    sidebarLayout(
        sidebarPanel(
            numericInput("gross_income",
                        "Annual Gross Income:",
                        value = 60000) ,
            numericInput("posttax_income",
                         "After Tax Income:",
                         value = 60000),
            radioButtons("frequency",
                         "Paycheck Frequency:",
                         choices = c("Weekly" = 52,
                                     "Biweekly" = 26,
                                     "Monthly" = 12,
                                     "Twice monthly" = 24),
                         selected = 26),
            numericInput("percentage_pretax",
                         "Percentage Pre-tax",
                         value = 5),
            numericInput("percentage_posttax",
                         "Percentage After Tax",
                         value = 5)
        ),
            mainPanel(
            dataTableOutput("pretax"),
            dataTableOutput("posttax")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # percentage_pretax = 1
    # percentage_posttax = 1
    # gross_income = 100000
    # posttax_income = 80000
    # frequency = 26

    
    output$pretax <- renderDataTable({
        percentages_pretax <- c(1,5,10,20,30,input$percentage_pretax)
        pretax_table <- matrix(rep.int(0,12),ncol = 6, byrow = TRUE)
        colnames(pretax_table) <- as.character(percentages_pretax)
        rownames(pretax_table) <- c("Pre tax contribution per paycheck", "Pre tax contribution Yearly")
        pretax_table <- as.data.frame(pretax_table)
        pretax_yearly <- round(percentages_pretax / 100 * input$gross_income)
        pretax_by_paycheck <- round(pretax_yearly / as.numeric(input$frequency))
        pretax_table[1,] = pretax_by_paycheck
        pretax_table[2,] = pretax_yearly
        pretax_table
    })
    
    output$posttax <- renderDataTable({
        percentages_posttax <- c(1,5,10,20,30,input$percentage_posttax)
        
        posttax_table <- matrix(rep.int(0,12),ncol = 6, byrow = TRUE)
        colnames(posttax_table) <- as.character(percentages_posttax)
        rownames(posttax_table) <- c("After tax contribution per paycheck", "After tax contribution Yearly")
        posttax_table <- as.data.frame(posttax_table)
        colnames(posttax_table) <- percentages_posttax
        posttax_yearly <- round(percentages_posttax / 100 * input$posttax_income)
        posttax_by_paycheck <- round(posttax_yearly / as.numeric(input$frequency))
        posttax_table[1,] = posttax_by_paycheck
        posttax_table[2,] = posttax_yearly
        colnames(posttax_table) <- percentages_posttax
        posttax_table
        
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
