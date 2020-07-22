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
ui <- fluidPage(
    titlePanel("Savings Calculator (based on W2 and estimation)"),
    sidebarLayout(
        sidebarPanel(
            numericInput("gross_income",
                        "Annual Gross Income:",
                        value = 100000) ,
            radioButtons("frequency",
                         "Paycheck Frequency:",
                         choices = c("Weekly" = 52,
                                     "Biweekly" = 26,
                                     "Monthly" = 12,
                                     "Twice monthly" = 24),
                         selected = 26),
            numericInput("pretax_401k",
                         "Pretax 401k contribution percentage:",
                         value = 10),
            checkboxInput("match",
                          "Employer Match Offered",
                          value = FALSE),
            conditionalPanel(condition = "input.match == '1'",
                             numericInput("match_percent",
                                          "Match Percent",
                                          value = 3)),
            numericInput("max_401k",
                         "Max yearly 401k contribution limit:",
                         value = 19500),
            numericInput("max_total",
                         "Total employee/employer contribution limit:",
                         value = 57000),
            numericInput("tax_rate",
                         "Tax rate percentage:",
                         value = 20),
            numericInput("posttax_401k",
                         "Posttax 401k contribution percentage:",
                         value = 5),
            numericInput("posttax_adjustments",
                         "Posttax adjustments in dollars:",
                         value = 100)
        ),
        
            mainPanel(
            h1("Overall Breakdown"),
            tableOutput("overall_breakdown"),
            h5(textOutput("note")),
            h1("Yearly Retirement Breakdown"),
            tableOutput("retirement_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    gross_income <- reactive({input$gross_income})
    pay_frequency <- reactive({as.numeric(input$frequency)})
    pretax_401k <- reactive({input$pretax_401k})
    tax_rate <- reactive({input$tax_rate})
    posttax_401k <- reactive({input$posttax_401k})
    posttax_adj <- reactive({input$posttax_adjustments})
    max_401k <- reactive({input$max_401k})

    calc_401k <- reactive({gross_income() * pretax_401k() / 100})
    
    gross <- reactive({c(gross_income(),round(gross_income() / pay_frequency()))})
    pretax_401k_contribution <- reactive({
        if (calc_401k() <= max_401k()) {
            c(calc_401k(), round(calc_401k() / pay_frequency()))
        } else {
            c(max_401k(), round(calc_401k() / pay_frequency()))
        }
    })
    pay_after_401k <- reactive({gross() - pretax_401k_contribution()})
    tax <- reactive({round(pay_after_401k() * tax_rate() / 100)})
    after_tax <- reactive({pay_after_401k() - tax()})
    posttax_401k_row <- reactive({round(after_tax() * posttax_401k() / 100)})
    after_posttax_401k <- reactive({after_tax() - posttax_401k_row()})
    posttax_adjustment <- reactive({c(posttax_adj() * pay_frequency(),posttax_adj())})
    after_adjustment <- reactive({after_posttax_401k() - posttax_adjustment()})
    
    overall_table <- reactive({
        temp <- rbind( gross(),pretax_401k_contribution(),pay_after_401k(),
                           tax(),after_tax(),posttax_401k_row(),after_posttax_401k(),
                           posttax_adjustment(), after_adjustment())
        colnames(temp) <- c("Annual","Per Paycheck")
        rownames(temp) <- c("Gross Income",
                            "Pretax 401k Contribution",
                            "Income after pretax 401k",
                            "Tax",
                            "Net Income After Tax",
                            "PostTax 401k Contribution",
                            "Net Income After Posttax contribution",
                            "Post tax Adjustment",
                            "Grand Total Take-home Pay")
        temp
    })

    pretax <- reactive({
        temp <- pretax_401k_contribution()
        temp[1]
    })
    
    posttax <- reactive({
        temp <- posttax_401k_row()
        temp[1]
    })
    
    retirement_table <- reactive({
        temp_tab <- rbind(pretax(),posttax(), 0)
        if (input$match) {
            temp_tab[3] <- input$match_percent/100 * gross_income()
        }
        temp_tab <- rbind(temp_tab,temp_tab[1] + temp_tab[2] + temp_tab[3])
        temp_tab <- rbind(temp_tab,input$max_total - temp_tab[4])
        colnames(temp_tab) <- c("Assets")
        rownames(temp_tab) <- c("Pretax contribution",
                                "Posttax contribution",
                                "Employer Match (pretax)",
                                "Grand Total",
                                "Amount left to save")
        temp_tab
    })
    
    output$overall_breakdown <- renderTable({
        overall_table()

    }, rownames = TRUE, digits = 0)
    
    
    # note <- ""
    # observe({
    #     
    #     if (calc_401k > max_401k) {
    #         note <- "Note!"
    #     }
    # })
    
    output$note <- renderText({
        text <-  ""
        per_payperiod_contribution <- calc_401k() / pay_frequency()
        max <-  0
        if (calc_401k() > max_401k()) {
            max <- max_401k() / per_payperiod_contribution
            text = paste("*Note* This pretax 401k contribution percentage 
            exceeds the yearly maximum. You will max it out in approximately", 
            round(max), "pay periods.",sep = " ")
        } 
        text
    })
    
    output$retirement_table <- renderTable({
        retirement_table()
    }, rownames = TRUE, digits = 0)
}

# Run the application 
shinyApp(ui = ui, server = server)
