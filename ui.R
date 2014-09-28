library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Monthly Residential Electricity Price Forecast"),

    tabsetPanel(
      tabPanel("App",
               fluidRow("This application calculates the forecasted residential electricity prices based on the monthly average prices in the United States from January 2001 to June 2014.  Forecasted prices are available for July 2014 to Jun 2016.  These prices were calculated using the forecast package in R.",
                        br(),br()
                        ),
               fluidRow(
                        tags$b(textOutput("o_results")), br(),br()
                       ),
               fluidRow(
                 column(4,
                        tags$em("Select Year and Month, then press 'Get Forecast' to display forecasted price above"),
                        br(),br(),
                        selectInput("in_year", "Year", c(2014,2015,2016)),
                        br(),
                        selectInput("in_month","Month", c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), selected="Jul"),
                        actionButton("in_button", "Get Forecast")
                 ),
                 column(8,
                        plotOutput("o_plot"),
                        br(),
                        "Figure 1. The average monthly price of residential electricity in the United States, January 2001 - June 2016."
                 )
               ),
               fluidRow(br(),br(),
                        "This data is published by the US Energy Information Administration and is available from ",
                        a(href="http://www.eia.gov/electricity/monthly/epm_table_grapher.cfm?t=epmt_5_3","http://www.eia.gov/electricity/monthly/epm_table_grapher.cfm?t=epmt_5_3")
               )
#              ),      
#      tabPanel("Docs",
#               "Here is the writeup"
              )

    )
  )
)