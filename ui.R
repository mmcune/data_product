library(shiny)

# rEF: http://shiny.rstudio.com/gallery/update-input-demo.html

shinyUI(
  fluidPage(
    titlePanel("Monthly Electricity Price Forecast"),

    tabsetPanel(
      tabPanel("App",
               fluidRow(textOutput("my.intro"),
                        br(),br()
                        ),
               fluidRow(
                        tags$b(textOutput("my.content")), br(),br()
                       ),
               fluidRow(
                 column(4,
                        htmlOutput("selector"),
                        br(),br(),
                        tags$em("Select Year and Month to display forecasted price above"),
                        br(),br(),
                        htmlOutput("sel_year"),
                        br(),
                        htmlOutput("sel_month")
                 ),
                 column(8,
                        plotOutput("o_plot")
                 )
               ),
               fluidRow(br(),br(),
                        "This data is published by the US Energy Information Administration and is available from ",
                        a(href="http://www.eia.gov/beta/api/qb.cfm?category=40","http://www.eia.gov/beta/api/qb.cfm?category=40")
               )
#              ),      
#      tabPanel("Docs",
#               "Here is the writeup"
              )

    )
  )
)