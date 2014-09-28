library(shiny)
library(forecast)

# Global assignments and processing (runs once when app is launched)
# raw data:
# Start: 200101
# End  : 201406

raw.data <- read.csv("data/Average_retail_price_of_electricity_monthly.csv", skip = 5, header=FALSE)

residential.prices <- rev(raw.data[,2])

# weirdness on shiny server prevents date conversions with non-standard formats, so convert month name to number
month_num <- c("Jan" = 1,"Feb" = 2,"Mar" = 3,"Apr" = 4,"May" = 5,"Jun" = 6,"Jul" = 7,"Aug" = 8,"Sep" = 9,"Oct" = 10,"Nov" = 11,"Dec" = 12)

units <- "Cents per Kilowatthour."

# max range of forecast
max_date  <- as.Date('2016-07-01')
min_date  <- as.Date('2014-06-01')

pr.ts  <- ts(residential.prices, start=c(2001,1), end=c(2014,6), frequency=12)
fcast  <- forecast(stlf(pr.ts))
fc.df  <- as.data.frame(fcast)


#fcplot <- plot(fcast)

shinyServer(
  function(input,output) {
    
    errorText <- paste("Date out of range.  Please choose a date between",as.character(min_date,"%b %Y"),"and",as.character(max_date,"%b %Y"),",exclusive.")
    
    output$o_plot  <- renderPlot({plot(fcast)})
    
    output$o_results <- renderText({
      input$in_button
      
      year  <- isolate(input$in_year)
      month  <- isolate(input$in_month)
     
      if (as.Date(paste(year,'-',month_num[month],'-','1', sep='')) <= min_date || as.Date(paste(year,'-',month_num[month],'-','1', sep='')) >= max_date) {
        o_text <- errorText
      } else {
        result <- fc.df[paste(month,year),]
        o_text <- paste("The forecasted price for",paste(month,year),"is",round(result[1,1], digits=4), units)
        o_text <- paste(o_text," The 95% confidence interval is", round(result[1,4], digits=4), "to", round(result[1,5], digits=4),units)
      }
      o_text
    })    
    
    
    
  }
)