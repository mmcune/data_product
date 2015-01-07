library(shiny)
library(forecast)
library(rjson)

# Global assignments and function definitions (runs once when app is launched)

sectors <- c(
  'All Sectors' = 'http://api.eia.gov/series/?api_key=47AE9F6B5604F30F0421D6B624183E78&series_id=ELEC.PRICE.US-ALL.M',
  'Residential' = 'http://api.eia.gov/series/?api_key=47AE9F6B5604F30F0421D6B624183E78&series_id=ELEC.PRICE.US-RES.M',
  'Commercial'  = 'http://api.eia.gov/series/?api_key=47AE9F6B5604F30F0421D6B624183E78&series_id=ELEC.PRICE.US-COM.M',
  'Industrial'  = 'http://api.eia.gov/series/?api_key=47AE9F6B5604F30F0421D6B624183E78&series_id=ELEC.PRICE.US-IND.M',
  'Transportation'  = 'http://api.eia.gov/series/?api_key=47AE9F6B5604F30F0421D6B624183E78&series_id=ELEC.PRICE.US-TRA.M',
  'Other'  = 'http://api.eia.gov/series/?api_key=47AE9F6B5604F30F0421D6B624183E78&series_id=ELEC.PRICE.US-OTH.M'
)

# weirdness on shiny server prevents date conversions with non-standard formats, so convert month name to number
month_num <- c("Jan" = 1,"Feb" = 2,"Mar" = 3,"Apr" = 4,"May" = 5,"Jun" = 6,"Jul" = 7,"Aug" = 8,"Sep" = 9,"Oct" = 10,"Nov" = 11,"Dec" = 12)

units <- "Cents per Kilowatthour."
title <- "Monthly Average Electricity Prices"


get_data <- function(url) {
  dat <- ''
  dat <- fromJSON(scan(url,"",sep="\n"))
  dat
}

process_data <- function(dat) {
  out.prices <- ''
  prices <- dat$series[[1]]$data
  
  prices <- do.call(rbind,prices)
  out.prices <- rev(unlist(prices[,2]))
  out.prices
}

# this function converts strings to date objects
# Strings must be in format Mon YYYY or YYYYMM
str2date <- function(str) {
  my_date <- ''
  if (grepl('^\\w{3} \\d{4}', str)) {
    parts <- unlist(strsplit(str,' '))
    my_date <- as.Date(paste(parts[2],month_num[parts[1]], '01', sep='-'))
  } else if (grepl('\\d{6}',str)) {
    yr <- substr(str,1,4)
    mm <- substr(str,5,6)
    dd <- '01'
    my_date <- as.Date(paste(yr,mm,dd, sep='-'))
  }
  my_date
}

# Takes String in Mon YYYY format and returns Mon or YYYY
# Created to work around (apparent) shinyapp date format issues
get_date_part <- function(str,type) {
  val <- ''
  if (type == 'Mon') {
    val <- sub('^(\\w{3}).*','\\1',str)
  } else if (type == 'YYYY') {
    val <- sub('^.* (\\d{4})','\\1',str)
  }
  val
}

# Seed the system
#dat <- get_data(sectors['All Sectors'])


shinyServer(
  function(input,output) {
    
    download_error <- NULL
    
    start <- ''
    end <- ''
    
    get.data <- reactive({
      dat <- get_data(sectors[input$select.sector])
    })
    
    make.forecast <- reactive({
      
      if(!exists("dat")) {
        dat <- get.data()
      }
      
      if (is.null(dat$data$error)) {
        dat.prices <- process_data(dat)
      } else {
        download_error <- "Data Download Error"
      }
      
      start <- dat$series[[1]]$start
      end <- dat$series[[1]]$end
      
      start_yr <- as.numeric(substr(start,1,4))
      start_mon <- as.numeric(substr(start,5,6))
      end_yr <- as.numeric(substr(end,1,4))
      end_mon <- as.numeric(substr(end,5,6))
      
      pr.ts  <- ts(dat.prices, start=c(start_yr,start_mon), end=c(end_yr,end_mon), frequency=12)
      fcast  <- forecast(stlf(pr.ts), na.ignore=TRUE)
      
      fcast
    })
    
    fcast.dates <- reactive({
      
      if(!exists("fcast")) {
        fcast <- make.forecast()
      }
      
      fc.df  <- as.data.frame(fcast)
      
      fc_dates <- row.names(fc.df)
      min_date <- fc_dates[1]
      max_date <- fc_dates[length(fc_dates)]
      c(min_date,max_date)
    })
    
    
    
    output$selector <- renderUI({selectInput("select.sector", "Choose Sector", names(sectors))})
    
    output$sel_year <- renderUI({
      fc_date_range <- fcast.dates()
      min_date <- fc_date_range[1]
      max_date <- fc_date_range[2]
      selectInput("in_year", "Year", 
                  seq(as.numeric(get_date_part(min_date,'YYYY')),as.numeric(get_date_part(max_date,'YYYY'))),
                  selected=get_date_part(min_date,'YYYY'))
                  })
    
    output$sel_month <- renderUI({
      fc_date_range <- fcast.dates()
      min_date <- fc_date_range[1]
      max_date <- fc_date_range[2]
      selectInput("in_month","Month", 
                  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), 
                  selected=get_date_part(min_date,'Mon'))
                  })
    
    output$o_plot  <- renderPlot({
      fcast  <-  make.forecast()
      plot(fcast,main=paste(title,'-',input$select.sector), ylab=units)
    })
    
    output$my.intro <- renderText({
      if(!exists("dat")) {
        dat <- get.data()
      }
      start <- dat$series[[1]]$start
      end <- dat$series[[1]]$end
      start.date <- as.character(str2date(start),"%b %Y")
      end.date <- as.character(str2date(end),"%b %Y")
      
      fc_date_range <- fcast.dates()
      min_date <- fc_date_range[1]
      max_date <- fc_date_range[2]
      paste("This application calculates the forecasted electricity prices based on the monthly average prices in the United States from",
            start.date,
            "to",
            end.date,
            ".  Forecasted prices are available for",
            min_date, 
            "to",
            max_date,
            ".  These prices were calculated using the forecast package in R.")
    })
    
    output$my.content <- renderText({
      
      if(!exists("fcast")) {
        fcast <- make.forecast()
      }
      
      fc.df  <- as.data.frame(fcast)
      
      fc_date_range <- fcast.dates()
      min_date <- fc_date_range[1]
      max_date <- fc_date_range[2]

      month <- input$in_month
      year <- input$in_year
      
      errorText <- paste("Date out of range.  Please choose a date between",min_date,"and",max_date,".")
      if (as.Date(paste(year,month_num[month],'1', sep='-')) < str2date(min_date) || as.Date(paste(year,month_num[month],'1', sep='-')) > str2date(max_date)) {
        o_text <- errorText
      } else if (!is.null(download_error)) {
        o_text <- download_error
      } else {
        result <- fc.df[paste(month,year),]
        o_text <- paste("The forecasted price for",paste(month,year),"is",round(result[1,1], digits=4), units)
        o_text <- paste(o_text," The 95% confidence interval is", round(result[1,4], digits=4), "to", round(result[1,5], digits=4),units)
      }
      o_text
      
    })
    
  }
)