library(shiny)
library(wbstats)
library(tidyr)
library(shinythemes)
library(data.table)
library(ggplot2)

#all indicator
wdi_ind <- readRDS('wdi.rds')


#following are regions with iso2c country codes we are interested to monitor
IDB <- "AF, AL, DZ, AZ, BH, BD, BJ, BN, BF, CM, TD, KM, CI, DJ, EG, GA, GM, GN, GW, GY, ID, IR, IQ, JO, KZ, KW, KG, LB, LY, MY, MV, ML, MR, MA, MZ, NE, NG, OM, PK, PS, QA, SA, SN, SL, SO, SD, SR, SY, TJ, TG, TN, TR, TM, AE, UG, UZ, YE"
IDB_MENA_18 <- "DZ, BH, EG, IR, IQ, JO, KW, LB, LY, MA, OM, PS, QA, SA, SY, TN, AE, YE"
IDB_Sub_Saharan_Africa_22 <- "BJ, BF, CM, TD, KM, CI, DJ, GA, GM, GN, GW, ML, MR, MZ, NE, NG, SN, SL, SO, SD, TG, UG"
IDB_Asia_Latin_America_9 <- "AF, BD, BN, GY, ID, MY, MV, PK, SR"
IDB_Europe_Central_Asia_8 <- "AL, AZ, KZ, KG, TJ, TR, TM, UZ"
IDB_Non_LDMC_32 <- "AL, DZ, AZ, BH, BN, CM, CI, EG, GA, GY, ID, IR, IQ, JO, KZ, KW, LB, LY, MY, MA, NG, OM, PK, QA, SA, SR, SY, TN, TR, TM, AE, UZ"
IDB_LDMC_25 <- "AF, BD, BJ, BF, TD, KM, DJ, GM, GN, GW, KG, MV, ML, MR, MZ, NE, PS, SN, SL, SO, SD, TJ, TG, UG, YE"
IDB_Fuel_18 <- "DZ, AZ, BH, BN, TD, GA, IR, IQ, KZ, KW, LY, NG, OM, QA, SA, TM, AE, YE"
IDB_Non_Fuel_39 <- "AF, AL, BD, BJ, BF, CM, KM, CI, DJ, EG, GM, GN, GW, GY, ID, JO, KG, LB, MY, MV, ML, MR, MA, MZ, NE, PK, PS, SN, SL, SO, SD, SR, SY, TJ, TG, TN, TR, UG, UZ"
WORLD <- "WLD, HIC, LMY, LDC"

#global functions

#this function takes input a region, and two indicator codes: target indicator and weight indicator
# it uses 'weighted.mean' function and returns a data.table with one observation
aggregate <- function(region,indicator, weight) {
  ind_dt <- data.table(indicator=indicator$indicator, country=indicator$country, country.id=indicator$iso2c,year=indicator$date, value=indicator$value)
  wgt_dt <-  data.table(indicator=weight$indicator, country=weight$country,country.id=indicator$iso2c, year=weight$date, value=weight$value)
  ind_ans<- ind_dt[country.id %in% lapply(strsplit(region,","), trimws)[[1]],.(indicator,country,value),by=year]
  wgt_ans<- wgt_dt[country.id %in% lapply(strsplit(region,","), trimws)[[1]],.(indicator,country,value), by=year]
  cal_dt<- merge(ind_ans,wgt_ans, by=c("country", "year"))
  result<- cal_dt[,.(weighted.mean(value.x, value.y, na.rm = T)), by=year]
  result$economy <- deparse(substitute(region))
  return(result)
}

# this is same as aggregate function, except it does not take any region country codes
# rather it cosiders all the 57 member countries and creates the IDB aggregate
aggregate_idb <- function(indicator, weight) {
  ind_dt <- data.table(indicator=indicator$indicator, country=indicator$country, year=indicator$date, value=indicator$value)
  ind_ans<- ind_dt[,.(indicator,country,value),by=year]
  wgt_dt <-  data.table(indicator=weight$indicator, country=weight$country, year=weight$date, value=weight$value)
  wgt_ans<- wgt_dt[,.(indicator,country,value), by=year]
  cal_dt<- merge(ind_ans,wgt_ans, by=c("country", "year"))
  result<-(cal_dt[,.(weighted.mean(value.x, value.y, na.rm = T)), by=year])
  result$economy<- "IDB"
  return(result)
}

server <- function(input,output, session){
  
  dat<- eventReactive(input$update,{
    selection <- wdi_ind$indicatorID[wdi_ind$indicator==input$indicator]
      withProgress({
        setProgress(message = "fetching indicators for IDB MC data ...")
        df<- wb(indicator=selection, country = strsplit(IDB, ", ")[[1]], 
            start = input$year[1], end = input$year[2])
      })
    #taking only relevant columns
    my_target<- df[,c(1,2,4,5,6)]
    ind_name <- df$indicator[1]
    #spread the years as columns
    countries<- spread(df[,c(1,2,6)],key = date, value = value )
    #comply with IDB naming and order of countries
    countries[countries$country=="Brunei Darussalam", "country"] <- "Brunei"
    countries[countries$country=="Egypt, Arab Rep.", "country"] <- "Egypt"
    countries[countries$country=="Gambia, The", "country"] <- "Gambia"
    countries[countries$country=="Iran, Islamic Rep.", "country"] <- "Iran"
    countries[countries$country=="West Bank and Gaza", "country"] <- "Palestine"
    countries[countries$country=="Syrian Arab Republic", "country"] <- "Syria"
    countries[countries$country=="United Arab Emirates", "country"] <- "U.A.E"
    countries[countries$country=="Yemen, Rep.", "country"] <- "Yemen"
    countries<- countries[order(countries$country),]
    
    #and now the weights
    weight_selection <- wdi_ind$indicatorID[wdi_ind$indicator==input$weight]
    withProgress({
      setProgress(message = "fetching weight indicator ...")
      my_weight<- wb(indicator=weight_selection, country = strsplit(IDB, ", ")[[1]], 
                     start = input$year[1], end = input$year[2])
    })
    
    
    idb_aggregates <- aggregate_idb(my_target, my_weight)
    idb_aggregates1 <- rbind(idb_aggregates, 
                            aggregate(IDB_Sub_Saharan_Africa_22, my_target, my_weight),
                            aggregate(IDB_MENA_18, my_target, my_weight),
                            aggregate(IDB_Asia_Latin_America_9, my_target, my_weight),
                            aggregate(IDB_Europe_Central_Asia_8, my_target, my_weight),
                            aggregate(IDB_LDMC_25, my_target, my_weight),
                            aggregate(IDB_Non_LDMC_32, my_target, my_weight),
                            aggregate(IDB_Fuel_18, my_target, my_weight),
                            aggregate(IDB_Non_Fuel_39, my_target, my_weight)
                            
    )
    idb_aggregates <- spread(idb_aggregates1, year, V1)
    idb_aggregates$s <- c(1,4,5,8,6,3,9,7,2)
    idb_aggregates <- idb_aggregates[order(idb_aggregates$s),]
    idb_aggregates$s <- NULL
    idb_aggregates<-as.data.frame(idb_aggregates)
    
    
    #now the some World statistics
    withProgress({
      setProgress(message = "fetching world stats ...")
      world<- wb(indicator=selection, country = strsplit(WORLD, ", ")[[1]], 
                     start = input$year[1], end = input$year[2])
    })
    world<- world[,c(1,2,6)]
    world <- spread(world,date, value )
    world$s<-c(2,4,3,1)
    world<-world[order(world$s),]
    world$s<-NULL
    colnames(world)[1]<-"economy"
    idb_aggregates<- rbind(idb_aggregates, world)
    list(countries, idb_aggregates, ind_name, idb_aggregates1)
    })
  
  output$header <- renderUI({
        h3(dat()[[3]])
    })
  output$header2 <- renderUI({
    h3(dat()[[3]])
  })
  output$table <- renderTable(dat()[[1]])
  output$aggregates <- renderTable(dat()[[2]])
  
  #plot
  output$plot <- renderPlot({
    df <- dat()[[4]]
    ggplot(data=df,
           aes(x=year, y=V1,group=economy, colour=economy)) +
      geom_line() +
      labs(y="Value", title=dat()[[3]])
    
  })
  
  #download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      agg <- dat()[[2]]
      colnames(agg)[1]<-c("country")
      ff <- rbind(dat()[[1]], agg)
      write.csv(ff, file, row.names = F)
    }
  )
}

ui <- fluidPage(theme=shinytheme("cosmo"),
  titlePanel("Analyse macro-economic indicators for the IDB member countries"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "indicator", "Select indicator:", wdi_ind$indicator, selected = 'Mortality rate, under-5 (per 1,000 live births)'),
      selectInput(inputId = "weight", "Select weight indicator:", wdi_ind$indicator, selected = 'Population, total'),
      sliderInput(inputId = "year", label = "Select start and end years:", min=1960, max=2020, value=c(2010,2015), sep ='', ticks = F ),
      actionButton(inputId = "update", label = "Fetch Data"),
      downloadLink("downloadData", " Download")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("IDB MC Data", 
                 htmlOutput("header"), 
                 tableOutput("table")),
        tabPanel("Aggregates", 
                 htmlOutput("header2"),
                 tableOutput("aggregates")),
        tabPanel("Plot", 
                 
                 plotOutput("plot"))
        
    )#tabset
  )#mainpanel
  ) #sidebar
  
) 

shinyApp(ui=ui, server=server)
