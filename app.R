library(shiny)
library(DT)
library(shinyWidgets)
library(httr)
library(leaflet)
library(plotly)
library(maps)
library(dplyr)
library(lubridate)
library(shinythemes)
library(owmr)
library(rintrojs)

cities = as.data.frame(force(world.cities))
list_cities = as.list(cities[128:nrow(cities),1])
names(list_cities) = cities[128:nrow(cities),1]  
ui <- fluidPage(
    titlePanel("",
      windowTitle = "Plan Your Expedition!"),
    h1(
      HTML('<style>
  <link href="http://allfont.net/allfont.css?fonts=agency-fb" rel="stylesheet" type="text/css" />
/* Style the body */
body {
 		font-family: arial;
        font-size: 50px;
}

/* Header/Logo Title */
.header {
 font-family: "Agency FB", arial;
        font-size: 65px;
}

/* Page Content */
.content {padding:20px;}
</style>
<div class="header">
  <p><i class="fas fa-leaf fa-flip-horizontal"></i><span style="color: rgb(112, 173, 71);">
  <strong>P</strong></span><strong>lan <span style="color: rgb(112, 173, 71);">Y</span>our 
<span style="color: rgb(112, 173, 71);">E</span>xpedition!</strong></span></p></p>
'), align = "center"),
    h4("A tool for planning place and time of biological field works\n
    based on iNaturalist observations and weather forecast
",align = "center"),
    hr(),
    navbarPage(
    br(),

      tabPanel("Nature observations",
   column(3, wellPanel(
     introjsUI(),
     tags$head(
       tags$style(HTML('#help{background-color:orange}'))
     ),
     tags$head(
       tags$style(HTML('#submit1{background-color:#70ad47}'))
     ),
     introBox(textAreaInput("caption", "Write a species or a place", "Accipiter striatus", width = "200px", 
                      height = "40px"),data.step = 1,data.intro = "Write a scientific/common name of species, or place name"),
        fluidRow(column(width = 10,
                        chooseSliderSkin("Flat", color = '#006600'),
                        introBox(sliderInput('myKnob', "Choose number of observations:",
                                    min = 0, max = 50000, value = 200
                        ),data.step = 2,data.intro = "Slide for choosing number of observations. 
                        Press Go! and see here total number of results on the iNaturalist")),
        column(width = 5,offset = 1,
                  tags$head(
                    tags$style(HTML("#006600"))
                  ),
               introBox(actionButton("submit1", label = "Go!"),data.step = 3,
                        data.intro = "Click here"),
               introBox(downloadButton("downloadData", "Download", icon = icon("file-csv")), data.step = 5,
                        data.intro = "download the table as .csv file"),
               actionButton("help", "Virtual guide", icon = icon("question-circle")))
      ))),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table",
                 introBox(DT::dataTableOutput("table"), icon = icon("columns"),data.step = 4,
                          data.intro = "This is a table with results")
                 #downloadButton("downloadData", "Download"),
        ),
        tabPanel(title = introBox("Map", data.step = 6,
                 data.intro = "This is geografical map"), leafletOutput("mymap"), width="100%",height="1000px", 
                 icon = icon("map-marker-alt")),
        tabPanel(title = introBox("Seasons and species", data.step = 7, data.intro = 
                                    "This is grafical data description", data.hint = "Click here"), 
                 introBox(plotlyOutput(outputId = "plotly"),data.step = 8,
                          data.intro = "Number of observations per month. 
                          Double click to a legend on the right side and choose places of your interest"),
                 introBox(plotlyOutput(outputId = "plotly_sp"),data.step = 9,
                          data.intro = "This chart represents spicies composition on results"), icon = icon("chart-bar")))
      )),
     tabPanel(title = introBox("Wheather forecast", data.step = 10, data.intro = "This is weather forecast for 6 days. 
                               Click here" ),
              tabsetPanel(
                tabPanel(title = introBox("Coordinates", data.step = 11, data.intro = ""),
                         column(4, wellPanel(textInput("latlot", "Write latitude and longitude",
                                                       value = "55.977575, 37.998665")),
                                actionButton("submit2", label = "Go!")),
                         mainPanel(
                           plotlyOutput(outputId = "plotly_w1"),
                           DT::dataTableOutput("data1")),
                         icon = icon("map")
                ),
                tabPanel("City", 
                         column(4, wellPanel(selectInput('cities', 'Choose a place', list_cities)), 
                                actionButton("submit3", label = "Go!")),
                         mainPanel(
                           plotlyOutput(outputId = "plotly_w2"),
                           DT::dataTableOutput("data2")), 
                         icon = icon("map"))
              )),
    tabPanel("About app", "https://github.com/capsellabursa/PlanYourExpedition/")
    ))


server <- function(input, output,session){
  
    species = eventReactive(input$submit1,{
      search <-  paste("&q=",gsub(" ","+",input$caption),sep="")
      base_url <- "http://www.inaturalist.org/"
      q_path <- "observations.csv"
      page_query <- paste(search,"&per_page=200&page=1",sep="")
      data <-  GET(base_url, path = q_path, query = page_query)
      entries = data$headers$`x-total-entries`
      entries
      })
    observeEvent(species(), {
      updateSliderInput(
        session = session,
        inputId = "myKnob",
        value = species()
      )
    }, ignoreInit = TRUE)
    output$text <-reactive({species()})
    output$res <- renderPrint(input$myKnob)

    
    ##table
    
    data_inat = reactive({
      search <-  paste("&q=",gsub(" ","+",input$caption),sep="")
      base_url <- "http://www.inaturalist.org/"
      q_path <- "observations.csv"
      page_query <- paste(search,"&per_page=200&page=1",sep="")
      data <-  GET(base_url, path = q_path, query = page_query)
      data <-  content(data, as = "text")
      data_out <- if(is.na(data)) NA else read.csv(textConnection(data), stringsAsFactors = FALSE)
      
      results = input$myKnob
      for(i in 2:ceiling(results/200)){
        page_query <- paste(search,"&per_page=200&page=",i,sep="")
        data <-  GET(base_url,path = q_path, query = page_query)
        data <- content(data, as = "text")
        data_out <- rbind(data_out, read.csv(textConnection(data), stringsAsFactors = FALSE))
        data_output = data_out[, c(1,8,4,5,6,20)]
      }
      data_nat = as.data.frame(data_output)
      data_nat$longitude[is.na(data_nat$longitude)] = 0
      data_nat$latitude[is.na(data_nat$latitude)] = 0
      data_nat$country = map.where(database="world", data_nat$longitude,data_nat$latitude)
      data_nat
    })
    output$table <- DT::renderDataTable(DT::datatable(data_inat(), filter = "top"))
    
    ##map
    
    data_geo = eventReactive(input$submit1,{cbind(data_inat()$longitude, data_inat()$latitude)})
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = data_geo())
      })
    
    ##best month
    
    month_dat = eventReactive(input$submit1,{

      months = month(as.POSIXlt(data_inat()$time_observed_at, format="%Y-%m-%d"))
      months[is.na(months)] = "unknown"
      country = data_inat()$country
      country[is.na(country)] = "unknown place"
      plotly_data = data.frame(months, country)
      
      by_month <- as.data.frame(plotly_data %>% 
                                  group_by(months, country)%>% 
                                  summarise(
                                    sum = n()))
      month_name = data.frame(months = c(1:12), month = c("January","February", "March","April",   
                                                          "May","June","July","August","September",
                                                          "October","November","December"))
      
      merged_m = merge(month_name, by_month, by = "months", all = T)
      merged_m$sum[is.na(merged_m$sum)] = 0
      merged_m$month = factor(merged_m$month,
                              levels = month_name$month)
      merged_m
    })
    
    output$plotly =  renderPlotly({
      fig <- plot_ly(month_dat(), x = ~month, y = ~sum, type = 'bar', name = ~country)
      fig <- fig %>% add_trace(y = ~month)
      fig <- fig %>% layout(xaxis = list(title = "<b>Month</b>"), 
                            yaxis = list(title = "<b>Number of observations</b>"), barmode = 'stack',
                            title= "<b>Observations per month</b>")
      fig
    })
    ##species 
    by_species =eventReactive(input$submit1,{as.data.frame(data_inat() %>% 
                                  group_by(scientific_name)%>% 
                                  summarise(
                                    sum = n()))})
    output$plotly_sp =  renderPlotly({
        plot_ly(by_species(), x = ~scientific_name, y = ~sum, type = 'bar', name = ~scientific_name)%>% 
        layout(xaxis = list(title = "<b>species</b>"), 
              yaxis = list(title = "<b>Number of observations</b>"), barmode = 'stack',
              title= "<b>Species composition</b>")
      })
    ##Download csv
    
    output$filtered_row = renderPrint({input[["dt_rows_all"]]})
    
    output$downloadData = downloadHandler(
        filename = paste(input$caption, ".csv", sep = ""),
        content = function(file){
          write.csv(data_inat()[input[["dt_rows_all"]], ],
                    file)
        }
      )
    
    ##Weather forcast
    weather_latlot = eventReactive(input$submit2, {
      OWM_API_KEY = "30dd068c2a8874f3d3b22c6fce7b3347"
      owmr_settings(OWM_API_KEY)
      latlot = as.numeric(unlist(strsplit(as.character(input$latlot), ",")))
      res <- find_cities_by_geo_point(
        lat = latlot[1],
        lon = latlot[2],
        cnt = 1,
        units = "metric"
      )
      forecast = get_forecast(
        lat = latlot[1],
        lon = latlot[2],
        units = "metric"
      )%>%owmr_as_tibble()
      forecast[is.na(forecast)] = "no value"
      forecast$place = res$list$name
      idx = c("place","dt_txt", "temp", "humidity", "weather_description", "wind_speed","pressure")
      forecast = data.frame(forecast[, idx])
      colnames(forecast) = c("place", "date", "t('C)", "humidity (%)", "weather", "wind (m/s)","pressure")
      forecast
    })
    
    weather_city = eventReactive(input$submit3, {
      OWM_API_KEY = "30dd068c2a8874f3d3b22c6fce7b3347"
      owmr_settings(OWM_API_KEY)
      forecast <- get_forecast(input$cities, units = "metric")%>%owmr_as_tibble()
      forecast[is.na(forecast)] = "no value"
      idx = c("dt_txt", "temp", "humidity", "weather_description", "wind_speed","pressure")
      forecast = data.frame(forecast[, idx])
      colnames(forecast) = c("date", "t('C)", "humidity (%)", "weather", "wind (m/s)","pressure")
      forecast
    })
    output$plotly_w1 =  renderPlotly({
      fig <- weather_latlot()[, c("date", "t('C)", "humidity (%)", "wind (m/s)","pressure")]
      fig <- fig %>% tidyr::gather(variable, value, -date)
      fig <- fig %>% transform(id = as.integer(factor(variable)))
      fig <- fig %>% plot_ly(x = ~date, y = ~value, color = ~variable,
                             yaxis = ~paste0("y", id))
      fig <- fig %>% add_lines()
      fig <- fig %>% subplot(nrows = 4, shareX = TRUE)
      
      fig})
    
    output$plotly_w2 =  renderPlotly({
      fig <- weather_city()[, c("date", "t('C)", "humidity (%)", "wind (m/s)","pressure")]
      fig <- fig %>% tidyr::gather(variable, value, -date)
      fig <- fig %>% transform(id = as.integer(factor(variable)))
      fig <- fig %>% plot_ly(x = ~date, y = ~value, color = ~variable,
                             yaxis = ~paste0("y", id))
      fig <- fig %>% add_lines()
      fig <- fig %>% subplot(nrows = 4, shareX = TRUE)
      
      fig})
    
    output$data1 <- DT::renderDataTable(DT::datatable(weather_latlot(), filter = "top"))
    output$data2 <- DT::renderDataTable(DT::datatable(weather_city(), filter = "top"))
    #output$data1 <- renderTable({weather_latlot()})
    #output$data2 <- renderTable({weather_city()})
    observeEvent(input$help,introjs(session, 
                                    options = list(onbeforechange = readCallback("switchTabs"))))
    
}


shinyApp(ui, server)
