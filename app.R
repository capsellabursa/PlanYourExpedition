library(shiny)
library(DT)
library(shinyWidgets)
library(httr)
library(leaflet)
library(plotly)
  
ui <- fluidPage(
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
    h4("The tool for planning place and time of biological field works\n
    based on nature observations and weather forecast
",align = "center"),
    hr(),
    br(),

    #textAreaInput("caption", "Write a species", "Accipiter striatus", width = "500px"),
   column(4, wellPanel(
        textAreaInput("caption", "Write a species, date or place", "Accipiter striatus", width = "300px", 
                      height = "40px"),
        fluidRow(column(width = 8,knobInput(
          inputId = 'myKnob',
          label = "Number of observation:",
          min = 0,
          value = 100,
          max = 1000,
          displayPrevious = TRUE, 
          lineCap = "default",
          fgColor = "#70ad47",
          inputColor = "#006600"
        )),
        column(width = 5,offset = 4,
                  tags$head(
                    tags$style(HTML("#006600"))
                  ),
                  submitButton(text = "Apply Changes")),
        column(width = 6,downloadButton("downloadData", "Download", icon = icon("file-csv")))
      ))),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table",
                 DT::dataTableOutput("table"), icon = icon("columns")
                 #downloadButton("downloadData", "Download"),
        ),
        tabPanel("Map", leafletOutput("mymap"), width="100%",height="1000px", icon = icon("map-marker-alt")),
        tabPanel("Seasons", plotlyOutput(outputId = "plotly"), icon = icon("chart-bar")),
        tabPanel("Weather", "coming soon",icon = icon("cloud") ))
      )
    )


server <- function(input, output,session){
    species = reactive({
      search <-  paste("&q=",gsub(" ","+",input$caption),sep="")
      base_url <- "http://www.inaturalist.org/"
      q_path <- "observations.csv"
      page_query <- paste(search,"&per_page=200&page=1",sep="")
      data <-  GET(base_url, path = q_path, query = page_query)
      entries = data$headers$`x-total-entries`
      entries
    })
    output$text <-reactive({species()})
    output$res <- renderPrint(input$myKnob)
    
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
      data_nat
    })
    output$table <- DT::renderDataTable(DT::datatable(data_inat()), filter = "top", options = list(
      pageLength = 300, lengthMenu = c(100,200,300,400,500,600)
    ))
    
    
    data_geo = reactive({cbind(data_inat()$longitude, data_inat()$latitude)})
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = data_geo())
                   #)%>%
        #fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
      })
    
    #best month
    
    month_dat = reactive({
    months = rep(0, nrow(data_inat()))
    for (i in 1:nrow(data_inat())){
      months[i] = unlist(strsplit(data_inat()$observed_on[i], "-"))[2]
    }
    months[months == "01"] = "January"
    months[months == "02"] = "February"
    months[months == "03"] = "March"
    months[months == "04"] = "April"
    months[months == "05"] = "May"
    months[months == "06"] = "June"
    months[months == "07"] = "July"
    months[months == "08"] = "August"
    months[months == "09"] = "September"
    months[months == "10"] = "October"
    months[months == "11"] = "November"
    months[months == "12"] = "December"
    
    
    da = table(months)
    table_date = data.frame(da)
    months = c("January","February","March","April",
               "May","June","July","August", "September",
               "October","November","December")
    order = data.frame(order = as.numeric(c(1:12)), months)
    data_date = merge(order, table_date, by = "months", all = T)
    data_date$Freq[is.na(data_date$Freq)] = 0
    data_date
    })
    
    output$plotly =  renderPlotly({

    fig <- plot_ly(month_dat(), x = ~order, y = ~Freq,
                   name = "Observations per month",
                   type = "bar", marker = list(color = "#70ad47"))
    fig%>%layout(xaxis = list(title = "<b>Month</b>", 
                              ticktext = month_dat()$months,
                              tickvals = c(1:12)),
                 yaxis = list(title = "<b>Number of observations</b>"))
    })
    

    
    # Downloadable csv of selected dataset ----
    
    output$downloadData = downloadHandler(
      filename = function() {
        paste(input$caption, ".csv", sep = "")}, 
      content = function(file) {
        filtered_data <- data_inat()$tbl_rows_all
      write.csv(filtered_data, file,row.names = F)
    })
}


shinyApp(ui, server)
