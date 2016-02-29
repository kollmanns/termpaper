## foursquare.R ##
foursquareUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(title = "Most Popular Categories",
          width = 6,
          plotOutput(ns("four_PopularCatPlot"), height = 200)
      ),
      box(title = "Number of Checkins",
          width = 6,
          plotOutput(ns("four_NumberCheckPlot"), height = 200)
      )
    ),
    fluidRow(
      box(title = "Most Popular Venues",
          width = 6,
          DT::dataTableOutput(ns("four_PopularVenTable"))
          #tableOutput(ns("four_PopularVenTable"))
      ),
      box(title = "Most Popular Spot",
          width = 6,
          leafletOutput(ns("four_PopularSpot"))
      )
    ),
    uiOutput(ns("four_Content"))
  )
  
}

foursquare <- function(input, output, session,
                       fCheckinsDf, fVenuesDf, four_PopularCategory) {
  ns <- session$ns
  
  output$four_PopularCatPlot <- renderPlot({
    plotData <- as.matrix(four_PopularCategory %>% select(Category, count) %>% head(10))
    plotData[, "Category"] <- encodeCorrect(plotData[, "Category"])
    
    par(mar = c(5, 0.1, 0.1, 0.1))
    # http://paletton.com/#uid=7050u0knKrKelDvjvuOrCnwuSiH
    # color: blue = 6, 76, 92, darkred = 149, 22, 5, green = 4, 111, 32
    xx <- barplot(as.numeric(plotData[, "count"]), axes = FALSE, col = rgb(6, 76, 92, maxColorValue=255),
                  border = NA)
    ## Add text at top of bars
    
    text(x = xx, y = as.numeric(plotData[, "count"]), label = as.numeric(plotData[, "count"]),
         pos = 1, cex = 0.9, col = "white")
    ## Add x-axis labels 
    axis(1, at=xx, labels=plotData[, "Category"], tick=FALSE, las=2, line=-0.5, cex.axis=0.9)
  })
  
  output$four_NumberCheckPlot <- renderPlot({
    par(mar = c(5, 0.1, 0.1, 0.1))
    plotData <- fCheckinsDf %>% arrange(Time) %>% select(Time)
    plotData$Number <- c(1:nrow(fCheckinsDf))
    
    ggplot(data = plotData, aes(Time, Number)) +
      geom_line(size=1 , colour=rgb(149, 22, 5, maxColorValue=255)) +
      scale_x_datetime(breaks = date_breaks("1 year")) +
      ylab("") +  xlab("") +
      theme(panel.grid.major = element_line(colour = "lightgrey"),
            panel.grid.minor = element_line(colour = "lightgrey"),
            panel.background = element_rect(fill = "white")
      )
  })
  
#   output$four_PopularVenTable <- renderTable({
#     topVenues <- as.matrix(select(head(fVenuesDf, 10), Name, Count))
#     topVenues[, "Name"] <- encodeCorrect(topVenues[, "Name"])
#     topVenues
#   }, include.rownames = FALSE)
  topVenues <- as.matrix(select(head(fVenuesDf, 10), Name, Count))
  topVenues[, "Name"] <- encodeCorrect(topVenues[, "Name"])
  
  output$four_PopularVenTable <- DT::renderDataTable(
    DT::datatable(topVenues,
                  options = list(paging = FALSE,
                                 bSort = FALSE,         # disable sorting
                                 bLengthChange = FALSE, # show/hide records per page dropdown
                                 bFilter = FALSE,       # global search box on/off
                                 bInfo = FALSE          # information on/off (records filtered, etc)
                  )
    )
  )
  
  # https://rstudio.github.io/leaflet/shiny.html
  output$four_PopularSpot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = fVenuesDf$Latitude[1], lng = fVenuesDf$Longitude[1], zoom = 14) %>%
      addPopups(lat = fVenuesDf$Latitude[1], lng = fVenuesDf$Longitude[1],
                paste0(icon("university"), " <b>", fVenuesDf$Name[1], "</b>", br(),
                       "Number of Checkins: ", fVenuesDf$Count[1]),
                options = popupOptions(closeButton = FALSE))
  })
  
  
  output$four_Content <- renderUI({
    div(
      p("There are a total number of ", nrow(fCheckinsDf),
        "checkins from this user within ", nrow(four_PopularCategory), " different categories.",
        br(), "The most popular venue accounts for ",
        sprintf("%.2f%%", fVenuesDf$Count[1]/nrow(fCheckinsDf)*100), " percent of all checkins.")
    )
  })
}