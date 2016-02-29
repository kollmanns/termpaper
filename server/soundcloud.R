## soundcloud.R ##
soundcloudUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("soundcloudDescription")),
    uiOutput(ns("soundcloudContent"))
  )
  
}

soundcloud <- function(input, output, session, soundcloudDf) {
  ns <- session$ns
  
  getSoundcloudBox <- function(my_i) {
    box(
      title = encodeCorrect(soundcloudDf$Name[my_i]),
      width = 3,
      # 1.000.000 Followers = Black
      #   500.000 Followers = Navy
      #   100.000 Followers = Maroon
      #    50.000 Followers = Red
      #    10.000 Followers = Orange
      #         0 Followers = Yellow
      background = ifelse(soundcloudDf$Followers[my_i] >= 1000000, "black",
                          ifelse(soundcloudDf$Followers[my_i] >= 500000, "navy",
                                 ifelse(soundcloudDf$Followers[my_i] >= 100000, "maroon",
                                        ifelse(soundcloudDf$Followers[my_i] >= 50000, "red",
                                               ifelse(soundcloudDf$Followers[my_i] >= 10000, "orange",
                                                      "yellow"))))),
      tags$a(href=soundcloudDf$URL[my_i], img(src = soundcloudDf$Avatar[my_i], width="100%", class = "img-rounded")),
      div(
        style = "display: flex; justify-content: space-between;",
        span(icon("male"), format(soundcloudDf$Followers[my_i], big.mark = " "), "  "),
        span(icon("music"), format(soundcloudDf$Tracks[my_i], big.mark = " "))
      )
    )
  }
  
  output$soundcloudDescription <- renderUI(
    div("The number behind ", icon("male"), " indicates the amount of people who are ",
        "following this artist, ", br(), "whereas ", icon("music"), " shows the number of tracks posted.", br(),
        "Furthermore, the background color of the boxes show in which range the amount of follwers is:", br(),
        tags$ul(
          tags$li("less than 9.999 Followers = ", span(style="color: #f39c12", "Yellow")), 
          tags$li("10.000 - 49.999 Followers = ", span(style="color: #ff851b", "Orange")), 
          tags$li("50.000 - 99.999 Followers = ", span(style="color: #dd4b39", "Red")), 
          tags$li("100.000 - 499.999 Followers = ", span(style="color: #d81b60", "Maroon")), 
          tags$li("500.000 - 999.999 Followers = ", span(style="color: #001f3f", "Navy")), 
          tags$li("more than 1.000.000 Followers = ", span(style="color: #111111", "Black"))
        )
    )
  )
  
  output$soundcloudContent <- renderUI({
    
    scOutputList <- lapply(seq(1, nrow(soundcloudDf), 4), function(i) {
      boxName <- ns(paste("scBox", i, sep=""))
      htmlOutput(boxName)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, scOutputList)
    
  })
  
  for (i in seq(1, nrow(soundcloudDf), 4)) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      boxName <- paste("scBox", my_i, sep="")
      
      # my_i <- 4
      output[[boxName]] <- renderUI({
        fluidRow(
          getSoundcloudBox(my_i),
          getSoundcloudBox(my_i+1),
          getSoundcloudBox(my_i+2),
          getSoundcloudBox(my_i+3)
        )
      })
      
    })
  }
  
}