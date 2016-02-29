## instagram.R ##
instagramUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      width = "100%",
      column(width = 4,
             sliderInput(ns("instagramTagcloudFreq"), label = "Minimum Frequency",
                         value=5, min=1, max=20),
             sliderInput(ns("instagramTagcloudMax"), label = "Maximum Number of Words",
                         value=50, min=1, max=100)
      ),
      column(width = 8,
             plotOutput(ns("instagramTagcloudPlot"), height = 280, width = "100%")
      )
    ),
    box(width = "100%",
        title = "Number of Pictures",
        sliderInput(ns("numberSliderInstagram"), label = NULL,
                    value=6, min=3, max=nrow(instagramDf), step= 3),
        collapsible = TRUE, collapsed = TRUE),
    uiOutput(ns("instagramBoxes"))
  )
  
}

instagram <- function(input, output, session, instagramDf, instagramTagcloudV) {
  ns <- session$ns
  
  output$instagramTagcloudPlot <- renderPlot({
    # http://shiny.rstudio.com/gallery/word-cloud.html
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    wordcloud(names(instagramTagcloudV), instagramTagcloudV, scale=c(3, 0.2), rot.per = 0,
              min.freq = input$instagramTagcloudFreq, max.words=input$instagramTagcloudMax,
              colors=brewer.pal(8, "Dark2"))
  })
  
  # Insert the right number of plot output objects into the web page
  output$instagramBoxes <- renderUI({
    boxesOutputList <- lapply(seq(3, input$numberSliderInstagram, 3), function(i) {
      boxName <- ns(paste("igBox", i, sep=""))
      htmlOutput(boxName, width = 4)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, boxesOutputList)
  })
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in seq(3, nrow(instagramDf), 3)) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      boxName <- paste("igBox", my_i, sep="")
      
      output[[boxName]] <- renderUI({
        fluidRow(
          column(4,
                 box(
                   footer = encodeCorrect(instagramDf$caption[my_i]),
                   title = toString(instagramDf$created_time[my_i]),
                   width = NULL,
                   img(src = instagramDf$image[my_i], width="100%")
                 )
          ),
          column(4,
                 box(
                   footer = encodeCorrect(instagramDf$caption[my_i+1]),
                   title = toString(instagramDf$created_time[my_i+1]),
                   width = NULL,
                   img(src = instagramDf$image[my_i+1], width="100%")
                 )
          ),
          column(4,
                 box(
                   footer = encodeCorrect(instagramDf$caption[my_i+2]),
                   title = toString(instagramDf$created_time[my_i+2]),
                   width = NULL,
                   img(src = instagramDf$image[my_i+2], width="100%")
                 )
          )
        )
      })
      
    })
  }
  
  
}