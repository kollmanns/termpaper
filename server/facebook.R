## facebook.R ##


facebookUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             box(title = "Most Popular Categories",
                 width = NULL,
                 plotOutput(ns("face_LikesCategory"), height = 300)
             ),
             column(width = 6,
                    uiOutput(ns("facebookColumnOne"))),
             column(width = 6,
                    uiOutput(ns("facebookColumnTwo")))
      ),
      column(width = 3,
             uiOutput(ns("facebookColumnThree"))),
      column(width = 3,
             uiOutput(ns("facebookColumnFour")))
    )
  )
  
}

facebook <- function(input, output, session, data, popularLikesCategory) {
  ns <- session$ns
  
  output$facebookColumnOne <- renderUI({
    fbCntList <- lapply(seq(1, nrow(data), 4), function(i) {
      boxName <- ns(paste0("fbCntBox", i))
      htmlOutput(boxName)
    })
    
    do.call(tagList, fbCntList)
  })
  
  output$facebookColumnTwo <- renderUI({
    fbCntList <- lapply(seq(2, nrow(data), 4), function(i) {
      boxName <- ns(paste0("fbCntBox", i))
      htmlOutput(boxName)
    })
    
    do.call(tagList, fbCntList)
  })
  
  output$facebookColumnThree <- renderUI({
    fbCntList <- lapply(seq(3, nrow(data), 4), function(i) {
      boxName <- ns(paste0("fbCntBox", i))
      htmlOutput(boxName)
    })
    
    do.call(tagList, fbCntList)
  })
  
  output$facebookColumnFour <- renderUI({
    fbCntList <- lapply(seq(4, nrow(data), 4), function(i) {
      boxName <- ns(paste0("fbCntBox", i))
      htmlOutput(boxName)
    })
    
    do.call(tagList, fbCntList)
  })
  
  for (i in 1:nrow(data)) {
    local({
      my_i <- i
      boxName <- paste0("fbCntBox", my_i)
      
      output[[boxName]] <- renderUI({
        box(
          footer = data$Category[my_i],
          title = encodeCorrect(data$Name[my_i]),
          width = NULL,
          if (!is.na(data$About[my_i]))
            tags$b(encodeCorrect(data$About[my_i])), br(),
          if (!is.na(data$Description[my_i]))
            encodeCorrect(data$Description[my_i]), br(),
          div(
            style = "display: flex; justify-content: space-between;",
            tags$a(href=data$FacebookURL[my_i], icon("facebook-square")),
            tags$a(href=data$Website[my_i], icon("external-link")),
            span(icon("thumbs-up"), format(data$Likes[my_i], big.mark = " "))
          )
        )
      })
      
    })
  }
  
  output$face_LikesCategory <- renderPlot({
    plotData <- popularLikesCategory %>% select(Category, count) %>% head(15)
    par(mar = c(10, 0.1, 0.1, 0.1))
    # http://paletton.com/#uid=7050u0knKrKelDvjvuOrCnwuSiH
    # color: blue = 6, 76, 92, darkred = 149, 22, 5, green = 4, 111, 32
    xx <- barplot(plotData$count, axes = FALSE, col = rgb(6, 76, 92, maxColorValue=255),
                  border = NA)
    ## Add text at top of bars
    
    text(x = xx, y = plotData$count, label = plotData$count, pos = 1, cex = 0.9, col = "white")
    ## Add x-axis labels 
    axis(1, at=xx, labels=plotData$Category, tick=FALSE, las=2, line=-0.5, cex.axis=0.8)
  })
  
}

# ########### my Code UI
# fluidRow(
#   column(width = 6,
#          box(title = "Most Popular Categories",
#              width = NULL,
#              plotOutput("face_LikesCategory", height = 300)
#          ),
#          column(width = 6,
#                 uiOutput("facebookColumnOne")),
#          column(width = 6,
#                 uiOutput("facebookColumnTwo"))
#   ),
#   column(width = 3,
#          uiOutput("facebookColumnThree")),
#   column(width = 3,
#          uiOutput("facebookColumnFour"))
# )
# 
# ########### my Code Server
# 
# 
# output$facebookColumnOne <- renderUI({
#   fbCntList <- lapply(seq(1, nrow(facebookDf), 4), function(i) {
#     boxName <- paste0("fbCntBox", i)
#     htmlOutput(boxName)
#   })
#   
#   do.call(tagList, fbCntList)
# })
# 
# output$facebookColumnTwo <- renderUI({
#   fbCntList <- lapply(seq(2, nrow(facebookDf), 4), function(i) {
#     boxName <- paste0("fbCntBox", i)
#     htmlOutput(boxName)
#   })
#   
#   do.call(tagList, fbCntList)
# })
# 
# output$facebookColumnThree <- renderUI({
#   fbCntList <- lapply(seq(3, nrow(facebookDf), 4), function(i) {
#     boxName <- paste0("fbCntBox", i)
#     htmlOutput(boxName)
#   })
#   
#   do.call(tagList, fbCntList)
# })
# 
# output$facebookColumnFour <- renderUI({
#   fbCntList <- lapply(seq(4, nrow(facebookDf), 4), function(i) {
#     boxName <- paste0("fbCntBox", i)
#     htmlOutput(boxName)
#   })
#   
#   do.call(tagList, fbCntList)
# })
# 
# for (i in 1:nrow(facebookDf)) {
#   local({
#     my_i <- i
#     boxName <- paste0("fbCntBox", my_i)
#     
#     output[[boxName]] <- renderUI({
#       box(
#         footer = facebookDf$Category[my_i],
#         title = encodeCorrect(facebookDf$Name[my_i]),
#         width = NULL,
#         if (!is.na(facebookDf$About[my_i]))
#           tags$b(encodeCorrect(facebookDf$About[my_i])), br(),
#         if (!is.na(facebookDf$Description[my_i]))
#           encodeCorrect(facebookDf$Description[my_i]), br(),
#         div(
#           style = "display: flex; justify-content: space-between;",
#           tags$a(href=facebookDf$FacebookURL[my_i], icon("facebook-square")),
#           tags$a(href=facebookDf$Website[my_i], icon("external-link")),
#           span(icon("thumbs-up"), format(facebookDf$Likes[my_i], big.mark = " "))
#         )
#       )
#     })
#     
#   })
# }
# 
# output$face_LikesCategory <- renderPlot({
#   plotData <- face_LikesCategory %>% select(Category, count) %>% head(15)
#   par(mar = c(10, 0.1, 0.1, 0.1))
#   # http://paletton.com/#uid=7050u0knKrKelDvjvuOrCnwuSiH
#   # color: blue = 6, 76, 92, darkred = 149, 22, 5, green = 4, 111, 32
#   xx <- barplot(plotData$count, axes = FALSE, col = rgb(6, 76, 92, maxColorValue=255),
#                 border = NA)
#   ## Add text at top of bars
#   
#   text(x = xx, y = plotData$count, label = plotData$count, pos = 1, cex = 0.9, col = "white")
#   ## Add x-axis labels 
#   axis(1, at=xx, labels=plotData$Category, tick=FALSE, las=2, line=-0.5, cex.axis=0.8)
# })
