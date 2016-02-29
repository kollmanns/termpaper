## twitter.R ##
twitterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(
      width = "100%",
      column(width = 6,
             column(width = 6,
                    sliderInput(ns("twitterTagcloudFreq"), label = "Minimum Frequency",
                                value=2, min=1, max=15),
                    uiOutput(ns("userInformation"))
             ),
             column(width = 6,
                    sliderInput(ns("twitterTagcloudMax"), label = "Maximum Number of Words",
                                value=100, min=1, max=200),
                    uiOutput(ns("averageStatistics"))
             )
      ),
      column(width = 6,
             tags$label("Most frequently used Words"),
             plotOutput(ns("twitterTagcloudPlot"), width = "100%")
      )
    ),
    fluidRow(
      column(width = 3, uiOutput(ns("twitterColumnOne"))),
      column(width = 3, uiOutput(ns("twitterColumnTwo"))),
      column(width = 3, uiOutput(ns("twitterColumnThree"))),
      column(width = 3, uiOutput(ns("twitterColumnFour")))
    )
  )
  
}

twitter <- function(input, output, session, twitterDf, twitterTagcloudV, twF, user) {
  ns <- session$ns
  
  output$twitterTagcloudPlot <- renderPlot({
    # http://shiny.rstudio.com/gallery/word-cloud.html
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    wordcloud(names(twitterTagcloudV), twitterTagcloudV, scale=c(3, 0.5), rot.per = 0,
              min.freq = input$twitterTagcloudFreq, max.words=input$twitterTagcloudMax,
              colors=brewer.pal(8, "Dark2"))
  })
  
  renderUserInformation <- function(profileimg, name, screenname) {
    span(
      style = paste0("background-image: url('", profileimg, "');",
                     "background-size: 48px 48px; background-repeat: no-repeat;",
                     "padding-left: 58px; display: table-cell; vertical-align: middle; height: 48px;"),
      tags$b(encodeCorrect(name)), br(),
      tags$a(href=paste0("http://twitter.com/", screenname),
             paste0("@", screenname))
    )
  }
  
  output$userInformation <- renderUI({
    tagList(
      tags$label("User Information"),
      renderUserInformation(user$User_ProfileImage[1], user$User[1], user$User_Screenname[1]),
      tags$ul(
        class = "fa-ul",
        tags$li(encodeCorrect(user$User_Description[1])),
        tags$li(icon("birthday-cake", class = "fa-li"), "Using Twitter since ",
                format(user$User_CreatedAt[1], format="%Y-%m-%d")),
        tags$li(icon("male", class = "fa-li"), tags$b("Followers: "),
                format(user$User_Followers[1], big.mark = " ")), 
        tags$li(icon("binoculars", class = "fa-li"), tags$b("Friends: "),
                format(user$User_Friends[1], big.mark = " ")), 
        tags$li(icon("twitter", class = "fa-li"), tags$b("Tweets: "),
                format(nrow(twitterDf), big.mark = " "))
      )
    )
    
  })
  
  output$averageStatistics <- renderUI({
    tagList(
      tags$label("Statistics"),
      tags$ul(
        class = "fa-ul",
        # remove biggest and smallest 10% (outliers) 
        # trim = 0.5 would be the same as median
        tags$li(icon("male", class = "fa-li"), tags$b("Followers AVG: "), br(),
                span(style="padding-left: 25px;",
                     format(round(mean(twF$Followers, trim = 0.1)), big.mark = " "))),
        tags$li(tags$b("Followers MAX: "), br(),
                span(style="padding-left: 25px;",
                     twF$Name[twF$Followers == max(twF$Followers)],
                     " (", format(max(twF$Followers), big.mark = " "),")")),
        tags$li(icon("binoculars", class = "fa-li"), tags$b("Friends AVG: "), br(),
                span(style="padding-left: 25px;",
                     format(round(mean(twF$Friends, trim = 0.1)), big.mark = " "))),
        tags$li(tags$b("Friends MAX: "), br(),
                span(style="padding-left: 25px;",
                     twF$Name[twF$Friends == max(twF$Friends)],
                     " (", format(max(twF$Friends), big.mark = " "),")")),
        tags$li(icon("twitter", class = "fa-li"), tags$b("Tweets AVG: "), br(),
                span(style="padding-left: 25px;",
                     format(round(mean(twF$NumberofTweets, trim = 0.1)), big.mark = " "))),
        tags$li(tags$b("Tweets MAX: "), br(),
                span(style="padding-left: 25px;",
                     twF$Name[twF$NumberofTweets == max(twF$NumberofTweets)],
                     " (", format(max(twF$NumberofTweets), big.mark = " "),")"))

        
      )
    )
    
  })
  
  output$twitterColumnOne <- renderUI({
    twOutputList <- lapply(seq(1, nrow(twF), 4), function(i) {
      boxName <- ns(paste0("twBox", i))
      htmlOutput(boxName)
    })
    
    do.call(tagList, twOutputList)
  })
  
  output$twitterColumnTwo <- renderUI({
    twOutputList <- lapply(seq(2, nrow(twF), 4), function(i) {
      boxName <- ns(paste0("twBox", i))
      htmlOutput(boxName)
    })
    
    do.call(tagList, twOutputList)
  })
  
  output$twitterColumnThree <- renderUI({
    twOutputList <- lapply(seq(3, nrow(twF), 4), function(i) {
      boxName <- ns(paste0("twBox", i))
      htmlOutput(boxName)
    })
    
    do.call(tagList, twOutputList)
  })
  
  output$twitterColumnFour <- renderUI({
    twOutputList <- lapply(seq(4, nrow(twF), 4), function(i) {
      boxName <- ns(paste0("twBox", i))
      htmlOutput(boxName)
    })
    
    do.call(tagList, twOutputList)
  })
  
  for (i in 1:nrow(twF)) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      boxName <- paste("twBox", my_i, sep="")
      
      output[[boxName]] <- renderUI({
        textCol <- as.character(twF$Profile_TextColor[my_i])
        backCol <- as.character(twF$Profile_BackgroundColor[my_i])
        if (textCol == backCol) {
          textCol <- "333333"
          backCol <- "C0DEED"
        }
        
        box(
          style = paste0("background-color: #", backCol, ";", "color: #", textCol, ";"),
          #       [1]    "Description"            
          #       [5]    "CreatedAt"         
          #       [9] "RecentStatus"   "RecentStatus_CreatedAt"  "RecentStatus_Retweets"   "RecentStatus_Favorites" 
          #       [13]  "Profile_BackgroundImage" ""
          
          title = renderUserInformation(twF$Profile_Image[my_i], twF$Name[my_i], twF$Screenname[my_i]),
          footer = encodeCorrect(twF$Location[my_i]),
          width = NULL,
          encodeCorrect(twF$Description[my_i]),
          #span(sub('@([A-Za-z0-9_]+)', "<a href='http://twitter.com/\\1'>@\\1</a>", encodeCorrect(twF$Description[my_i]), perl=TRUE)),
          div(
            style = "display: flex; justify-content: space-between;",
            span(icon("male"), format(twF$Followers[my_i], big.mark = " ")),
            span(icon("binoculars"), format(twF$Friends[my_i], big.mark = " ")),
            span(icon("twitter"), format(twF$NumberofTweets[my_i], big.mark = " "))
          )
        )
      })
      
    })
  }
  
  
}