## dashboard.R ##
dashboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("dashboardHeader")),
    valueBoxOutput(ns("currentAmount")),
    valueBoxOutput(ns("salary")),
    valueBoxOutput(ns("lastSalary")),
    fluidRow(
      column(width = 5,
             uiOutput(ns("basicUserProfile"))
      ),
      column(width = 7,
             box(
               width = NULL,
               title = "Timeline (last 45 Days)",
               uiOutput(ns("timeline"))
             )
             
      )
    )
  )
  
}

dashboard <- function(input, output, session, mongo) {
  ns <- session$ns
  
  output$dashboardHeader <- renderUI({
    h2(generalData$Firstname, " ", generalData$Lastname)
  })
  
  output$currentAmount <- renderValueBox({
    sumAmount <- transactionsDf %>% summarize(a = sum(Amount))
    
    valueBox(
      format(sumAmount, digits=2, nsmall=2, big.mark=" "), "Current Amount",
      icon = icon("euro"),
      color = ifelse(sumAmount >= 0, "green", "red")
    )
  })
  
  output$salary <- renderValueBox({
    avgSalary <- transactionsDf %>%
      filter(grepl("gehalt|lohn|salary", tolower(Text))) %>%
      summarize(a = mean(Amount))
      
    valueBox(
      paste0(format(avgSalary, digits=2, nsmall=2, big.mark=" "), encodeCorrect('€')), "Average Salary",
      icon = icon("money"),
      color = "yellow"
    )
  })
  
  output$lastSalary <- renderValueBox({
    lastSalary <- transactionsDf %>%
      filter(grepl("gehalt|lohn|salary", tolower(Text))) %>%
      arrange(desc(Date)) %>% head(1) %>% select(Amount)
    
    valueBox(
      paste0(format(lastSalary, digits=2, nsmall=2, big.mark=" "), encodeCorrect('€')), "Current Salary",
      icon = icon("money")
      # color = "yellow"
    )
  })
  
  output$basicUserProfile <- renderUI({
    currentPosition <- linkedIn_Positions %>% filter(PositionIsCurrent == TRUE)
    
    div(
      class = "list-group",
      span(class="list-group-item",
           style = "background-color: #f9f9f9;",
           h4("Customer Details")
      ),
      span(class="list-group-item",
           icon(ifelse(generalData$Gender == "male", "mars", "venus"), class="fa-fw"),
           generalData$Gender),
      span(class="list-group-item", icon("birthday-cake", class="fa-fw"),
           style = "background-color: #f9f9f9;",
           generalData$Birthday),
      span(class="list-group-item", icon("map-marker", class="fa-fw"),
           a(href=paste0("http://maps.google.com/?q=", generalData$Address), generalData$Address)),
      span(class="list-group-item", icon("envelope", class="fa-fw"),
           style = "background-color: #f9f9f9;",
           a(href=paste0("mailto:", generalData$Mail), generalData$Mail)),
      span(class="list-group-item", icon("phone", class="fa-fw"),
           a(href=paste0("tel:", gsub(" ", "", generalData$Phone)), generalData$Phone)),
      span(class="list-group-item", icon("suitcase", class="fa-fw"),
           style = "background-color: #f9f9f9;",
           currentPosition$Position, " @ ", currentPosition$Company),
      span(class="list-group-item",
           tags$table(
             tags$tr(
               tags$td(
                 icon("credit-card", class="fa-fw")
               ),
               tags$td(
                 style = "padding-left:20px; text-align: right;",
                 tags$label("Type"), br(),
                 "Valid until", br(),
                 "Card No.", br(),
                 "IBAN", br(),
                 "BIC"
               ),
               tags$td(
                 style = "padding-left:20px;",
                 tags$label(products$name), br(),
                 format(products$validUntil, format = "%Y-%m"), br(),
                 # replace everything except first and last 4 chars with x
                 paste0(substr(products$Number, 1, 4), " ",
                        paste(rep("xxxx", nchar(substr(products$Number, 5, nchar(products$Number)-4))/4),
                              collapse = " "), " ",
                        substr(products$Number, nchar(products$Number)-3, nchar(products$Number))
                 ), br(),
                 products$IBAN, br(),
                 products$BIC
               )
             )
           )
      )
    )
  })
  
  getTimelineElement <- function(heading, body, icon="comment", class="") {
    tagList(
      div(class = paste0("timeline-badge ", class), icon(icon)),
      div(class="timeline-panel",
          div(class = "timeline-heading", h4(class = "timeline-title", heading)),
          div(class = "timeline-body", HTML(body))
      )
    )
  }
  
  output$timeline <- renderUI({
    ## gather data
    timelineList <- lapply(1:nrow(timeline), function(i) {
      elementName <- ns(paste0("tlElement", i))
      htmlOutput(elementName, container = tags$li)
    })
    
    ## output
    tags$ul(
      class = "timeline",
      do.call(tagList, timelineList)
    )
   
  })
  
  for (i in 1:nrow(timeline)) {
    local({
      my_i <- i
      elementName <- paste0("tlElement", my_i)
      
      output[[elementName]] <- renderUI({
        getTimelineElement(timeline$Date[my_i], timeline$Text[my_i],
                           timeline$Icon[my_i], timeline$Class[my_i]
        )
      })
    })
  }

}

